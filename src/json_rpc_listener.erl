% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(json_rpc_listener).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(LISTENER, json_rpc_listener).

%%% Public API

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% gen_server callbacks

-spec init([]) -> {ok, map()} | {stop, term()}.
init([]) ->
    process_flag(trap_exit, true),
    Port = json_rpc_config:get(port),
    MaxBody = json_rpc_config:get(max_body_bytes),
    MaxConns = json_rpc_config:get(max_connections),
    NumAcceptors = json_rpc_config:get(num_acceptors),
    IdleTimeout = json_rpc_config:get(idle_timeout_ms),
    RequestTimeout = json_rpc_config:get(request_timeout_ms),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/rpc", json_rpc_http_handler, #{max_body_bytes => MaxBody}},
            {"/ws", json_rpc_ws_handler, []}
        ]}
    ]),
    TransportOpts = #{
        socket_opts => [{port, Port}],
        num_acceptors => NumAcceptors,
        max_connections => MaxConns
    },
    ProtocolOpts = #{
        env => #{dispatch => Dispatch},
        idle_timeout => IdleTimeout,
        max_keepalive => infinity,
        request_timeout => RequestTimeout
    },
    case cowboy:start_clear(?LISTENER, TransportOpts, ProtocolOpts) of
        {ok, _ListenerPid} ->
            {ok, #{}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% Best-effort graceful shutdown: install a 503-replying dispatch so no new
%% work is accepted, poll Ranch until in-flight connections drain (capped at
%% `drain_timeout_ms'), then stop the listener. For stronger guarantees use
%% a load balancer that stops sending traffic before SIGTERM.
terminate(_Reason, _State) ->
    DrainMs = json_rpc_config:get(drain_timeout_ms),
    DrainDispatch = cowboy_router:compile([
        {'_', [{"/[...]", json_rpc_drain_handler, []}]}
    ]),
    _ = cowboy:set_env(?LISTENER, dispatch, DrainDispatch),
    Deadline = erlang:monotonic_time(millisecond) + DrainMs,
    wait_drain(Deadline),
    _ = cowboy:stop_listener(?LISTENER),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal

wait_drain(Deadline) ->
    case ranch:procs(?LISTENER, connections) of
        [] ->
            ok;
        _Pids ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true -> ok;
                false ->
                    timer:sleep(50),
                    wait_drain(Deadline)
            end
    end.
