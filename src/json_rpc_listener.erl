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

-moduledoc """
Owns the Cowboy listener and its graceful shutdown.

The listener itself is supervised by Ranch, not by this process. What this
`m:gen_server` adds is a lifetime to attach shutdown behaviour to: it traps
exits so the supervisor's shutdown arrives as a message and `terminate/2`
gets to drain before the socket goes away.

## Draining

On shutdown, in order:

1. Swap the routes for `m:json_rpc_drain_handler`, so anything arriving from
   now on gets `503` and stops being counted as work to wait for.
2. Ask every WebSocket connection to send a `1001 Going Away` close frame.
3. Poll Ranch until the in-flight connections are gone, capped at
   `drain_timeout_ms`.
4. Stop the listener.

This is best-effort by construction: a client that reconnects during the
drain window is still served a `503`. Removing the node from a load
balancer's rotation before `SIGTERM` is what actually makes a shutdown
invisible; this only keeps in-flight work from being cut off.

## HTTP/1.1 only

`protocols => [http]` pins the listener. Cowboy's `request_timeout` knob is
HTTP/1.1-only, so silently accepting an h2c upgrade would leave the
transport-level idle wait unset on exactly the connections that could hold
the most streams open.
""".

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

-define(LISTENER, ?MODULE).
-define(DRAIN_POLL_MS, 50).

-doc "Start the listener. Called by `m:json_rpc_sup`.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc false.
-spec init([]) -> {ok, map()} | {stop, term()}.
init([]) ->
    process_flag(trap_exit, true),
    case cowboy:start_clear(?LISTENER, transport_opts(), protocol_opts()) of
        {ok, _ListenerPid} ->
            ?LOG_INFO("json_rpc: listening on port ~p", [json_rpc_config:get(port)]),
            {ok, #{}};
        {error, Reason} ->
            {stop, Reason}
    end.

-doc false.
handle_call(Request, _From, State) ->
    ?LOG_WARNING("json_rpc_listener: unexpected call ~p", [Request]),
    {reply, {error, unknown_call}, State}.

-doc false.
handle_cast(_Msg, State) ->
    {noreply, State}.

-doc false.
handle_info(_Info, State) ->
    {noreply, State}.

-doc false.
terminate(_Reason, _State) ->
    DrainMs = json_rpc_config:get(drain_timeout_ms),
    _ = cowboy:set_env(?LISTENER, dispatch, drain_dispatch()),
    broadcast_drain(),
    wait_for_drain(erlang:monotonic_time(millisecond) + DrainMs),
    _ = cowboy:stop_listener(?LISTENER),
    ok.

-doc false.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal

transport_opts() ->
    #{
        socket_opts => [{port, json_rpc_config:get(port)}],
        num_acceptors => json_rpc_config:get(num_acceptors),
        max_connections => json_rpc_config:get(max_connections)
    }.

protocol_opts() ->
    #{
        env => #{dispatch => dispatch()},
        idle_timeout => json_rpc_config:get(idle_timeout_ms),
        request_timeout => json_rpc_config:get(request_timeout_ms),
        max_keepalive => json_rpc_config:get(max_keepalive_requests),
        protocols => [http]
    }.

dispatch() ->
    cowboy_router:compile([
        {'_', [
            {json_rpc_config:get(http_path), json_rpc_http_handler, #{}},
            {json_rpc_config:get(ws_path), json_rpc_ws_handler, #{}}
        ]}
    ]).

drain_dispatch() ->
    cowboy_router:compile([{'_', [{"/[...]", json_rpc_drain_handler, #{}}]}]).

wait_for_drain(Deadline) ->
    case ranch:procs(?LISTENER, connections) of
        [] ->
            ok;
        Pids ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    ?LOG_WARNING(
                        "json_rpc: drain deadline reached with ~p connection(s) still open",
                        [length(Pids)]
                    ),
                    ok;
                false ->
                    timer:sleep(?DRAIN_POLL_MS),
                    wait_for_drain(Deadline)
            end
    end.

%% Ask every WebSocket connection to close itself. Without this the drain
%% poll would just wait out the full deadline on idle-but-open sockets.
broadcast_drain() ->
    try pg:get_members(json_rpc, json_rpc_ws_connections) of
        Pids -> lists:foreach(fun(Pid) -> Pid ! json_rpc_drain end, Pids)
    catch
        %% The pg scope can already be down depending on shutdown order.
        %% Nothing to notify then; the poll below still bounds the wait.
        _Class:_Reason -> ok
    end.
