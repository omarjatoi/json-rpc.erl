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

-module(json_rpc_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(LISTENER, json_rpc_listener).

start(_StartType, _StartArgs) ->
    Port = application:get_env(json_rpc, port, 8080),
    MaxBody = application:get_env(json_rpc, max_body_bytes, 1048576),
    MaxConns = application:get_env(json_rpc, max_connections, 1024),
    NumAcceptors = application:get_env(json_rpc, num_acceptors, 100),
    IdleTimeout = application:get_env(json_rpc, idle_timeout_ms, 60000),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/rpc", json_rpc_http_handler, #{max_body_bytes => MaxBody}},
            {"/ws", json_rpc_ws_handler, []}
        ]}
    ]),

    case json_rpc_sup:start_link() of
        {ok, SupPid} ->
            TransportOpts = [
                {port, Port},
                {num_acceptors, NumAcceptors},
                {max_connections, MaxConns}
            ],
            ProtocolOpts = #{
                env => #{dispatch => Dispatch},
                idle_timeout => IdleTimeout,
                max_keepalive => infinity,
                request_timeout => 10000
            },
            case cowboy:start_clear(?LISTENER, TransportOpts, ProtocolOpts) of
                {ok, _ListenerPid} ->
                    {ok, SupPid};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    _ = cowboy:stop_listener(?LISTENER),
    ok.
