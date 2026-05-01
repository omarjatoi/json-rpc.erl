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

-module(json_rpc_ws_handler).

-behaviour(cowboy_websocket).

-include_lib("kernel/include/logger.hrl").

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, State) ->
    %% Accept the upgrade regardless of what (if any) subprotocols the
    %% client offers. RFC 6455 lets the server simply not select one — by
    %% omitting the `sec-websocket-protocol' response header — and many
    %% browser/SDK clients routinely advertise a subprotocol as a hint.
    %% Refusing the upgrade for that broke interop without buying anything.
    Opts = #{
        max_frame_size => json_rpc_config:get(ws_max_frame_bytes),
        idle_timeout => json_rpc_config:get(ws_idle_timeout_ms),
        compress => false
    },
    {cowboy_websocket, Req, State, Opts}.

websocket_init(_State) ->
    %% Join the connection-wide drain group so the listener can broadcast a
    %% close request during shutdown. `pg' monitors members and removes
    %% them on exit, so no explicit cleanup in `terminate/3' is required.
    ok = pg:join(json_rpc, json_rpc_ws_connections, self()),
    {[], #{}}.

websocket_handle({text, Frame}, State) ->
    case decode_json(Frame) of
        {ok, Parsed} ->
            Timeout = json_rpc_config:get(handler_timeout_ms),
            case json_rpc_worker:run(Parsed, Timeout) of
                {ok, no_response} ->
                    {[], State};
                {ok, Reply} ->
                    {[{text, jiffy:encode(Reply)}], State};
                {error, timeout} ->
                    Id = json_rpc_dispatcher:call_id_for_error(Parsed),
                    ErrBody = jiffy:encode(
                        json_rpc_dispatcher:create_error_response(
                            Id, -32603, <<"Internal error">>, #{reason => timeout}
                        )
                    ),
                    {[{text, ErrBody}], State};
                {error, {crash, Class, Reason}} ->
                    ?LOG_ERROR("Handler crashed: ~p:~p", [Class, Reason]),
                    Id = json_rpc_dispatcher:call_id_for_error(Parsed),
                    ErrBody = jiffy:encode(
                        json_rpc_dispatcher:create_error_response(
                            Id, -32603, <<"Internal error">>
                        )
                    ),
                    {[{text, ErrBody}], State}
            end;
        {error, parse_error} ->
            ErrBody = jiffy:encode(
                json_rpc_dispatcher:create_error_response(null, -32700, <<"Parse error">>)
            ),
            {[{text, ErrBody}], State}
    end;
websocket_handle({binary, _Data}, State) ->
    %% JSON-RPC framing on WS is text-only. Close the connection with
    %% status 1003 (Unsupported Data) rather than silently dropping the
    %% frame, so a client that sends binary by mistake gets explicit
    %% feedback instead of a stalled stream.
    {[{close, 1003, <<"binary frames not supported">>}], State};
websocket_handle(_Frame, State) ->
    {[], State}.

websocket_info({json_rpc_push, Frame}, State) ->
    %% Server-push delivery from `json_rpc_ws:push/3' or `publish/3'. The
    %% frame is already an encoded JSON-RPC Notification; emit it as a
    %% text frame.
    {[{text, Frame}], State};
websocket_info(json_rpc_drain, State) ->
    %% Listener is shutting down. Send a 1001 (Going Away) close frame so
    %% the client knows to reconnect elsewhere; Cowboy will tear the
    %% connection down once the close frame is flushed.
    {[{close, 1001, <<"server shutting down">>}], State};
websocket_info(_Info, State) ->
    {[], State}.

terminate(_Reason, _Req, _State) ->
    ok.

decode_json(Body) ->
    try
        {ok, jiffy:decode(Body, [return_maps])}
    catch
        _:_ -> {error, parse_error}
    end.
