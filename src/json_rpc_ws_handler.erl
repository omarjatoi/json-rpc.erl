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
    {cowboy_websocket, Req, State}.

websocket_init(_State) ->
    {[], #{}}.

websocket_handle({text, Frame}, State) ->
    case decode_json(Frame) of
        {ok, Parsed} ->
            Timeout = json_rpc_config:get(request_timeout_ms),
            case json_rpc_worker:run(Parsed, Timeout) of
                {ok, no_response} ->
                    {[], State};
                {ok, Reply} ->
                    {[{text, jiffy:encode(Reply)}], State};
                {error, timeout} ->
                    ErrBody = jiffy:encode(
                        json_rpc_dispatcher:create_error_response(
                            null, -32603, <<"Internal error">>, #{reason => timeout}
                        )
                    ),
                    {[{text, ErrBody}], State};
                {error, {crash, Class, Reason}} ->
                    ?LOG_ERROR("Handler crashed: ~p:~p", [Class, Reason]),
                    ErrBody = jiffy:encode(
                        json_rpc_dispatcher:create_error_response(
                            null, -32603, <<"Internal error">>
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
    {[], State};
websocket_handle(_Frame, State) ->
    {[], State}.

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
