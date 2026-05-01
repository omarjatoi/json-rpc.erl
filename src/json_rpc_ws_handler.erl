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
    %% We do not currently support any subprotocols. If the client offered
    %% one, refuse the upgrade with 400 Bad Request rather than silently
    %% ignoring their advertised expectations.
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        undefined ->
            {cowboy_websocket, Req, State};
        _Offered ->
            Req1 = cowboy_req:reply(400, #{}, <<>>, Req),
            {ok, Req1, State}
    end.

websocket_init(_State) ->
    {[], #{}}.

websocket_handle({text, Frame}, State) ->
    case decode_json(Frame) of
        {ok, Parsed} ->
            Methods = json_rpc_server:get_methods(),
            Reply = json_rpc_dispatcher:dispatch(Parsed, Methods),
            case Reply of
                no_response ->
                    {[], State};
                _ ->
                    {[{text, jiffy:encode(Reply)}], State}
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
