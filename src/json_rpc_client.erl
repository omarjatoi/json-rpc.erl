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

-module(json_rpc_client).

-export([connect/2, call/4, notify/3, batch/2, close/1, set_auth/2]).

-record(client, {socket, auth}).

connect(Host, Port) when is_list(Host), is_integer(Port), Port > 0, Port < 65536 ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            {ok, #client{socket = Socket}};
        Error ->
            Error
    end.

call(Client, Method, Params, Id) when is_binary(Method) ->
    Request = create_request(Method, Params, Id),
    send_and_receive(Client, Request).

notify(Client, Method, Params) when is_binary(Method) ->
    Request = create_request(Method, Params, undefined),
    send_request(Client, Request).

batch(Client, Requests) when is_list(Requests) ->
    BatchRequests = [create_request(M, P, I) || {M, P, I} <- Requests],
    send_and_receive(Client, BatchRequests).

create_request(Method, Params, Id) ->
    Base =
        #{
            jsonrpc => <<"2.0">>,
            method => Method,
            params => Params
        },
    case Id of
        undefined ->
            Base;
        _ ->
            Base#{id => Id}
    end.

send_and_receive(Client, Request) ->
    case send_request(Client, Request) of
        ok ->
            receive_response(Client#client.socket);
        Error ->
            Error
    end.

send_request(#client{socket = Socket, auth = Auth}, Request) ->
    FullRequest = add_auth(Request, Auth),
    gen_tcp:send(Socket, jiffy:encode(FullRequest)).

add_auth(Request, undefined) ->
    Request;
add_auth(Request, Auth) when is_map(Request) ->
    Request#{auth => Auth};
add_auth(Requests, Auth) when is_list(Requests) ->
    [add_auth(R, Auth) || R <- Requests].

receive_response(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            parse_response(Data);
        {error, Reason} ->
            {error, Reason}
    end.

parse_response(Data) ->
    try
        Response = jiffy:decode(Data, [return_maps]),
        case Response of
            Responses when is_list(Responses) ->
                {ok, [parse_single_response(R) || R <- Responses]};
            _ ->
                parse_single_response(Response)
        end
    catch
        error:badarg ->
            {error, parse_error}
    end.

parse_single_response(#{
    <<"jsonrpc">> := <<"2.0">>,
    <<"result">> := Result,
    <<"id">> := Id
}) ->
    {ok, Result, Id};
parse_single_response(#{
    <<"jsonrpc">> := <<"2.0">>,
    <<"error">> := Error,
    <<"id">> := Id
}) ->
    {error, Error, Id};
parse_single_response(_) ->
    {error, invalid_response}.

close(#client{socket = Socket}) ->
    gen_tcp:close(Socket).

set_auth(Client, Auth) ->
    Client#client{auth = Auth}.
