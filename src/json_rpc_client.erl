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

-include_lib("kernel/include/logger.hrl").

-export([connect/2, call/4, notify/3, batch/2, close/1, set_auth/2, raw_request/2]).

-record(client, {socket, auth}).

-opaque client() :: #client{}.
-type id() :: binary() | integer() | null.
-type params() :: list() | map().
-type rpc_error() :: #{binary() => term()}.
-type response() ::
    {ok, term(), id()}
    | {error, rpc_error(), id()}
    | {error, invalid_response}.
-type batch_request() :: {binary(), params(), id() | undefined}.

-export_type([client/0, id/0, params/0, rpc_error/0, response/0]).

-spec connect(string(), inet:port_number()) -> {ok, client()} | {error, term()}.
connect(Host, Port) when is_list(Host), is_integer(Port), Port > 0, Port < 65536 ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            {ok, #client{socket = Socket}};
        Error ->
            Error
    end.

-spec call(client(), binary(), params(), id()) ->
    response() | {error, term()}.
call(Client, Method, Params, Id) when is_binary(Method) ->
    Request = create_request(Method, Params, Id),
    send_and_receive(Client, Request).

-spec notify(client(), binary(), params()) -> ok | {error, term()}.
notify(Client, Method, Params) when is_binary(Method) ->
    Request = create_request(Method, Params, undefined),
    send_request(Client, Request).

-spec batch(client(), [batch_request()]) ->
    ok | {ok, [response()]} | {error, term()}.
batch(Client, Requests) when is_list(Requests) ->
    {Notifications, BatchRequests} = lists:partition(
        fun({_, _, Id}) -> Id =:= undefined end,
        Requests
    ),

    NotificationRequests = [create_request(M, P, undefined) || {M, P, _} <- Notifications],
    BatchRequestsWithIds = [create_request(M, P, I) || {M, P, I} <- BatchRequests],

    % Send notifications, propagating the first error
    case send_each(Client, NotificationRequests) of
        ok ->
            % Send batch requests and receive responses
            case BatchRequestsWithIds of
                [] ->
                    ok;
                _ ->
                    send_and_receive(Client, BatchRequestsWithIds)
            end;
        {error, _} = Error ->
            Error
    end.

send_each(_Client, []) ->
    ok;
send_each(Client, [Req | Rest]) ->
    case send_request(Client, Req) of
        ok -> send_each(Client, Rest);
        {error, _} = Error -> Error
    end.

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
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to parse response: ~p:~p~n~p", [Class, Reason, Stacktrace]),
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

-spec close(client()) -> ok.
close(#client{socket = Socket}) ->
    gen_tcp:close(Socket).

-spec set_auth(client(), term()) -> client().
set_auth(Client, Auth) ->
    Client#client{auth = Auth}.

%% @doc Low-level escape hatch for tests: send the given binary payload
%% verbatim and parse whatever the server replies with. Bypasses request
%% encoding entirely so that test cases can inject malformed JSON or
%% arbitrary structures to exercise server-side error paths. NOT part of
%% the supported public API for application code.
-spec raw_request(client(), binary()) ->
    response() | {ok, [response()]} | {error, term()}.
raw_request(#client{socket = Socket}, Payload) when is_binary(Payload) ->
    case gen_tcp:send(Socket, Payload) of
        ok ->
            receive_response(Socket);
        {error, _} = Error ->
            Error
    end.
