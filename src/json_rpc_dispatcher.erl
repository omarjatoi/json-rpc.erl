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

-module(json_rpc_dispatcher).

-include_lib("kernel/include/logger.hrl").

-export([dispatch/2, create_error_response/3, create_error_response/4]).

-type id() :: binary() | integer() | null.
-type method_fun() :: fun((term()) -> term()).
-type methods() :: #{binary() => method_fun()}.
-type reply() :: no_response | map() | [map()].

-export_type([id/0, method_fun/0, methods/0, reply/0]).

%% @doc Dispatch a parsed JSON-RPC payload (single request or batch) against
%% the given methods map. Returns either `no_response' (for notifications and
%% all-notification batches) or an Erlang term ready to be jiffy-encoded.
-spec dispatch(term(), methods()) -> reply().
dispatch(Parsed, Methods) ->
    process_request(Parsed, Methods).

%% Internal

process_request([], _Methods) ->
    create_error_response(null, -32600, <<"Invalid Request">>);
process_request(Requests, Methods) when is_list(Requests) ->
    Responses = [process_single_request(R, Methods) || R <- Requests],
    case [R || R <- Responses, R =/= no_response] of
        [] -> no_response;
        Filtered -> Filtered
    end;
process_request(Request, _Methods) when is_map(Request), map_size(Request) == 0 ->
    create_error_response(null, -32600, <<"Invalid Request">>);
process_request(Request, Methods) when is_map(Request) ->
    process_single_request(Request, Methods);
process_request(_Request, _Methods) ->
    create_error_response(null, -32600, <<"Invalid Request">>).

process_single_request(
    #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Request,
    Methods
) when is_binary(Method), Method =/= <<>> ->
    Id = extract_call_id(Request),
    case is_reserved_method(Method) of
        true ->
            response_or_drop(Id, create_error_response(call_id(Id), -32601, <<"Method not found">>));
        false ->
            case validate_params(Request) of
                {ok, Params} ->
                    dispatch_method(Method, Params, Id, Methods);
                {error, Code, Msg} ->
                    response_or_drop(Id, create_error_response(call_id(Id), Code, Msg))
            end
    end;
process_single_request(_, _) ->
    create_error_response(null, -32600, <<"Invalid Request">>).

is_reserved_method(<<"rpc.", _/binary>>) -> true;
is_reserved_method(_) -> false.

validate_params(Request) ->
    case maps:find(<<"params">>, Request) of
        error ->
            {ok, []};
        {ok, Params} when is_list(Params); is_map(Params) ->
            {ok, Params};
        {ok, _} ->
            {error, -32602, <<"Invalid params">>}
    end.

%% Returns the id for a call/notification:
%%   - missing key: notification (encoded internally as the atom 'notification')
%%   - present (including null): call; return the id value verbatim
extract_call_id(Request) ->
    case maps:find(<<"id">>, Request) of
        error -> notification;
        {ok, Id} -> Id
    end.

dispatch_method(Method, Params, Id, Methods) ->
    case maps:get(Method, Methods, not_found) of
        not_found ->
            response_or_drop(Id, create_error_response(call_id(Id), -32601, <<"Method not found">>));
        Fun when is_function(Fun, 1) ->
            invoke(Fun, Params, Id)
    end.

invoke(Fun, Params, Id) ->
    try
        Result = Fun(Params),
        case Id of
            notification -> no_response;
            _ -> create_result_response(Id, Result)
        end
    catch
        throw:{jsonrpc_error, Code, Msg} when is_integer(Code), is_binary(Msg) ->
            response_or_drop(Id, create_error_response(call_id(Id), Code, Msg));
        throw:{jsonrpc_error, Code, Msg, Data} when is_integer(Code), is_binary(Msg) ->
            response_or_drop(Id, create_error_response(call_id(Id), Code, Msg, Data));
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Handler error: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            response_or_drop(Id, create_error_response(call_id(Id), -32603, <<"Internal error">>))
    end.

response_or_drop(notification, _Response) -> no_response;
response_or_drop(_Id, Response) -> Response.

call_id(notification) -> null;
call_id(Id) -> Id.

create_result_response(Id, Result) ->
    #{
        jsonrpc => <<"2.0">>,
        result => Result,
        id => Id
    }.

-spec create_error_response(id(), integer(), binary()) -> map().
create_error_response(Id, Code, Message) ->
    #{
        jsonrpc => <<"2.0">>,
        error => #{code => Code, message => Message},
        id => Id
    }.

-spec create_error_response(id(), integer(), binary(), term()) -> map().
create_error_response(Id, Code, Message, Data) ->
    #{
        jsonrpc => <<"2.0">>,
        error => #{code => Code, message => Message, data => Data},
        id => Id
    }.
