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

-export([
    dispatch/1,
    create_error_response/3,
    create_error_response/4,
    call_id_for_error/1
]).

-type id() :: binary() | integer() | float() | null.
-type reply() :: no_response | map() | [map()].

-export_type([id/0, reply/0]).

%% @doc Dispatch a parsed JSON-RPC payload (single request or batch). Looks
%% each method up in the `json_rpc_methods' ETS registry. Returns either
%% `no_response' (for notifications and all-notification batches) or an Erlang
%% term ready to be jiffy-encoded.
-spec dispatch(term()) -> reply().
dispatch([]) ->
    create_error_response(null, -32600, <<"Invalid Request">>);
dispatch(Requests) when is_list(Requests) ->
    Responses = [process_single_request(R) || R <- Requests],
    case [R || R <- Responses, R =/= no_response] of
        [] -> no_response;
        Filtered -> Filtered
    end;
dispatch(Request) when is_map(Request), map_size(Request) == 0 ->
    create_error_response(null, -32600, <<"Invalid Request">>);
dispatch(Request) when is_map(Request) ->
    process_single_request(Request);
dispatch(_Request) ->
    create_error_response(null, -32600, <<"Invalid Request">>).

%% Internal

process_single_request(Request) when is_map(Request) ->
    %% Extract the id best-effort first so a per-element envelope error can
    %% still echo the request id back to the client. extract_call_id/1
    %% returns 'notification' (id absent), null/binary/integer/float (valid
    %% id), or {error, invalid_id} (id present but not a permitted JSON
    %% type).
    case extract_call_id(Request) of
        {error, invalid_id} ->
            create_error_response(null, -32600, <<"Invalid Request">>);
        Id ->
            case validate_envelope(Request) of
                {ok, Method} ->
                    case validate_params(Request) of
                        {ok, Params} ->
                            dispatch_method(Method, Params, Id);
                        {error, Code, Msg} ->
                            response_or_drop(
                                Id, create_error_response(call_id(Id), Code, Msg)
                            )
                    end;
                {error, Code, Msg} ->
                    %% Envelope-level errors are not droppable as
                    %% notifications: the request was malformed, so we owe
                    %% the client an error envelope (with id: null when the
                    %% id was absent).
                    create_error_response(call_id(Id), Code, Msg)
            end
    end;
process_single_request(_) ->
    create_error_response(null, -32600, <<"Invalid Request">>).

validate_envelope(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method}) when
    is_binary(Method), Method =/= <<>>
->
    {ok, Method};
validate_envelope(_) ->
    {error, -32600, <<"Invalid Request">>}.

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
%%   - present and one of String/Number/Null per spec: return verbatim
%%   - present but any other JSON type (boolean, array, object): {error, invalid_id}
extract_call_id(Request) ->
    case maps:find(<<"id">>, Request) of
        error -> notification;
        {ok, null} -> null;
        {ok, Id} when is_binary(Id) -> Id;
        {ok, Id} when is_integer(Id) -> Id;
        {ok, Id} when is_float(Id) -> Id;
        {ok, _} -> {error, invalid_id}
    end.

dispatch_method(Method, Params, Id) ->
    case json_rpc_methods:lookup(Method) of
        not_found ->
            response_or_drop(Id, create_error_response(call_id(Id), -32601, <<"Method not found">>));
        {ok, {mfa, M, F}} ->
            invoke(fun() -> apply(M, F, [Params]) end, Id)
    end.

invoke(Thunk, Id) ->
    try
        Result = Thunk(),
        case Id of
            notification -> no_response;
            _ -> create_result_response(Id, Result)
        end
    catch
        throw:{jsonrpc_error, Code, Msg} when is_integer(Code), is_binary(Msg) ->
            handle_thrown(Id, Code, Msg, no_data);
        throw:{jsonrpc_error, Code, Msg, Data} when is_integer(Code), is_binary(Msg) ->
            handle_thrown(Id, Code, Msg, {data, Data});
        %% Convert handler bugs (`error:_') and bare throws into a -32603
        %% envelope so a single misbehaving handler doesn't take down the
        %% connection. We deliberately do *not* catch `exit:_': an
        %% `exit/1' from a handler signals an unrecoverable failure that
        %% should propagate to `json_rpc_worker', which kills the worker
        %% process, returns `{error, {crash, exit, _}}' to the transport,
        %% and lets the transport reply with a generic Internal error.
        Class:Reason:Stacktrace when Class =:= error; Class =:= throw ->
            ?LOG_ERROR("Handler error: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            response_or_drop(Id, create_error_response(call_id(Id), -32603, <<"Internal error">>))
    end.

%% A handler must use the application-defined error space. Reject any code in
%% the JSON-RPC reserved range -32768..-32000 — substituting -32603 Internal
%% error so the framework's own codes can't be impersonated by a handler.
handle_thrown(Id, Code, _Msg, _Data) when Code >= -32768, Code =< -32000 ->
    ?LOG_WARNING(
        "Handler threw reserved JSON-RPC error code ~p; substituting -32603", [Code]
    ),
    response_or_drop(Id, create_error_response(call_id(Id), -32603, <<"Internal error">>));
handle_thrown(Id, Code, Msg, no_data) ->
    response_or_drop(Id, create_error_response(call_id(Id), Code, Msg));
handle_thrown(Id, Code, Msg, {data, Data}) ->
    response_or_drop(Id, create_error_response(call_id(Id), Code, Msg, Data)).

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

%% Best-effort id extraction from a parsed JSON-RPC payload, used by the
%% transport layers when synthesising a timeout/crash error envelope. For a
%% single-call object whose `id' is one of the JSON types permitted by the
%% spec (string/number/null) we echo it back so the client can correlate the
%% error to the originating request. For batches and any other shape we fall
%% back to `null' — there is no single id to attribute the error to.
-spec call_id_for_error(term()) -> id().
call_id_for_error(Parsed) when is_map(Parsed) ->
    case maps:find(<<"id">>, Parsed) of
        {ok, Id} when is_binary(Id); is_integer(Id); is_float(Id); Id =:= null -> Id;
        _ -> null
    end;
call_id_for_error(_Parsed) ->
    null.
