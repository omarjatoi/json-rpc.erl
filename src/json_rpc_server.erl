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

-module(json_rpc_server).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, stop/0, register_method/2, set_auth/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {listener, methods = #{}, auth_fun}).

%% API
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

stop() ->
    gen_server:stop(?SERVER).

register_method(Name, Fun) when is_binary(Name), is_function(Fun, 1) ->
    gen_server:call(?SERVER, {register_method, Name, Fun}, 5000).

set_auth(AuthFun) when is_function(AuthFun, 1) ->
    gen_server:call(?SERVER, {set_auth, AuthFun}).

%% gen_server callbacks
init([Port]) when is_integer(Port), Port > 0, Port < 65536 ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            self() ! accept,
            {ok, #state{listener = ListenSocket}};
        {error, Reason} ->
            {stop, Reason}
    end;
init([Port]) ->
    {stop, {invalid_port, Port}}.

handle_call({register_method, Name, Fun}, _From, State) ->
    NewMethods = maps:put(Name, Fun, State#state.methods),
    {reply, ok, State#state{methods = NewMethods}};
handle_call({set_auth, AuthFun}, _From, State) ->
    {reply, ok, State#state{auth_fun = AuthFun}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(accept, #state{listener = ListenSocket} = State) ->
    % Add a 0 timeout
    case gen_tcp:accept(ListenSocket, 0) of
        {ok, Socket} ->
            Pid = spawn_link(fun() -> wait_and_handle(State) end),
            ok = gen_tcp:controlling_process(Socket, Pid),
            Pid ! {start, Socket},
            self() ! accept,
            {noreply, State};
        {error, timeout} ->
            % Schedule another accept after 1000ms
            erlang:send_after(1000, self(), accept),
            {noreply, State};
        {error, closed} ->
            {stop, normal, State};
        {error, Reason} ->
            ?LOG_ERROR("Error in accept: ~p", [Reason]),
            {stop, Reason, State}
    end;
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    ?LOG_ERROR("Client ~p exited: ~p", [Pid, Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listener = ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

wait_and_handle(State) ->
    receive
        {start, Socket} ->
            handle_client(Socket, State)
    after 5000 ->
        ?LOG_ERROR("Client worker timed out waiting for socket transfer"),
        ok
    end.

handle_client(Socket, State) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            handle_request(Socket, Data, State),
            handle_client(Socket, State);
        {error, closed} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Client socket error: ~p", [Reason])
    end.

handle_request(Socket, Data, State) ->
    try
        Request = jiffy:decode(Data, [return_maps]),
        case authenticate(Request, State) of
            ok ->
                Response = process_request(Request, State),
                send_response(Socket, Response);
            error ->
                ErrorResponse = create_error_response(
                    extract_id(Request), -32000, <<"Authentication failed">>
                ),
                send_response(Socket, ErrorResponse)
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Parse error: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            ParseErrorResponse = create_error_response(null, -32700, <<"Parse error">>),
            send_response(Socket, ParseErrorResponse)
    end.

send_response(_Socket, no_response) ->
    ok;
send_response(Socket, Response) ->
    case gen_tcp:send(Socket, jiffy:encode(Response)) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to send response: ~p", [Reason]),
            ok
    end.

authenticate(_Request, #state{auth_fun = undefined}) ->
    ok;
authenticate(Request, #state{auth_fun = AuthFun}) ->
    try
        case AuthFun(Request) of
            ok -> ok;
            _ -> error
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Auth function failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            error
    end.

process_request([], _State) ->
    create_error_response(null, -32600, <<"Invalid Request">>);
process_request(Requests, State) when is_list(Requests) ->
    Responses = [process_single_request(R, State) || R <- Requests],
    case [R || R <- Responses, R =/= no_response] of
        [] -> no_response;
        Filtered -> Filtered
    end;
process_request(Request, _State) when is_map(Request), map_size(Request) == 0 ->
    create_error_response(null, -32600, <<"Invalid Request">>);
process_request(Request, State) when is_map(Request) ->
    process_single_request(Request, State);
process_request(_Request, _State) ->
    create_error_response(null, -32600, <<"Invalid Request">>).

process_single_request(
    #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Request,
    State
) when is_binary(Method), Method =/= <<>> ->
    Id = extract_call_id(Request),
    case is_reserved_method(Method) of
        true ->
            response_or_drop(Id, create_error_response(call_id(Id), -32601, <<"Method not found">>));
        false ->
            case validate_params(Request) of
                {ok, Params} ->
                    dispatch(Method, Params, Id, State);
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

dispatch(Method, Params, Id, State) ->
    case maps:get(Method, State#state.methods, not_found) of
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

create_error_response(Id, Code, Message) ->
    #{
        jsonrpc => <<"2.0">>,
        error => #{code => Code, message => Message},
        id => Id
    }.

create_error_response(Id, Code, Message, Data) ->
    #{
        jsonrpc => <<"2.0">>,
        error => #{code => Code, message => Message, data => Data},
        id => Id
    }.

extract_id(#{<<"id">> := Id}) ->
    Id;
extract_id(_) ->
    null.
