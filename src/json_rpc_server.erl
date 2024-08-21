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
    end.

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
            Pid = spawn_link(fun() -> handle_client(Socket, State) end),
            gen_tcp:controlling_process(Socket, Pid),
            self() ! accept,
            {noreply, State};
        {error, timeout} ->
            % Schedule another accept after 1000ms
            erlang:send_after(1000, self(), accept),
            {noreply, State};
        {error, closed} ->
            {stop, normal, State};
        {error, Reason} ->
            logger:error("Error in accept: ~p~n", [Reason]),
            {stop, Reason, State}
    end;
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    logger:error("Client ~p exited: ~p", [Pid, Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listener = ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
handle_client(Socket, State) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            handle_request(Socket, Data, State),
            handle_client(Socket, State);
        {error, closed} ->
            ok;
        {error, Reason} ->
            logger:error("Client socket error: ~p", [Reason])
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
        error:badarg ->
            ParseErrorResponse = create_error_response(null, -32700, <<"Parse error">>),
            send_response(Socket, ParseErrorResponse)
    end.

send_response(_Socket, no_response) ->
    ok;
send_response(Socket, Response) ->
    gen_tcp:send(Socket, jiffy:encode(Response)).

authenticate(_Request, #state{auth_fun = undefined}) ->
    ok;
authenticate(Request, #state{auth_fun = AuthFun}) ->
    try
        case AuthFun(Request) of
            ok -> ok;
            _ -> error
        end
    catch
        _:_ -> error
    end.

process_request([], _State) ->
    [create_error_response(null, -32600, <<"Invalid Request">>)];
process_request(Requests, State) when is_list(Requests) ->
    [process_single_request(R, State) || R <- Requests];
process_request(Request, _State) when map_size(Request) == 0 ->
    create_error_response(null, -32600, <<"Invalid Request">>);
process_request(Request, State) ->
    process_single_request(Request, State).
process_single_request(
    #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Request,
    State
) ->
    Id = maps:get(<<"id">>, Request, null),
    case maps:get(Method, State#state.methods, not_found) of
        not_found ->
            create_error_response(Id, -32601, <<"Method not found">>);
        Fun when is_function(Fun, 1) ->
            Params = maps:get(<<"params">>, Request, []),
            try
                Result = Fun(Params),
                case Id of
                    null ->
                        % This is a notification, don't send a response
                        no_response;
                    _ ->
                        create_result_response(Id, Result)
                end
            catch
                _:_ ->
                    create_error_response(Id, -32603, <<"Internal error">>)
            end
    end;
process_single_request(_, _) ->
    create_error_response(null, -32600, <<"Invalid Request">>).

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

extract_id(#{<<"id">> := Id}) ->
    Id;
extract_id(_) ->
    null.
