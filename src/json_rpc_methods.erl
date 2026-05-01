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

-module(json_rpc_methods).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([
    start_link/0,
    register_method/2,
    unregister_method/1,
    lookup/1,
    list_methods/0,
    discover/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(TABLE, ?MODULE).
-define(BUILTIN_DISCOVER, <<"rpc.discover">>).

-type handler() :: {module(), atom()}.
-type stored_handler() :: {mfa, module(), atom()}.
-type method_name() :: binary().

-export_type([handler/0, method_name/0]).

%%% Public API

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_method(method_name(), handler()) ->
    ok
    | {error, {invalid_method_name, term()}}
    | {error, {invalid_handler, term()}}
    | {error, registry_full}.
register_method(Name, Handler) ->
    case validate_name(Name) of
        ok ->
            case validate_handler(Handler) of
                {ok, Stored} ->
                    gen_server:call(?MODULE, {register, Name, Stored}, 5000);
                {error, _} = E ->
                    E
            end;
        {error, _} = E ->
            E
    end.

-spec unregister_method(method_name()) -> ok | {error, {invalid_method_name, term()}}.
unregister_method(Name) when is_binary(Name), Name =/= <<>> ->
    gen_server:call(?MODULE, {unregister, Name}, 5000);
unregister_method(Name) ->
    {error, {invalid_method_name, Name}}.

-spec lookup(method_name()) -> {ok, stored_handler()} | not_found.
lookup(Name) when is_binary(Name) ->
    case ets:lookup(?TABLE, Name) of
        [{Name, Handler}] -> {ok, Handler};
        [] -> not_found
    end.

-spec list_methods() -> [method_name()].
list_methods() ->
    ets:select(?TABLE, [{{'$1', '_'}, [], ['$1']}]).

%% Built-in `rpc.discover'. Takes no params; returns the list of currently
%% registered method names as a JSON array (encoded by the dispatcher).
-spec discover(term()) -> [method_name()].
discover(_Params) ->
    list_methods().

%%% gen_server callbacks

-spec init([]) -> {ok, map()}.
init([]) ->
    _ = ets:new(?TABLE, [set, named_table, protected, {read_concurrency, true}]),
    %% Insert the built-in discovery method directly; it bypasses the
    %% reserved-namespace check by design.
    true = ets:insert_new(?TABLE, {?BUILTIN_DISCOVER, {mfa, ?MODULE, discover}}),
    ?LOG_DEBUG("json_rpc_methods: registered built-in ~s", [?BUILTIN_DISCOVER]),
    {ok, #{}}.

handle_call({register, Name, Stored}, _From, State) ->
    Max = json_rpc_config:get(max_methods),
    Existing = ets:member(?TABLE, Name),
    case (not Existing) andalso ets:info(?TABLE, size) >= Max of
        true ->
            {reply, {error, registry_full}, State};
        false ->
            true = ets:insert(?TABLE, {Name, Stored}),
            ?LOG_DEBUG("json_rpc_methods: registered ~s", [Name]),
            {reply, ok, State}
    end;
handle_call({unregister, Name}, _From, State) ->
    true = ets:delete(?TABLE, Name),
    ?LOG_DEBUG("json_rpc_methods: unregistered ~s", [Name]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal

validate_name(<<>>) ->
    {error, {invalid_method_name, <<>>}};
validate_name(?BUILTIN_DISCOVER) ->
    ok;
validate_name(<<"rpc.", _/binary>> = N) ->
    {error, {invalid_method_name, N}};
validate_name(N) when is_binary(N) ->
    ok;
validate_name(N) ->
    {error, {invalid_method_name, N}}.

validate_handler({M, F}) when is_atom(M), is_atom(F) ->
    {ok, {mfa, M, F}};
validate_handler(H) ->
    {error, {invalid_handler, H}}.
