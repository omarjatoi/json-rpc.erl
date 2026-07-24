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

-moduledoc """
The method registry: a name-to-handler map backed by ETS.

Writes go through this `m:gen_server` so validation and the size cap cannot
be raced. Reads go straight to the ETS table, which is `protected` and
configured for read concurrency — dispatch is the hot path and must not
queue behind a registration.

## Handler arity

A handler is a `{Module, Function}` pair. It is called with the request's
`params` as its only argument, or — if the module exports the same name at
arity 2 — with `params` and the request context (see
`t:json_rpc_dispatcher:context/0`). Arity 2 is preferred when both exist,
because a handler that wants the context has no other way to get it. The
export is checked at registration, so a typo fails immediately rather than
on the first call.

## Surviving a restart

If this process crashes, the table dies with it and `rest_for_one` restarts
the listener alongside it. Methods listed in the `methods` application
environment key are re-registered automatically on every start, so a
statically declared method set heals itself. Methods registered at runtime
do not: an application that registers dynamically should either declare them
in `methods` instead or re-register from its own supervision tree.
""".

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/0,
    register_method/2,
    unregister_method/1,
    lookup/1,
    list_methods/0,
    discover/1
]).

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
-define(CALL_TIMEOUT, 5000).

-doc "A handler: a module and an exported function of arity 1 or 2.".
-type handler() :: {module(), atom()}.

-doc "How a handler is stored, with the arity resolved at registration.".
-type stored_handler() :: {mfa, module(), atom(), 1 | 2}.

-doc "A registered method name. Must be a non-empty binary.".
-type method_name() :: binary().

-type register_error() ::
    {invalid_method_name, term()}
    | {invalid_handler, term()}
    | {undefined_handler, handler()}
    | registry_full.

-export_type([handler/0, stored_handler/0, method_name/0, register_error/0]).

%%% Public API

-doc "Start the registry. Called by `m:json_rpc_sup`.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Register `Handler` under `Name`, replacing any existing entry.

Fails with `{invalid_method_name, Name}` for an empty name or one in the
specification-reserved `rpc.` namespace, `{invalid_handler, Handler}` if the
handler is not a `{Module, Function}` pair, `{undefined_handler, Handler}`
if that module exports no such function at arity 1 or 2, and `registry_full`
once `max_methods` entries exist.
""".
-spec register_method(method_name(), handler()) -> ok | {error, register_error()}.
register_method(Name, Handler) ->
    case validate_name(Name) of
        ok ->
            case resolve_handler(Handler) of
                {ok, Stored} -> gen_server:call(?MODULE, {register, Name, Stored}, ?CALL_TIMEOUT);
                {error, _Reason} = Error -> Error
            end;
        {error, _Reason} = Error ->
            Error
    end.

-doc """
Remove `Name` from the registry.

Returns `{error, not_found}` if it was not registered, and
`{error, {reserved_method_name, Name}}` for built-ins such as
`rpc.discover`, which may not be removed.
""".
-spec unregister_method(method_name()) ->
    ok | {error, not_found | {invalid_method_name, term()} | {reserved_method_name, binary()}}.
unregister_method(?BUILTIN_DISCOVER) ->
    {error, {reserved_method_name, ?BUILTIN_DISCOVER}};
unregister_method(Name) when is_binary(Name), Name =/= <<>> ->
    gen_server:call(?MODULE, {unregister, Name}, ?CALL_TIMEOUT);
unregister_method(Name) ->
    {error, {invalid_method_name, Name}}.

-doc """
Look up the handler registered under `Name`.

Returns `{error, unavailable}` when the registry is not running — the table
is gone during a restart, and dispatch has to report that as an internal
error rather than as a missing method.
""".
-spec lookup(method_name()) -> {ok, stored_handler()} | not_found | {error, unavailable}.
lookup(Name) when is_binary(Name) ->
    try ets:lookup(?TABLE, Name) of
        [{Name, Handler}] -> {ok, Handler};
        [] -> not_found
    catch
        error:badarg -> {error, unavailable}
    end.

-doc "Every registered method name, in no particular order.".
-spec list_methods() -> [method_name()].
list_methods() ->
    try
        ets:select(?TABLE, [{{'$1', '_'}, [], ['$1']}])
    catch
        error:badarg -> []
    end.

-doc """
The built-in `rpc.discover` method: lists every registered method name.

Registered automatically at start; it takes no parameters and cannot be
unregistered.
""".
-spec discover(json_rpc_request:params()) -> [method_name()].
discover(_Params) ->
    list_methods().

%%% gen_server callbacks

-doc false.
-spec init([]) -> {ok, #{}}.
init([]) ->
    _ = ets:new(?TABLE, [set, named_table, protected, {read_concurrency, true}]),
    %% The built-in bypasses the reserved-namespace check by design.
    true = ets:insert_new(?TABLE, {?BUILTIN_DISCOVER, {mfa, ?MODULE, discover, 1}}),
    ok = seed_from_env(),
    {ok, #{}}.

-doc false.
handle_call({register, Name, Stored}, _From, State) ->
    {reply, do_register(Name, Stored), State};
handle_call({unregister, Name}, _From, State) ->
    {reply, do_unregister(Name), State};
handle_call(Request, _From, State) ->
    ?LOG_WARNING("json_rpc_methods: unexpected call ~p", [Request]),
    {reply, {error, unknown_call}, State}.

-doc false.
handle_cast(_Msg, State) ->
    {noreply, State}.

-doc false.
handle_info(_Info, State) ->
    {noreply, State}.

-doc false.
terminate(_Reason, _State) ->
    ok.

-doc false.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal

do_register(Name, Stored) ->
    Max = json_rpc_config:get(max_methods),
    IsNew = not ets:member(?TABLE, Name),
    case IsNew andalso ets:info(?TABLE, size) >= Max of
        true ->
            ?LOG_WARNING(
                "json_rpc_methods: refusing ~ts, registry is full (max_methods=~p)", [Name, Max]
            ),
            {error, registry_full};
        false ->
            true = ets:insert(?TABLE, {Name, Stored}),
            ?LOG_DEBUG("json_rpc_methods: registered ~ts", [Name]),
            ok
    end.

do_unregister(Name) ->
    case ets:member(?TABLE, Name) of
        true ->
            true = ets:delete(?TABLE, Name),
            ?LOG_DEBUG("json_rpc_methods: unregistered ~ts", [Name]),
            ok;
        false ->
            {error, not_found}
    end.

%% Re-register the statically declared method set on every start, so a
%% registry crash does not silently leave the server with no methods.
seed_from_env() ->
    lists:foreach(
        fun({Name, Handler}) ->
            case validate_name(Name) of
                ok -> seed_one(Name, Handler);
                {error, Reason} -> log_seed_failure(Name, Reason)
            end
        end,
        json_rpc_config:get(methods)
    ).

seed_one(Name, Handler) ->
    case resolve_handler(Handler) of
        {ok, Stored} ->
            case do_register(Name, Stored) of
                ok -> ok;
                {error, Reason} -> log_seed_failure(Name, Reason)
            end;
        {error, Reason} ->
            log_seed_failure(Name, Reason)
    end.

log_seed_failure(Name, Reason) ->
    ?LOG_ERROR("json_rpc_methods: could not seed ~tp from config: ~p", [Name, Reason]).

validate_name(<<>>) ->
    {error, {invalid_method_name, <<>>}};
validate_name(?BUILTIN_DISCOVER) ->
    ok;
validate_name(<<"rpc.", _Rest/binary>> = Name) ->
    {error, {invalid_method_name, Name}};
validate_name(Name) when is_binary(Name) ->
    ok;
validate_name(Name) ->
    {error, {invalid_method_name, Name}}.

%% Resolve the arity once, at registration, so dispatch never has to ask and
%% a misspelled function name fails here instead of on the first call.
resolve_handler({Module, Function} = Handler) when is_atom(Module), is_atom(Function) ->
    _ = code:ensure_loaded(Module),
    case {exported(Module, Function, 2), exported(Module, Function, 1)} of
        {true, _} -> {ok, {mfa, Module, Function, 2}};
        {false, true} -> {ok, {mfa, Module, Function, 1}};
        {false, false} -> {error, {undefined_handler, Handler}}
    end;
resolve_handler(Handler) ->
    {error, {invalid_handler, Handler}}.

exported(Module, Function, Arity) ->
    erlang:function_exported(Module, Function, Arity).
