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

-module(json_rpc_registry_SUITE).

%% The method registry: validation at registration time, the size cap, the
%% protection around built-ins, and the table's write protection.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
    discover_lists_methods/1,
    register_rejects_reserved_namespace/1,
    register_rejects_empty_name/1,
    register_rejects_non_mfa_handler/1,
    register_rejects_undefined_function/1,
    register_prefers_arity_two/1,
    register_replaces_existing/1,
    registry_full_at_cap/1,
    unregister_unknown_returns_not_found/1,
    unregister_builtin_is_refused/1,
    table_is_write_protected/1,
    seeded_methods_registered_at_start/1,
    seeded_methods_survive_registry_crash/1
]).

-define(PORT, 18084).

all() ->
    [
        discover_lists_methods,
        register_rejects_reserved_namespace,
        register_rejects_empty_name,
        register_rejects_non_mfa_handler,
        register_rejects_undefined_function,
        register_prefers_arity_two,
        register_replaces_existing,
        registry_full_at_cap,
        unregister_unknown_returns_not_found,
        unregister_builtin_is_refused,
        table_is_write_protected,
        seeded_methods_registered_at_start,
        seeded_methods_survive_registry_crash
    ].

init_per_suite(Config) ->
    Seeded = [{<<"seeded">>, {json_rpc_test_methods, subtract}}],
    ok = json_rpc_test_support:start_app(?PORT, #{methods => Seeded}),
    Config.

end_per_suite(_Config) ->
    ok = json_rpc_test_support:stop_app().

%%% Cases

discover_lists_methods(_Config) ->
    Methods = json_rpc:dispatch(#{
        <<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"rpc.discover">>, <<"id">> => 1
    }),
    #{result := Names} = Methods,
    Required = [Name || {Name, _Handler} <- json_rpc_test_support:method_specs()],
    lists:foreach(fun(Name) -> ?assert(lists:member(Name, Names)) end, Required),
    ?assert(lists:member(<<"rpc.discover">>, Names)).

%% The `rpc.' namespace belongs to the specification, so applications cannot
%% claim names in it.
register_rejects_reserved_namespace(_Config) ->
    ?assertEqual(
        {error, {invalid_method_name, <<"rpc.foo">>}},
        json_rpc:register(<<"rpc.foo">>, {json_rpc_test_methods, subtract})
    ).

register_rejects_empty_name(_Config) ->
    ?assertEqual(
        {error, {invalid_method_name, <<>>}},
        json_rpc:register(<<>>, {json_rpc_test_methods, subtract})
    ).

register_rejects_non_mfa_handler(_Config) ->
    ?assertMatch(
        {error, {invalid_handler, _Handler}},
        json_rpc:register(<<"bogus">>, not_an_mfa)
    ),
    ?assertMatch(
        {error, {invalid_handler, _Handler}},
        json_rpc:register(<<"bogus">>, {only_one})
    ).

%% A typo'd function name must fail at registration, not on the first call.
register_rejects_undefined_function(_Config) ->
    ?assertEqual(
        {error, {undefined_handler, {json_rpc_test_methods, no_such_function}}},
        json_rpc:register(<<"typo">>, {json_rpc_test_methods, no_such_function})
    ).

%% A handler exporting both arities gets the context-aware one, since a
%% handler that wants the context has no other way to reach it.
register_prefers_arity_two(_Config) ->
    ok = json_rpc:register(<<"ctx">>, {json_rpc_test_methods, context}),
    ?assertEqual(
        {ok, {mfa, json_rpc_test_methods, context, 2}},
        json_rpc_methods:lookup(<<"ctx">>)
    ).

register_replaces_existing(_Config) ->
    ok = json_rpc:register(<<"replaceable">>, {json_rpc_test_methods, subtract}),
    ok = json_rpc:register(<<"replaceable">>, {json_rpc_test_methods, sum}),
    ?assertEqual(
        {ok, {mfa, json_rpc_test_methods, sum, 1}},
        json_rpc_methods:lookup(<<"replaceable">>)
    ).

registry_full_at_cap(_Config) ->
    Existing = length(json_rpc:methods()),
    json_rpc_test_support:with_env(max_methods, Existing + 2, fun() ->
        ok = json_rpc:register(<<"cap_a">>, {json_rpc_test_methods, subtract}),
        ok = json_rpc:register(<<"cap_b">>, {json_rpc_test_methods, subtract}),
        ?assertEqual(
            {error, registry_full},
            json_rpc:register(<<"cap_c">>, {json_rpc_test_methods, subtract})
        ),
        %% Replacing an existing entry does not add one, so it is still
        %% allowed at the cap.
        ?assertEqual(ok, json_rpc:register(<<"cap_a">>, {json_rpc_test_methods, sum})),
        ok = json_rpc:unregister(<<"cap_a">>),
        ok = json_rpc:unregister(<<"cap_b">>)
    end).

unregister_unknown_returns_not_found(_Config) ->
    ?assertEqual({error, not_found}, json_rpc:unregister(<<"never_registered">>)).

%% Removing the built-in used to succeed and left the server without
%% rpc.discover until the registry restarted.
unregister_builtin_is_refused(_Config) ->
    ?assertEqual(
        {error, {reserved_method_name, <<"rpc.discover">>}},
        json_rpc:unregister(<<"rpc.discover">>)
    ),
    ?assertMatch({ok, _Handler}, json_rpc_methods:lookup(<<"rpc.discover">>)).

%% The table is `protected', so validation and the cap cannot be bypassed by
%% writing to it directly.
table_is_write_protected(_Config) ->
    Self = self(),
    _Pid = spawn(fun() ->
        Result =
            try ets:insert(json_rpc_methods, {<<"rogue">>, {mfa, erlang, self, 1}}) of
                Inserted -> {ok, Inserted}
            catch
                Class:Reason -> {Class, Reason}
            end,
        Self ! {result, Result}
    end),
    receive
        {result, Result} -> ?assertMatch({error, badarg}, Result)
    after 5000 ->
        erlang:error(writer_did_not_finish)
    end,
    ?assertEqual(not_found, json_rpc_methods:lookup(<<"rogue">>)).

seeded_methods_registered_at_start(_Config) ->
    ?assertEqual(
        {ok, {mfa, json_rpc_test_methods, subtract, 1}},
        json_rpc_methods:lookup(<<"seeded">>)
    ).

%% A registry crash drops every runtime registration. Methods declared in the
%% `methods' environment key are re-seeded on restart, so a statically
%% declared method set heals itself.
seeded_methods_survive_registry_crash(_Config) ->
    ok = json_rpc:register(<<"runtime_only">>, {json_rpc_test_methods, subtract}),
    Pid = whereis(json_rpc_methods),
    MonitorRef = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MonitorRef, process, Pid, _Reason} -> ok
    after 5000 ->
        erlang:error(registry_did_not_die)
    end,
    ok = json_rpc_test_support:wait_until(
        fun() ->
            case whereis(json_rpc_methods) of
                undefined -> false;
                New -> New =/= Pid andalso json_rpc_methods:lookup(<<"seeded">>) =/= not_found
            end
        end,
        5000
    ),
    ?assertMatch({ok, _Handler}, json_rpc_methods:lookup(<<"seeded">>)),
    ?assertEqual(not_found, json_rpc_methods:lookup(<<"runtime_only">>)),
    %% rest_for_one takes the listener down with the registry, so put the
    %% suite's method set back for whatever runs next.
    json_rpc_test_support:register_methods().
