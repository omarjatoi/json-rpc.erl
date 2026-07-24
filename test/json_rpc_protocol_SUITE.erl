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

-module(json_rpc_protocol_SUITE).

%% Specification conformance at the dispatcher level, with no transport in
%% the way. Cases assert on Erlang terms, so a failure points at the protocol
%% logic rather than at HTTP framing.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([
    positional_params/1,
    named_params/1,
    notification/1,
    notification_to_missing_method/1,
    method_not_found/1,
    reserved_method_namespace/1,
    missing_jsonrpc_member/1,
    wrong_jsonrpc_version/1,
    missing_method_member/1,
    non_string_method/1,
    empty_method/1,
    non_structured_params/1,
    explicit_null_id_is_a_call/1,
    id_types/1,
    invalid_id_types/1,
    result_may_be_null/1,
    batch_mixed/1,
    batch_all_notifications/1,
    batch_empty/1,
    batch_invalid_elements/1,
    batch_preserves_element_ids/1,
    batch_preserves_order/1,
    batch_over_limit/1,
    batch_not_a_list_of_objects/1,
    payload_not_an_object/1,
    empty_object/1,
    handler_crash/1,
    handler_exit/1,
    handler_bare_throw/1,
    handler_thrown_legacy_error/1,
    handler_thrown_error_object/1,
    handler_reserved_code_substituted/1,
    handler_server_error_code_allowed/1,
    handler_returned_errors/1,
    handler_ok_tuple/1,
    handler_timeout/1,
    batch_timeout_is_not_shared/1,
    context_is_passed_to_arity_two/1
]).

-define(PORT, 18081).
-define(VERSION, <<"2.0">>).

all() ->
    [
        {group, single},
        {group, batch},
        {group, handlers}
    ].

groups() ->
    [
        {single, [], [
            positional_params,
            named_params,
            notification,
            notification_to_missing_method,
            method_not_found,
            reserved_method_namespace,
            missing_jsonrpc_member,
            wrong_jsonrpc_version,
            missing_method_member,
            non_string_method,
            empty_method,
            non_structured_params,
            explicit_null_id_is_a_call,
            id_types,
            invalid_id_types,
            result_may_be_null,
            payload_not_an_object,
            empty_object
        ]},
        {batch, [], [
            batch_mixed,
            batch_all_notifications,
            batch_empty,
            batch_invalid_elements,
            batch_preserves_element_ids,
            batch_preserves_order,
            batch_over_limit,
            batch_not_a_list_of_objects
        ]},
        {handlers, [], [
            handler_crash,
            handler_exit,
            handler_bare_throw,
            handler_thrown_legacy_error,
            handler_thrown_error_object,
            handler_reserved_code_substituted,
            handler_server_error_code_allowed,
            handler_returned_errors,
            handler_ok_tuple,
            handler_timeout,
            batch_timeout_is_not_shared,
            context_is_passed_to_arity_two
        ]}
    ].

init_per_suite(Config) ->
    ok = json_rpc_test_support:start_app(?PORT, #{}),
    Config.

end_per_suite(_Config) ->
    ok = json_rpc_test_support:stop_app().

%%% Helpers

call(Method, Params, Id) ->
    json_rpc:dispatch(#{
        <<"jsonrpc">> => ?VERSION, <<"method">> => Method, <<"params">> => Params, <<"id">> => Id
    }).

request(Method, Params, Id) ->
    #{<<"jsonrpc">> => ?VERSION, <<"method">> => Method, <<"params">> => Params, <<"id">> => Id}.

notify(Method, Params) ->
    #{<<"jsonrpc">> => ?VERSION, <<"method">> => Method, <<"params">> => Params}.

ok_response(Id, Result) ->
    #{jsonrpc => ?VERSION, id => Id, result => Result}.

error_response(Id, Code, Message) ->
    #{jsonrpc => ?VERSION, id => Id, error => #{code => Code, message => Message}}.

invalid_request(Id) ->
    error_response(Id, -32600, <<"Invalid Request">>).

%%% Single requests

positional_params(_Config) ->
    ?assertEqual(ok_response(1, 19), call(<<"subtract">>, [42, 23], 1)),
    ?assertEqual(ok_response(2, -19), call(<<"subtract">>, [23, 42], 2)).

named_params(_Config) ->
    %% `echo' returns params verbatim, which proves a by-name object reaches
    %% the handler unchanged.
    Params = #{<<"a">> => 1, <<"b">> => 2},
    ?assertEqual(ok_response(1, Params), call(<<"echo">>, Params, 1)).

notification(_Config) ->
    ?assertEqual(no_response, json_rpc:dispatch(notify(<<"update">>, [1, 2, 3]))).

%% A Notification is owed no Response even when the method does not exist.
notification_to_missing_method(_Config) ->
    ?assertEqual(no_response, json_rpc:dispatch(notify(<<"no_such_method">>, []))).

method_not_found(_Config) ->
    ?assertEqual(
        error_response(<<"1">>, -32601, <<"Method not found">>),
        call(<<"foobar">>, [], <<"1">>)
    ).

%% `rpc.'-prefixed names cannot be registered, so they can never resolve.
reserved_method_namespace(_Config) ->
    ?assertEqual(
        error_response(<<"r1">>, -32601, <<"Method not found">>),
        call(<<"rpc.foo">>, [], <<"r1">>)
    ).

missing_jsonrpc_member(_Config) ->
    ?assertEqual(
        invalid_request(1),
        json_rpc:dispatch(#{<<"method">> => <<"subtract">>, <<"id">> => 1})
    ).

wrong_jsonrpc_version(_Config) ->
    ?assertEqual(
        invalid_request(1),
        json_rpc:dispatch(#{
            <<"jsonrpc">> => <<"1.0">>, <<"method">> => <<"subtract">>, <<"id">> => 1
        })
    ).

missing_method_member(_Config) ->
    ?assertEqual(
        invalid_request(<<"m">>),
        json_rpc:dispatch(#{<<"jsonrpc">> => ?VERSION, <<"id">> => <<"m">>})
    ).

non_string_method(_Config) ->
    ?assertEqual(
        invalid_request(1),
        json_rpc:dispatch(#{<<"jsonrpc">> => ?VERSION, <<"method">> => 42, <<"id">> => 1})
    ).

empty_method(_Config) ->
    ?assertEqual(
        invalid_request(1),
        json_rpc:dispatch(#{<<"jsonrpc">> => ?VERSION, <<"method">> => <<>>, <<"id">> => 1})
    ).

%% `params' must be structured. A scalar makes the whole Request invalid.
non_structured_params(_Config) ->
    ?assertEqual(
        error_response(<<"p1">>, -32602, <<"Invalid params">>),
        call(<<"subtract">>, 42, <<"p1">>)
    ).

%% An explicit `"id": null' is a call, not a Notification: the member is
%% present, and Null is a permitted id type.
explicit_null_id_is_a_call(_Config) ->
    ?assertEqual(ok_response(null, 19), call(<<"subtract">>, [42, 23], null)).

id_types(_Config) ->
    ?assertEqual(ok_response(<<"str">>, 19), call(<<"subtract">>, [42, 23], <<"str">>)),
    ?assertEqual(ok_response(7, 19), call(<<"subtract">>, [42, 23], 7)),
    ?assertEqual(ok_response(-7, 19), call(<<"subtract">>, [42, 23], -7)),
    %% The spec says numbers SHOULD NOT have fractional parts, not MUST NOT,
    %% so a float id is echoed rather than rejected.
    ?assertEqual(ok_response(1.5, 19), call(<<"subtract">>, [42, 23], 1.5)).

%% Booleans, arrays, and objects are not permitted id types. There is nothing
%% safe to echo, so the error carries null.
invalid_id_types(_Config) ->
    lists:foreach(
        fun(Id) -> ?assertEqual(invalid_request(null), call(<<"subtract">>, [1, 2], Id)) end,
        [true, false, [1, 2], #{<<"a">> => 1}]
    ).

%% `result' must be present on success even when the value is null.
result_may_be_null(_Config) ->
    Response = call(<<"echo">>, [null], 1),
    ?assertEqual(ok_response(1, [null]), Response),
    ?assert(maps:is_key(result, Response)).

payload_not_an_object(_Config) ->
    lists:foreach(
        fun(Payload) -> ?assertEqual(invalid_request(null), json_rpc:dispatch(Payload)) end,
        [<<"a string">>, 42, true, null]
    ).

empty_object(_Config) ->
    ?assertEqual(invalid_request(null), json_rpc:dispatch(#{})).

%%% Batches

batch_mixed(_Config) ->
    Batch = [
        request(<<"sum">>, [1, 2, 4], <<"1">>),
        notify(<<"notify_hello">>, [7]),
        request(<<"subtract">>, [42, 23], <<"2">>),
        request(<<"foo">>, [], <<"9">>)
    ],
    ?assertEqual(
        [
            ok_response(<<"1">>, 7),
            ok_response(<<"2">>, 19),
            error_response(<<"9">>, -32601, <<"Method not found">>)
        ],
        json_rpc:dispatch(Batch)
    ).

batch_all_notifications(_Config) ->
    Batch = [notify(<<"notify_sum">>, [1, 2, 4]), notify(<<"notify_hello">>, [7])],
    ?assertEqual(no_response, json_rpc:dispatch(Batch)).

batch_empty(_Config) ->
    ?assertEqual(invalid_request(null), json_rpc:dispatch([])).

%% A batch of nothing but junk still answers one error per element.
batch_invalid_elements(_Config) ->
    ?assertEqual(
        [invalid_request(null), invalid_request(null), invalid_request(null)],
        json_rpc:dispatch([1, 2, 3])
    ).

%% An element whose envelope is malformed but whose id is usable must have
%% that id echoed, or the client cannot tell which call failed.
batch_preserves_element_ids(_Config) ->
    Batch = [
        #{<<"jsonrpc">> => <<"1.0">>, <<"method">> => <<"subtract">>, <<"id">> => <<"v">>},
        #{<<"jsonrpc">> => ?VERSION, <<"id">> => <<"m">>},
        <<"junk">>,
        request(<<"subtract">>, [42, 23], <<"ok">>)
    ],
    ?assertEqual(
        [
            invalid_request(<<"v">>),
            invalid_request(<<"m">>),
            invalid_request(null),
            ok_response(<<"ok">>, 19)
        ],
        json_rpc:dispatch(Batch)
    ).

%% Elements run concurrently, so ordering has to be restored explicitly.
%% Sleeping in decreasing order makes an order-preserving bug visible.
batch_preserves_order(_Config) ->
    Batch = [
        request(<<"slow">>, [180], 1),
        request(<<"slow">>, [120], 2),
        request(<<"slow">>, [60], 3),
        request(<<"subtract">>, [42, 23], 4)
    ],
    ?assertEqual(
        [
            ok_response(1, <<"done">>),
            ok_response(2, <<"done">>),
            ok_response(3, <<"done">>),
            ok_response(4, 19)
        ],
        json_rpc:dispatch(Batch)
    ).

%% Oversized batches are refused as one error, not element by element.
batch_over_limit(_Config) ->
    json_rpc_test_support:with_env(max_batch_size, 3, fun() ->
        Batch = [request(<<"subtract">>, [1, 1], N) || N <- lists:seq(1, 4)],
        Response = json_rpc:dispatch(Batch),
        ?assertMatch(#{jsonrpc := ?VERSION, id := null, error := #{code := -32600}}, Response),
        ?assertMatch(#{error := #{data := #{max_batch_size := 3}}}, Response),
        %% Exactly at the cap is still accepted.
        AtCap = [request(<<"subtract">>, [1, 1], N) || N <- lists:seq(1, 3)],
        ?assertEqual(3, length(json_rpc:dispatch(AtCap)))
    end).

batch_not_a_list_of_objects(_Config) ->
    ?assertEqual([invalid_request(null)], json_rpc:dispatch([[]])).

%%% Handler behaviour

handler_crash(_Config) ->
    ?assertEqual(
        error_response(1, -32603, <<"Internal error">>),
        call(<<"crash">>, [], 1)
    ).

%% `exit/1' must be contained exactly like `error/1'.
handler_exit(_Config) ->
    ?assertEqual(
        error_response(1, -32603, <<"Internal error">>),
        call(<<"crash_exit">>, [], 1)
    ).

%% A bare throw is a handler bug, not a protocol error.
handler_bare_throw(_Config) ->
    ?assertEqual(
        error_response(1, -32603, <<"Internal error">>),
        call(<<"crash_throw">>, [], 1)
    ).

handler_thrown_legacy_error(_Config) ->
    ?assertEqual(
        #{
            jsonrpc => ?VERSION,
            id => <<"e1">>,
            error => #{
                code => -1, message => <<"bad arg">>, data => #{<<"detail">> => <<"oops">>}
            }
        },
        call(<<"throw_error">>, [], <<"e1">>)
    ).

handler_thrown_error_object(_Config) ->
    ?assertEqual(
        error_response(<<"e2">>, -1, <<"bad arg">>),
        call(<<"throw_error_object">>, [], <<"e2">>)
    ).

%% A handler must not be able to claim a framework-owned code.
handler_reserved_code_substituted(_Config) ->
    ?assertEqual(
        error_response(<<"r2">>, -32603, <<"Internal error">>),
        call(<<"throw_reserved">>, [], <<"r2">>)
    ).

%% -32099..-32000 is delegated to the implementation, so it passes through.
handler_server_error_code_allowed(_Config) ->
    ?assertEqual(
        error_response(<<"s1">>, -32000, <<"upstream unavailable">>),
        call(<<"throw_server_error">>, [], <<"s1">>)
    ).

handler_returned_errors(_Config) ->
    ?assertEqual(
        error_response(1, -32050, <<"pair">>),
        call(<<"return_error_pair">>, [], 1)
    ),
    ?assertEqual(
        #{
            jsonrpc => ?VERSION,
            id => 2,
            error => #{code => -32051, message => <<"triple">>, data => #{extra => true}}
        },
        call(<<"return_error_triple">>, [], 2)
    ),
    ?assertEqual(
        error_response(3, -32052, <<"object">>),
        call(<<"return_error_object">>, [], 3)
    ).

handler_ok_tuple(_Config) ->
    ?assertEqual(ok_response(1, 42), call(<<"return_ok_tuple">>, [], 1)).

handler_timeout(_Config) ->
    json_rpc_test_support:with_env(handler_timeout_ms, 150, fun() ->
        ?assertEqual(
            #{
                jsonrpc => ?VERSION,
                id => 7,
                error => #{
                    code => -32603, message => <<"Internal error">>, data => #{reason => timeout}
                }
            },
            call(<<"slow">>, [2000], 7)
        )
    end).

%% Batch elements run concurrently under one deadline, so a slow element must
%% not consume the budget its siblings need. Four 200ms calls under a 600ms
%% deadline only fit if they overlap.
batch_timeout_is_not_shared(_Config) ->
    json_rpc_test_support:with_env(handler_timeout_ms, 600, fun() ->
        Batch = [request(<<"slow">>, [200], N) || N <- lists:seq(1, 4)],
        Responses = json_rpc:dispatch(Batch),
        ?assertEqual(4, length(Responses)),
        lists:foreach(
            fun(Response) -> ?assertMatch(#{result := <<"done">>}, Response) end,
            Responses
        )
    end).

context_is_passed_to_arity_two(_Config) ->
    ?assertMatch(
        #{result := #{transport := internal, has_connection_pid := true, has_request_id := false}},
        call(<<"context">>, [], 1)
    ).
