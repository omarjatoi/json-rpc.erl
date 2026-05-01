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

-module(json_rpc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% HTTP test cases.
-export([
    test_http_positional_params/1,
    test_http_notification/1,
    test_http_method_not_found/1,
    test_http_reserved_method_name/1,
    test_http_invalid_params_type/1,
    test_http_handler_throws_jsonrpc_error/1,
    test_http_handler_throws_reserved_error/1,
    test_http_explicit_null_id_is_call/1,
    test_http_invalid_id_boolean/1,
    test_http_invalid_id_array/1,
    test_http_invalid_id_object/1,
    test_http_batch_mixed/1,
    test_http_batch_element_id_preserved_on_envelope_error/1,
    test_http_all_notification_batch/1,
    test_http_empty_batch/1,
    test_http_malformed_json/1,
    test_http_oversize_body/1,
    test_http_method_not_allowed/1,
    test_http_unsupported_media_type/1,
    test_http_handler_timeout/1,
    test_http_handler_crash_isolation/1,
    test_http_handler_exit_isolation/1
]).

%% WebSocket test cases.
-export([
    test_ws_call/1,
    test_ws_notification/1,
    test_ws_batch/1,
    test_ws_malformed_json/1,
    test_ws_handler_timeout/1,
    test_ws_handler_crash_isolation/1,
    test_ws_handler_exit_isolation/1,
    test_ws_subprotocol_offered/1,
    test_ws_oversize_frame/1,
    test_ws_binary_frame/1,
    test_ws_push/1,
    test_ws_publish/1,
    test_ws_subscriber_cleanup_on_close/1,
    test_ws_drain_on_shutdown/1
]).

%% Registry test cases.
-export([
    test_rpc_discover/1,
    test_register_rpc_reserved/1,
    test_register_invalid_handler/1,
    test_register_full/1,
    test_methods_table_is_protected/1
]).

%% Application lifecycle test cases.
-export([
    test_app_lifecycle/1,
    test_app_drain/1
]).

%% Used as a placeholder MFA for register/unregister tests.
-export([dummy_handler/1]).

-define(PORT, 18080).
-define(HOST, "localhost").
-define(JSON_HEADERS, [
    {<<"content-type">>, <<"application/json">>},
    {<<"accept">>, <<"application/json">>}
]).

%%% Common Test callbacks

all() ->
    [
        test_http_positional_params,
        test_http_notification,
        test_http_method_not_found,
        test_http_reserved_method_name,
        test_http_invalid_params_type,
        test_http_handler_throws_jsonrpc_error,
        test_http_handler_throws_reserved_error,
        test_http_explicit_null_id_is_call,
        test_http_invalid_id_boolean,
        test_http_invalid_id_array,
        test_http_invalid_id_object,
        test_http_batch_mixed,
        test_http_batch_element_id_preserved_on_envelope_error,
        test_http_all_notification_batch,
        test_http_empty_batch,
        test_http_malformed_json,
        test_http_oversize_body,
        test_http_method_not_allowed,
        test_http_unsupported_media_type,
        test_http_handler_timeout,
        test_http_handler_crash_isolation,
        test_http_handler_exit_isolation,
        test_ws_call,
        test_ws_notification,
        test_ws_batch,
        test_ws_malformed_json,
        test_ws_handler_timeout,
        test_ws_handler_crash_isolation,
        test_ws_handler_exit_isolation,
        test_ws_subprotocol_offered,
        test_ws_oversize_frame,
        test_ws_binary_frame,
        test_ws_push,
        test_ws_publish,
        test_ws_subscriber_cleanup_on_close,
        test_ws_drain_on_shutdown,
        test_rpc_discover,
        test_register_rpc_reserved,
        test_register_invalid_handler,
        test_register_full,
        test_methods_table_is_protected,
        test_app_drain,
        %% Lifecycle tests must run last since they stop the application.
        test_app_lifecycle
    ].

init_per_suite(Config) ->
    %% Load the app first so set_env values aren't overwritten by the
    %% defaults baked into json_rpc.app.src on subsequent load.
    _ = application:load(json_rpc),
    application:set_env(json_rpc, port, ?PORT),
    %% Use a small body cap so the oversize-body test has something to bump
    %% into without us shipping a multi-MB payload through CT.
    application:set_env(json_rpc, max_body_bytes, 4096),
    {ok, _Apps} = application:ensure_all_started(json_rpc),
    {ok, _GunApps} = application:ensure_all_started(gun),
    register_methods(),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(json_rpc),
    ok.

init_per_testcase(TestCase, Config0) ->
    Config1 = maybe_lower_timeout(TestCase, Config0),
    Config2 = maybe_lower_ws_frame(TestCase, Config1),
    case needs_conn(TestCase) of
        false ->
            Config2;
        true ->
            {ok, ConnPid} = gun:open(?HOST, ?PORT, #{
                transport => tcp,
                protocols => [http],
                retry => 0
            }),
            {ok, http} = gun:await_up(ConnPid, 5000),
            [{conn, ConnPid} | Config2]
    end.

end_per_testcase(_TestCase, Config) ->
    case ?config(conn, Config) of
        undefined -> ok;
        ConnPid -> gun:close(ConnPid)
    end,
    case ?config(saved_request_timeout_ms, Config) of
        undefined ->
            ok;
        {ok, Prev} ->
            application:set_env(json_rpc, request_timeout_ms, Prev);
        unset ->
            application:unset_env(json_rpc, request_timeout_ms)
    end,
    case ?config(saved_ws_max_frame_bytes, Config) of
        undefined ->
            ok;
        {ok, PrevWs} ->
            application:set_env(json_rpc, ws_max_frame_bytes, PrevWs);
        unset ->
            application:unset_env(json_rpc, ws_max_frame_bytes)
    end,
    ok.

%% The handler-timeout cases need a small request_timeout_ms so the test
%% can sleep past it without slowing the suite. Save and restore the
%% original value so test_app_drain (which uses the slow handler at 800ms)
%% and other tests aren't affected.
maybe_lower_timeout(TestCase, Config) when
    TestCase =:= test_http_handler_timeout;
    TestCase =:= test_ws_handler_timeout
->
    Saved =
        case application:get_env(json_rpc, request_timeout_ms) of
            {ok, V} -> {ok, V};
            undefined -> unset
        end,
    application:set_env(json_rpc, request_timeout_ms, 200),
    [{saved_request_timeout_ms, Saved} | Config];
maybe_lower_timeout(_TestCase, Config) ->
    Config.

%% The oversize-WS-frame test needs a tiny ws_max_frame_bytes so we don't
%% have to ship a multi-MB frame through CT. Saved/restored per testcase
%% so other WS tests aren't affected.
maybe_lower_ws_frame(test_ws_oversize_frame, Config) ->
    Saved =
        case application:get_env(json_rpc, ws_max_frame_bytes) of
            {ok, V} -> {ok, V};
            undefined -> unset
        end,
    application:set_env(json_rpc, ws_max_frame_bytes, 4096),
    [{saved_ws_max_frame_bytes, Saved} | Config];
maybe_lower_ws_frame(_TestCase, Config) ->
    Config.

%% Test cases that don't open a gun connection in init_per_testcase. The
%% lifecycle test owns the application start/stop itself; the registry
%% tests don't talk over the wire.
needs_conn(test_app_lifecycle) -> false;
needs_conn(test_register_rpc_reserved) -> false;
needs_conn(test_register_invalid_handler) -> false;
needs_conn(test_register_full) -> false;
needs_conn(test_methods_table_is_protected) -> false;
needs_conn(test_ws_drain_on_shutdown) -> false;
needs_conn(_) -> true.

%%% Method registration

register_methods() ->
    Methods = [
        {<<"subtract">>, {json_rpc_test_methods, subtract}},
        {<<"sum">>, {json_rpc_test_methods, sum}},
        {<<"get_data">>, {json_rpc_test_methods, get_data}},
        {<<"update">>, {json_rpc_test_methods, update}},
        {<<"notify_sum">>, {json_rpc_test_methods, notify_sum}},
        {<<"notify_hello">>, {json_rpc_test_methods, notify_hello}},
        {<<"throw_error">>, {json_rpc_test_methods, throw_error}},
        {<<"throw_reserved_error">>, {json_rpc_test_methods, throw_reserved_error}},
        {<<"slow">>, {json_rpc_test_methods, slow}},
        {<<"crash">>, {json_rpc_test_methods, crash}},
        {<<"crash_exit">>, {json_rpc_test_methods, crash_exit}}
    ],
    lists:foreach(
        fun({Name, Handler}) -> ok = json_rpc_methods:register_method(Name, Handler) end,
        Methods
    ).

%%% HTTP helpers

%% POST a JSON-RPC payload (already encoded as a binary) to /rpc and return
%% {Status, Headers, Body} where Body is the raw response body.
raw_post(Conn, Payload) ->
    raw_post(Conn, Payload, ?JSON_HEADERS).

raw_post(Conn, Payload, Headers) ->
    StreamRef = gun:post(Conn, "/rpc", Headers, Payload),
    case gun:await(Conn, StreamRef, 5000) of
        {response, fin, Status, RespHeaders} ->
            {Status, RespHeaders, <<>>};
        {response, nofin, Status, RespHeaders} ->
            {ok, Body} = gun:await_body(Conn, StreamRef, 5000),
            {Status, RespHeaders, Body}
    end.

%% Issue a JSON-RPC call/notification/batch as an Erlang term, return the
%% decoded JSON-RPC envelope (or no_response for 204s).
rpc_call(Conn, Term) ->
    Payload = jiffy:encode(Term),
    case raw_post(Conn, Payload) of
        {204, _Hs, _} ->
            no_response;
        {200, _Hs, Body} ->
            jiffy:decode(Body, [return_maps])
    end.

%%% HTTP test cases

test_http_positional_params(Config) ->
    Conn = ?config(conn, Config),
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => 1},
        rpc_call(Conn, #{
            jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => 1
        })
    ),
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => -19, <<"id">> => 2},
        rpc_call(Conn, #{
            jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [23, 42], id => 2
        })
    ).

test_http_notification(Config) ->
    Conn = ?config(conn, Config),
    %% No id member -> notification -> 204 No Content, empty body.
    Payload = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"update">>, params => [1, 2, 3]
    }),
    ?assertEqual({204, ignore, <<>>}, normalize(raw_post(Conn, Payload))).

test_http_method_not_found(Config) ->
    Conn = ?config(conn, Config),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32601, <<"message">> => <<"Method not found">>},
            <<"id">> => <<"1">>
        },
        rpc_call(Conn, #{
            jsonrpc => <<"2.0">>, method => <<"foobar">>, params => [], id => <<"1">>
        })
    ).

test_http_reserved_method_name(Config) ->
    Conn = ?config(conn, Config),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32601, <<"message">> => <<"Method not found">>},
            <<"id">> => <<"r1">>
        },
        rpc_call(Conn, #{
            jsonrpc => <<"2.0">>, method => <<"rpc.foo">>, params => [], id => <<"r1">>
        })
    ).

test_http_invalid_params_type(Config) ->
    Conn = ?config(conn, Config),
    %% "params": 42 is neither array nor object -> -32602.
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": 42, \"id\": \"p1\"}">>,
    {200, _Hs, Body} = raw_post(Conn, Payload),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32602, <<"message">> => <<"Invalid params">>},
            <<"id">> => <<"p1">>
        },
        jiffy:decode(Body, [return_maps])
    ).

test_http_handler_throws_jsonrpc_error(Config) ->
    Conn = ?config(conn, Config),
    %% throw_error/1 uses an application-defined code (not in the reserved
    %% range -32768..-32000) so the dispatcher passes it through verbatim.
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{
                <<"code">> => -1,
                <<"message">> => <<"bad arg">>,
                <<"data">> => #{<<"detail">> => <<"oops">>}
            },
            <<"id">> => <<"e1">>
        },
        rpc_call(Conn, #{
            jsonrpc => <<"2.0">>, method => <<"throw_error">>, params => [], id => <<"e1">>
        })
    ).

test_http_handler_throws_reserved_error(Config) ->
    Conn = ?config(conn, Config),
    %% A handler throwing a code in the JSON-RPC reserved range (-32768..-32000)
    %% must not be able to impersonate a framework error. The dispatcher
    %% substitutes -32603 Internal error.
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32603, <<"message">> => <<"Internal error">>},
            <<"id">> => <<"r2">>
        },
        rpc_call(Conn, #{
            jsonrpc => <<"2.0">>, method => <<"throw_reserved_error">>,
            params => [], id => <<"r2">>
        })
    ).

test_http_explicit_null_id_is_call(Config) ->
    Conn = ?config(conn, Config),
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42,23], \"id\": null}">>,
    {200, _Hs, Body} = raw_post(Conn, Payload),
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => null},
        jiffy:decode(Body, [return_maps])
    ).

test_http_invalid_id_boolean(Config) ->
    Conn = ?config(conn, Config),
    %% Per spec, id MUST be String, Number, or Null. Boolean is invalid.
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [1,2], \"id\": true}">>,
    {200, _Hs, Body} = raw_post(Conn, Payload),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
            <<"id">> => null
        },
        jiffy:decode(Body, [return_maps])
    ).

test_http_invalid_id_array(Config) ->
    Conn = ?config(conn, Config),
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [1,2], \"id\": [1,2]}">>,
    {200, _Hs, Body} = raw_post(Conn, Payload),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
            <<"id">> => null
        },
        jiffy:decode(Body, [return_maps])
    ).

test_http_invalid_id_object(Config) ->
    Conn = ?config(conn, Config),
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [1,2], \"id\": {}}">>,
    {200, _Hs, Body} = raw_post(Conn, Payload),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
            <<"id">> => null
        },
        jiffy:decode(Body, [return_maps])
    ).

test_http_batch_mixed(Config) ->
    Conn = ?config(conn, Config),
    Batch = [
        #{jsonrpc => <<"2.0">>, method => <<"sum">>, params => [1, 2, 4], id => <<"1">>},
        #{jsonrpc => <<"2.0">>, method => <<"notify_hello">>, params => [7]},
        #{jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => <<"2">>},
        #{jsonrpc => <<"2.0">>, method => <<"foo">>, params => [], id => <<"9">>}
    ],
    Decoded = rpc_call(Conn, Batch),
    ?assert(is_list(Decoded)),
    Expected = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 7, <<"id">> => <<"1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => <<"2">>},
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32601, <<"message">> => <<"Method not found">>},
            <<"id">> => <<"9">>
        }
    ],
    ?assertEqual(lists:sort(Expected), lists:sort(Decoded)).

%% Per-batch-element envelope errors must preserve the original `id' when one
%% was present and well-typed. The dispatcher used to require a fully valid
%% envelope before extracting the id, so a batch element with a valid id but
%% (e.g.) wrong jsonrpc version came back as id: null and clients couldn't
%% correlate the error to its request.
test_http_batch_element_id_preserved_on_envelope_error(Config) ->
    Conn = ?config(conn, Config),
    Batch = [
        %% Wrong jsonrpc version, but id is present and valid -> echo id.
        #{jsonrpc => <<"1.0">>, method => <<"subtract">>, params => [1, 2], id => <<"v">>},
        %% Missing method, valid id -> echo id.
        #{jsonrpc => <<"2.0">>, id => <<"m">>},
        %% Completely garbage element (not an object) -> id: null (cannot
        %% extract one).
        <<"junk">>,
        %% Sanity: a normal call still works alongside the broken elements.
        #{jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => <<"ok">>}
    ],
    Decoded = rpc_call(Conn, Batch),
    ?assert(is_list(Decoded)),
    Expected = [
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
            <<"id">> => <<"v">>
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
            <<"id">> => <<"m">>
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
            <<"id">> => null
        },
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => <<"ok">>}
    ],
    ?assertEqual(lists:sort(Expected), lists:sort(Decoded)).

test_http_all_notification_batch(Config) ->
    Conn = ?config(conn, Config),
    Batch = [
        #{jsonrpc => <<"2.0">>, method => <<"notify_sum">>, params => [1, 2, 4]},
        #{jsonrpc => <<"2.0">>, method => <<"notify_hello">>, params => [7]}
    ],
    Payload = jiffy:encode(Batch),
    ?assertEqual({204, ignore, <<>>}, normalize(raw_post(Conn, Payload))).

test_http_empty_batch(Config) ->
    Conn = ?config(conn, Config),
    {200, _Hs, Body} = raw_post(Conn, <<"[]">>),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
            <<"id">> => null
        },
        jiffy:decode(Body, [return_maps])
    ).

test_http_malformed_json(Config) ->
    Conn = ?config(conn, Config),
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": \"foo\", \"params\": \"bar\", \"baz]">>,
    {200, _Hs, Body} = raw_post(Conn, Payload),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32700, <<"message">> => <<"Parse error">>},
            <<"id">> => null
        },
        jiffy:decode(Body, [return_maps])
    ).

test_http_oversize_body(Config) ->
    Conn = ?config(conn, Config),
    %% Suite-level max_body_bytes is set to 4096; send 5 KiB of garbage.
    Big = binary:copy(<<"x">>, 5120),
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": \"x\", \"params\": [\"", Big/binary, "\"]}">>,
    {Status, _Hs, Body} = raw_post(Conn, Payload),
    ?assertEqual(413, Status),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
            <<"id">> => null
        },
        jiffy:decode(Body, [return_maps])
    ).

test_http_method_not_allowed(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = gun:get(Conn, "/rpc", []),
    {response, fin, 405, Headers} = gun:await(Conn, StreamRef, 5000),
    %% The handler must advertise POST.
    Allow = proplists:get_value(<<"allow">>, Headers, undefined),
    ?assertEqual(<<"POST">>, Allow).

test_http_unsupported_media_type(Config) ->
    Conn = ?config(conn, Config),
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [1,2], \"id\": 1}">>,
    Headers = [{<<"content-type">>, <<"text/plain">>}],
    {Status, _Hs, _Body} = raw_post(Conn, Payload, Headers),
    ?assertEqual(415, Status).

%% A handler that sleeps past request_timeout_ms (lowered to 200ms in
%% init_per_testcase) must produce -32603 with data.reason = timeout, and
%% the HTTP status must remain 200.
test_http_handler_timeout(Config) ->
    Conn = ?config(conn, Config),
    Payload = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"slow">>, params => [1000], id => 7
    }),
    {Status, _Hs, Body} = raw_post(Conn, Payload),
    ?assertEqual(200, Status),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{
                <<"code">> => -32603,
                <<"message">> => <<"Internal error">>,
                <<"data">> => #{<<"reason">> => <<"timeout">>}
            },
            <<"id">> => 7
        },
        jiffy:decode(Body, [return_maps])
    ).

%% A handler that crashes must yield a -32603 reply, and the HTTP keepalive
%% connection must stay healthy enough for an immediate follow-up call to
%% succeed on the same gun pid.
test_http_handler_crash_isolation(Config) ->
    Conn = ?config(conn, Config),
    Reply1 = rpc_call(Conn, #{
        jsonrpc => <<"2.0">>, method => <<"crash">>, params => [], id => 1
    }),
    ?assertMatch(
        #{
            <<"jsonrpc">> := <<"2.0">>,
            <<"error">> := #{<<"code">> := -32603, <<"message">> := <<"Internal error">>},
            <<"id">> := 1
        },
        Reply1
    ),
    %% Same connection, immediately, must work.
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => 2},
        rpc_call(Conn, #{
            jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => 2
        })
    ).

%% A handler that calls `exit/1' is not caught by the dispatcher's narrowed
%% try/catch — the worker process dies, json_rpc_worker returns
%% `{error, {crash, exit, _}}', and the HTTP handler must turn that into a
%% -32603 envelope while keeping the connection alive for follow-up calls.
test_http_handler_exit_isolation(Config) ->
    Conn = ?config(conn, Config),
    Reply1 = rpc_call(Conn, #{
        jsonrpc => <<"2.0">>, method => <<"crash_exit">>, params => [], id => 1
    }),
    ?assertMatch(
        #{
            <<"jsonrpc">> := <<"2.0">>,
            <<"error">> := #{<<"code">> := -32603, <<"message">> := <<"Internal error">>},
            <<"id">> := 1
        },
        Reply1
    ),
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => 2},
        rpc_call(Conn, #{
            jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => 2
        })
    ).

%%% WebSocket test cases

test_ws_call(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = ws_upgrade(Conn),
    Frame = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => 1
    }),
    gun:ws_send(Conn, StreamRef, {text, Frame}),
    {ws, {text, RespBin}} = gun:await(Conn, StreamRef, 5000),
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => 1},
        jiffy:decode(RespBin, [return_maps])
    ).

test_ws_notification(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = ws_upgrade(Conn),
    Frame = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"update">>, params => [1, 2, 3]
    }),
    gun:ws_send(Conn, StreamRef, {text, Frame}),
    %% No frame back. Use a short await; expect timeout.
    ?assertMatch({error, timeout}, gun:await(Conn, StreamRef, 300)).

test_ws_batch(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = ws_upgrade(Conn),
    Batch = [
        #{jsonrpc => <<"2.0">>, method => <<"sum">>, params => [1, 2, 4], id => <<"1">>},
        #{jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => <<"2">>}
    ],
    gun:ws_send(Conn, StreamRef, {text, jiffy:encode(Batch)}),
    {ws, {text, RespBin}} = gun:await(Conn, StreamRef, 5000),
    Decoded = jiffy:decode(RespBin, [return_maps]),
    Expected = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 7, <<"id">> => <<"1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => <<"2">>}
    ],
    ?assertEqual(lists:sort(Expected), lists:sort(Decoded)).

test_ws_malformed_json(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = ws_upgrade(Conn),
    gun:ws_send(Conn, StreamRef, {text, <<"{not valid json">>}),
    {ws, {text, RespBin}} = gun:await(Conn, StreamRef, 5000),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{<<"code">> => -32700, <<"message">> => <<"Parse error">>},
            <<"id">> => null
        },
        jiffy:decode(RespBin, [return_maps])
    ).

%% Same idea as test_http_handler_timeout but over WS — the WS handler
%% must echo the original request `id' so the client can correlate the
%% timeout error to the originating call.
test_ws_handler_timeout(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = ws_upgrade(Conn),
    Frame = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"slow">>, params => [1000], id => 1
    }),
    gun:ws_send(Conn, StreamRef, {text, Frame}),
    {ws, {text, RespBin}} = gun:await(Conn, StreamRef, 5000),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{
                <<"code">> => -32603,
                <<"message">> => <<"Internal error">>,
                <<"data">> => #{<<"reason">> => <<"timeout">>}
            },
            <<"id">> => 1
        },
        jiffy:decode(RespBin, [return_maps])
    ).

%% A crashing handler must yield -32603 and leave the WS stream healthy
%% enough to handle the very next frame.
test_ws_handler_crash_isolation(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = ws_upgrade(Conn),
    Frame1 = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"crash">>, params => [], id => 1
    }),
    gun:ws_send(Conn, StreamRef, {text, Frame1}),
    {ws, {text, RespBin1}} = gun:await(Conn, StreamRef, 5000),
    ?assertMatch(
        #{
            <<"jsonrpc">> := <<"2.0">>,
            <<"error">> := #{<<"code">> := -32603, <<"message">> := <<"Internal error">>},
            <<"id">> := 1
        },
        jiffy:decode(RespBin1, [return_maps])
    ),
    Frame2 = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => 2
    }),
    gun:ws_send(Conn, StreamRef, {text, Frame2}),
    {ws, {text, RespBin2}} = gun:await(Conn, StreamRef, 5000),
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => 2},
        jiffy:decode(RespBin2, [return_maps])
    ).

%% Same as test_ws_handler_crash_isolation but goes through the worker
%% isolation path instead of the dispatcher's catch — the handler calls
%% `exit/1', the worker dies, the WS handler reports -32603, and the
%% connection survives.
test_ws_handler_exit_isolation(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = ws_upgrade(Conn),
    Frame1 = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"crash_exit">>, params => [], id => 1
    }),
    gun:ws_send(Conn, StreamRef, {text, Frame1}),
    {ws, {text, RespBin1}} = gun:await(Conn, StreamRef, 5000),
    ?assertMatch(
        #{
            <<"jsonrpc">> := <<"2.0">>,
            <<"error">> := #{<<"code">> := -32603, <<"message">> := <<"Internal error">>},
            <<"id">> := 1
        },
        jiffy:decode(RespBin1, [return_maps])
    ),
    Frame2 = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => 2
    }),
    gun:ws_send(Conn, StreamRef, {text, Frame2}),
    {ws, {text, RespBin2}} = gun:await(Conn, StreamRef, 5000),
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => 2},
        jiffy:decode(RespBin2, [return_maps])
    ).

%% A client that advertises a `Sec-WebSocket-Protocol' must still be allowed
%% to upgrade. The server doesn't select any subprotocol, so the upgrade
%% response carries no `sec-websocket-protocol' header. JSON-RPC traffic
%% afterwards works normally.
test_ws_subprotocol_offered(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = gun:ws_upgrade(
        Conn, "/ws", [{<<"sec-websocket-protocol">>, <<"foo">>}]
    ),
    UpgradeHeaders =
        receive
            {gun_upgrade, Conn, StreamRef, [<<"websocket">>], Hs} ->
                Hs;
            {gun_response, Conn, StreamRef, _, Status, _Hs} ->
                erlang:error({ws_upgrade_failed, Status})
        after 5000 ->
            erlang:error(ws_upgrade_timeout)
        end,
    ?assertEqual(
        undefined,
        proplists:get_value(<<"sec-websocket-protocol">>, UpgradeHeaders)
    ),
    Frame = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => 1
    }),
    gun:ws_send(Conn, StreamRef, {text, Frame}),
    {ws, {text, RespBin}} = gun:await(Conn, StreamRef, 5000),
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => 19, <<"id">> => 1},
        jiffy:decode(RespBin, [return_maps])
    ).

%% A WS text frame larger than `ws_max_frame_bytes' (set to 4096 in
%% init_per_testcase) must not OOM the node — Cowboy closes the connection
%% with status 1009 (message too big) when the frame exceeds the limit.
test_ws_oversize_frame(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = ws_upgrade(Conn),
    Big = binary:copy(<<"x">>, 8192),
    gun:ws_send(Conn, StreamRef, {text, Big}),
    Result =
        receive
            {gun_ws, Conn, StreamRef, {close, Code, _Reason}} -> {close, Code};
            {gun_ws, Conn, StreamRef, close} -> {close, 1006};
            {gun_down, Conn, _Proto, _Reason, _Killed} -> down
        after 5000 ->
            erlang:error(no_close_received)
        end,
    ?assertMatch({close, 1009}, Result).

%% A binary frame on the WS endpoint must close the connection with
%% status 1003 (Unsupported Data) — JSON-RPC framing is text-only and
%% silently dropping the frame leaves the client hanging.
test_ws_binary_frame(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = ws_upgrade(Conn),
    gun:ws_send(Conn, StreamRef, {binary, <<"junk">>}),
    Result =
        receive
            {gun_ws, Conn, StreamRef, {close, Code, _Reason}} -> {close, Code}
        after 5000 ->
            erlang:error(no_close_received)
        end,
    ?assertMatch({close, 1003}, Result).

%% `json_rpc_ws:push/3' delivers a single Notification to one WS handler
%% pid. We open one WS connection, look up its handler pid via
%% ranch:procs/2, push, and assert the client receives the text frame.
test_ws_push(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = ws_upgrade(Conn),
    HandlerPid = ws_handler_pid(),
    ok = json_rpc_ws:push(HandlerPid, <<"event">>, #{<<"k">> => 1}),
    {ws, {text, RespBin}} = gun:await(Conn, StreamRef, 5000),
    Decoded = jiffy:decode(RespBin, [return_maps]),
    %% A Notification has no `id' member.
    ?assertNot(maps:is_key(<<"id">>, Decoded)),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"event">>,
            <<"params">> => #{<<"k">> => 1}
        },
        Decoded
    ).

%% `json_rpc_ws:publish/3' fans a Notification out to every subscriber of
%% `Topic'. Open two WS connections, subscribe both, publish, verify both
%% receive the frame. Then unsubscribe one, publish again, verify only the
%% remaining subscriber gets it.
test_ws_publish(Config) ->
    Conn1 = ?config(conn, Config),
    Stream1 = ws_upgrade(Conn1),
    {ok, Conn2} = gun:open(?HOST, ?PORT, #{
        transport => tcp, protocols => [http], retry => 0
    }),
    {ok, http} = gun:await_up(Conn2, 5000),
    try
        Stream2 = ws_upgrade_on(Conn2),
        [PidA, PidB] = ws_handler_pids(2),
        ok = json_rpc_ws:subscribe(PidA, news),
        ok = json_rpc_ws:subscribe(PidB, news),
        ok = json_rpc_ws:publish(news, <<"hi">>, [1, 2]),
        Frame1 = await_text(Conn1, Stream1),
        Frame2 = await_text(Conn2, Stream2),
        Expected = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"hi">>,
            <<"params">> => [1, 2]
        },
        ?assertEqual(Expected, jiffy:decode(Frame1, [return_maps])),
        ?assertEqual(Expected, jiffy:decode(Frame2, [return_maps])),
        ok = json_rpc_ws:unsubscribe(PidB, news),
        ok = json_rpc_ws:publish(news, <<"hi2">>, [3]),
        Frame1b = await_text(Conn1, Stream1),
        ?assertEqual(<<"hi2">>, maps:get(<<"method">>, jiffy:decode(Frame1b, [return_maps]))),
        ?assertEqual({error, timeout}, gun:await(Conn2, Stream2, 200))
    after
        gun:close(Conn2)
    end.

%% `pg' monitors its members, so when a WS handler process exits its
%% subscriptions disappear automatically. Open a connection, subscribe to a
%% topic, close the connection, and assert the topic's member list is
%% empty.
test_ws_subscriber_cleanup_on_close(Config) ->
    Conn = ?config(conn, Config),
    _StreamRef = ws_upgrade(Conn),
    HandlerPid = ws_handler_pid(),
    ok = json_rpc_ws:subscribe(HandlerPid, cleanup_topic),
    ?assertEqual(
        [HandlerPid],
        pg:get_members(json_rpc, {json_rpc_topic, cleanup_topic})
    ),
    MRef = erlang:monitor(process, HandlerPid),
    gun:close(Conn),
    receive
        {'DOWN', MRef, process, HandlerPid, _} -> ok
    after 5000 ->
        erlang:error(handler_did_not_exit)
    end,
    %% Members are cleaned up asynchronously by the pg monitor; poll briefly.
    ok = wait_for_pg_cleanup({json_rpc_topic, cleanup_topic}, 2000).

ws_upgrade_on(Conn) ->
    StreamRef = gun:ws_upgrade(Conn, "/ws"),
    receive
        {gun_upgrade, Conn, StreamRef, [<<"websocket">>], _Hs} -> StreamRef
    after 5000 ->
        erlang:error(ws_upgrade_timeout)
    end.

await_text(Conn, StreamRef) ->
    case gun:await(Conn, StreamRef, 5000) of
        {ws, {text, Bin}} -> Bin;
        Other -> erlang:error({unexpected, Other})
    end.

ws_handler_pid() ->
    [Pid] = ws_handler_pids(1),
    Pid.

%% Wait briefly until ranch reports the expected number of connection
%% processes. Returns the pid list, sorted to make positional access stable
%% across runs.
ws_handler_pids(N) ->
    ws_handler_pids(N, 50).

ws_handler_pids(N, 0) ->
    erlang:error({no_ws_handler_pids, N, ranch:procs(json_rpc_listener, connections)});
ws_handler_pids(N, Tries) ->
    Pids = ranch:procs(json_rpc_listener, connections),
    case length(Pids) =:= N of
        true -> lists:sort(Pids);
        false ->
            timer:sleep(20),
            ws_handler_pids(N, Tries - 1)
    end.

wait_for_pg_cleanup(_Group, Budget) when Budget =< 0 ->
    erlang:error(pg_cleanup_timeout);
wait_for_pg_cleanup(Group, Budget) ->
    case pg:get_members(json_rpc, Group) of
        [] -> ok;
        _ ->
            timer:sleep(50),
            wait_for_pg_cleanup(Group, Budget - 50)
    end.

%% Open a WS connection, then stop the application from a separate
%% process. The WS handler must receive the drain broadcast and emit a
%% 1001 close frame within the drain window. Restart the app on the way
%% out so later tests keep working.
test_ws_drain_on_shutdown(_Config) ->
    {ok, Conn} = gun:open(?HOST, ?PORT, #{
        transport => tcp, protocols => [http], retry => 0
    }),
    {ok, http} = gun:await_up(Conn, 5000),
    try
        StreamRef = ws_upgrade_on(Conn),
        Self = self(),
        Stopper = spawn(fun() ->
            ok = application:stop(json_rpc),
            Self ! {stopped, self()}
        end),
        Result =
            receive
                {gun_ws, Conn, StreamRef, {close, Code, _Reason}} -> {close, Code}
            after 6000 ->
                erlang:error(no_close_received)
            end,
        ?assertEqual({close, 1001}, Result),
        receive
            {stopped, Stopper} -> ok
        after 6000 ->
            erlang:error(stop_did_not_complete)
        end
    after
        gun:close(Conn),
        {ok, _} = application:ensure_all_started(json_rpc),
        register_methods()
    end.

%%% WebSocket helpers

ws_upgrade(Conn) ->
    StreamRef = gun:ws_upgrade(Conn, "/ws"),
    receive
        {gun_upgrade, Conn, StreamRef, [<<"websocket">>], _Headers} ->
            StreamRef;
        {gun_response, Conn, StreamRef, _, Status, _Headers} ->
            erlang:error({ws_upgrade_failed, Status});
        {gun_error, Conn, StreamRef, Reason} ->
            erlang:error({ws_upgrade_error, Reason})
    after 5000 ->
        erlang:error(ws_upgrade_timeout)
    end.

%% Drop the response Headers value when comparing — we only care about
%% Status/Body for the cases that use this.
normalize({Status, _Hs, Body}) -> {Status, ignore, Body}.

%%% Registry test cases

test_rpc_discover(Config) ->
    Conn = ?config(conn, Config),
    Req = #{jsonrpc => <<"2.0">>, method => <<"rpc.discover">>, id => 1},
    {200, _Hs, Body} = raw_post(Conn, jiffy:encode(Req)),
    #{<<"result">> := Methods, <<"id">> := 1} = jiffy:decode(Body, [return_maps]),
    %% Every method we registered in init_per_suite plus the built-in
    %% rpc.discover itself must be present. Any additional methods that
    %% other tests register transiently shouldn't break this.
    Required = [
        <<"subtract">>, <<"sum">>, <<"get_data">>, <<"update">>,
        <<"notify_sum">>, <<"notify_hello">>, <<"throw_error">>,
        <<"throw_reserved_error">>, <<"slow">>, <<"crash">>,
        <<"crash_exit">>, <<"rpc.discover">>
    ],
    lists:foreach(
        fun(M) -> ?assert(lists:member(M, Methods)) end,
        Required
    ).

test_register_rpc_reserved(_Config) ->
    %% Reserved namespace is rejected at registration time. The built-in
    %% rpc.discover is the single allowed exception.
    ?assertMatch(
        {error, {invalid_method_name, <<"rpc.foo">>}},
        json_rpc_methods:register_method(<<"rpc.foo">>, {?MODULE, dummy_handler})
    ),
    ?assertMatch(
        {error, {invalid_method_name, <<>>}},
        json_rpc_methods:register_method(<<>>, {?MODULE, dummy_handler})
    ).

test_register_invalid_handler(_Config) ->
    ?assertMatch(
        {error, {invalid_handler, _}},
        json_rpc_methods:register_method(<<"bogus">>, not_an_mfa)
    ),
    ?assertMatch(
        {error, {invalid_handler, _}},
        json_rpc_methods:register_method(<<"bogus">>, {only_one})
    ).

test_register_full(_Config) ->
    %% Set a tiny cap, register up to it, then assert the next one fails.
    %% Restore the original cap on the way out so later tests aren't affected.
    Original = application:get_env(json_rpc, max_methods),
    Existing = length(json_rpc_methods:list_methods()),
    Cap = Existing + 3,
    application:set_env(json_rpc, max_methods, Cap),
    try
        ok = json_rpc_methods:register_method(<<"cap_a">>, {?MODULE, dummy_handler}),
        ok = json_rpc_methods:register_method(<<"cap_b">>, {?MODULE, dummy_handler}),
        ok = json_rpc_methods:register_method(<<"cap_c">>, {?MODULE, dummy_handler}),
        ?assertEqual(
            {error, registry_full},
            json_rpc_methods:register_method(<<"cap_d">>, {?MODULE, dummy_handler})
        )
    after
        ok = json_rpc_methods:unregister_method(<<"cap_a">>),
        ok = json_rpc_methods:unregister_method(<<"cap_b">>),
        ok = json_rpc_methods:unregister_method(<<"cap_c">>),
        case Original of
            {ok, V} -> application:set_env(json_rpc, max_methods, V);
            undefined -> application:unset_env(json_rpc, max_methods)
        end
    end.

%% A foreign process must not be able to bypass the gen_server's
%% validation by writing to the ETS table directly. The table is created
%% as `protected', so only the owner (the json_rpc_methods gen_server)
%% may write; foreign writes raise `error:badarg'.
test_methods_table_is_protected(_Config) ->
    Self = self(),
    Pid = spawn(fun() ->
        Result =
            try ets:insert(json_rpc_methods, {<<"rogue">>, {mfa, ?MODULE, dummy_handler}}) of
                Other -> {ok, Other}
            catch
                Class:Reason -> {Class, Reason}
            end,
        Self ! {result, Result}
    end),
    receive
        {result, R} ->
            ?assertMatch({error, badarg}, R)
    after 5000 ->
        erlang:error({foreign_writer_did_not_finish, Pid})
    end,
    %% And the rogue entry must not be present in the registry.
    ?assertEqual(not_found, json_rpc_methods:lookup(<<"rogue">>)).

%%% Application lifecycle test cases

%% Start an in-flight request that the handler holds open, then stop the
%% application from a separate process. The drain timeout (default 5s) gives
%% the in-flight request enough time to complete; assert it returns success
%% rather than being killed mid-response. Restart the app at the end so the
%% rest of the suite can keep running.
test_app_drain(_Config) ->
    %% Open a dedicated connection — the per-testcase one would race with
    %% application stop.
    {ok, Conn} = gun:open(?HOST, ?PORT, #{transport => tcp, protocols => [http], retry => 0}),
    {ok, http} = gun:await_up(Conn, 5000),
    Payload = jiffy:encode(#{
        jsonrpc => <<"2.0">>, method => <<"slow">>, params => [800], id => 1
    }),
    StreamRef = gun:post(Conn, "/rpc", ?JSON_HEADERS, Payload),
    %% Give the handler a moment to start sleeping before we tear down.
    timer:sleep(150),
    Self = self(),
    Stopper = spawn(fun() ->
        ok = application:stop(json_rpc),
        Self ! {stopped, self()}
    end),
    {response, nofin, 200, _Hs} = gun:await(Conn, StreamRef, 5000),
    {ok, Body} = gun:await_body(Conn, StreamRef, 5000),
    ?assertMatch(
        #{<<"result">> := <<"done">>, <<"id">> := 1},
        jiffy:decode(Body, [return_maps])
    ),
    receive
        {stopped, Stopper} -> ok
    after 6000 ->
        erlang:error(stop_did_not_complete)
    end,
    gun:close(Conn),
    %% Restart for the remaining tests.
    {ok, _} = application:ensure_all_started(json_rpc),
    register_methods(),
    ok.

%% Stop the application, confirm a fresh start succeeds, and that we can
%% call a method again over a brand-new connection. Run this last so it
%% doesn't tear the suite out from under any later test cases. We always
%% leave the app running on the way out so end_per_suite can stop cleanly.
test_app_lifecycle(_Config) ->
    ok = application:stop(json_rpc),
    try
        {ok, _} = application:ensure_all_started(json_rpc),
        register_methods(),
        {ok, Conn} = gun:open(?HOST, ?PORT, #{
            transport => tcp, protocols => [http], retry => 0
        }),
        {ok, http} = gun:await_up(Conn, 5000),
        Req = #{
            jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [5, 3], id => 1
        },
        {200, _Hs, Body} = raw_post(Conn, jiffy:encode(Req)),
        ?assertMatch(
            #{<<"result">> := 2, <<"id">> := 1},
            jiffy:decode(Body, [return_maps])
        ),
        gun:close(Conn)
    catch
        Class:Reason:Stack ->
            %% Make sure the app is up so end_per_suite can stop it.
            _ = application:ensure_all_started(json_rpc),
            erlang:raise(Class, Reason, Stack)
    end,
    ok.

%% Used as a placeholder MFA for register/unregister tests.
dummy_handler(_) -> ok.
