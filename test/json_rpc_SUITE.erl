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
    test_http_explicit_null_id_is_call/1,
    test_http_batch_mixed/1,
    test_http_all_notification_batch/1,
    test_http_empty_batch/1,
    test_http_malformed_json/1,
    test_http_oversize_body/1,
    test_http_method_not_allowed/1,
    test_http_unsupported_media_type/1
]).

%% WebSocket test cases.
-export([
    test_ws_call/1,
    test_ws_notification/1,
    test_ws_batch/1,
    test_ws_malformed_json/1
]).

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
        test_http_explicit_null_id_is_call,
        test_http_batch_mixed,
        test_http_all_notification_batch,
        test_http_empty_batch,
        test_http_malformed_json,
        test_http_oversize_body,
        test_http_method_not_allowed,
        test_http_unsupported_media_type,
        test_ws_call,
        test_ws_notification,
        test_ws_batch,
        test_ws_malformed_json
    ].

init_per_suite(Config) ->
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

init_per_testcase(_TestCase, Config) ->
    {ok, ConnPid} = gun:open(?HOST, ?PORT, #{
        transport => tcp,
        protocols => [http],
        retry => 0
    }),
    {ok, http} = gun:await_up(ConnPid, 5000),
    [{conn, ConnPid} | Config].

end_per_testcase(_TestCase, Config) ->
    case ?config(conn, Config) of
        undefined -> ok;
        ConnPid -> gun:close(ConnPid)
    end,
    ok.

%%% Method registration

register_methods() ->
    Methods = [
        {<<"subtract">>, fun([A, B]) -> A - B end},
        {<<"sum">>, fun([A, B, C]) -> A + B + C end},
        {<<"get_data">>, fun(_) -> [<<"hello">>, 5] end},
        {<<"update">>, fun(_) -> ok end},
        {<<"notify_sum">>, fun(_) -> ok end},
        {<<"notify_hello">>, fun(_) -> ok end},
        {<<"throw_error">>, fun(_) ->
            throw({jsonrpc_error, -32602, <<"bad arg">>, #{<<"detail">> => <<"oops">>}})
        end}
    ],
    lists:foreach(
        fun({Name, Fun}) -> ok = json_rpc_server:register_method(Name, Fun) end,
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
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{
                <<"code">> => -32602,
                <<"message">> => <<"bad arg">>,
                <<"data">> => #{<<"detail">> => <<"oops">>}
            },
            <<"id">> => <<"e1">>
        },
        rpc_call(Conn, #{
            jsonrpc => <<"2.0">>, method => <<"throw_error">>, params => [], id => <<"e1">>
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
            <<"error">> => #{<<"code">> => -32700, <<"message">> => <<"Parse error">>},
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
