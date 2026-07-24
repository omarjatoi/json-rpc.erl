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

-module(json_rpc_http_SUITE).

%% The HTTP transport: status codes, framing, body limits, and the guarantee
%% that every response — including the failures — is a JSON-RPC envelope.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([
    call_returns_200/1,
    notification_returns_204/1,
    all_notification_batch_returns_204/1,
    batch_returns_array/1,
    malformed_json_returns_parse_error/1,
    oversize_body_returns_413_with_envelope/1,
    oversize_body_does_not_overshoot/1,
    wrong_method_returns_405_with_envelope/1,
    wrong_content_type_returns_415_with_envelope/1,
    content_type_with_charset_is_accepted/1,
    structured_json_suffix_is_accepted/1,
    handler_timeout_returns_200/1,
    crash_isolation_keeps_connection/1,
    unencodable_result_degrades/1,
    unencodable_batch_element_degrades_alone/1,
    context_reports_http_transport/1,
    request_id_header_is_honoured/1,
    keepalive_serves_many_requests/1
]).

-define(PORT, 18082).
-define(MAX_BODY, 4096).

all() ->
    [
        call_returns_200,
        notification_returns_204,
        all_notification_batch_returns_204,
        batch_returns_array,
        malformed_json_returns_parse_error,
        oversize_body_returns_413_with_envelope,
        oversize_body_does_not_overshoot,
        wrong_method_returns_405_with_envelope,
        wrong_content_type_returns_415_with_envelope,
        content_type_with_charset_is_accepted,
        structured_json_suffix_is_accepted,
        handler_timeout_returns_200,
        crash_isolation_keeps_connection,
        unencodable_result_degrades,
        unencodable_batch_element_degrades_alone,
        context_reports_http_transport,
        request_id_header_is_honoured,
        keepalive_serves_many_requests
    ].

init_per_suite(Config) ->
    ok = json_rpc_test_support:start_app(?PORT, #{max_body_bytes => ?MAX_BODY}),
    Config.

end_per_suite(_Config) ->
    ok = json_rpc_test_support:stop_app().

init_per_testcase(_TestCase, Config) ->
    [{conn, json_rpc_test_support:connect(?PORT)} | Config].

end_per_testcase(_TestCase, Config) ->
    gun:close(?config(conn, Config)).

%%% Helpers

rpc(Config, Term) ->
    json_rpc_test_support:rpc(?config(conn, Config), "/rpc", Term).

post(Config, Body) ->
    json_rpc_test_support:post(?config(conn, Config), "/rpc", Body).

post(Config, Body, Headers) ->
    json_rpc_test_support:post(?config(conn, Config), "/rpc", Body, Headers).

request(Method, Params, Id) ->
    #{jsonrpc => <<"2.0">>, method => Method, params => Params, id => Id}.

%%% Cases

call_returns_200(Config) ->
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => 19},
        rpc(Config, request(<<"subtract">>, [42, 23], 1))
    ).

%% A Notification has no Response, and 204 is how that is said over HTTP.
notification_returns_204(Config) ->
    Body = json_rpc_test_support:encode(#{
        jsonrpc => <<"2.0">>, method => <<"update">>, params => [1, 2, 3]
    }),
    ?assertMatch({204, _Headers, <<>>}, post(Config, Body)).

all_notification_batch_returns_204(Config) ->
    Body = json_rpc_test_support:encode([
        #{jsonrpc => <<"2.0">>, method => <<"notify_sum">>, params => [1, 2, 4]},
        #{jsonrpc => <<"2.0">>, method => <<"notify_hello">>, params => [7]}
    ]),
    ?assertMatch({204, _Headers, <<>>}, post(Config, Body)).

batch_returns_array(Config) ->
    Decoded = rpc(Config, [
        request(<<"sum">>, [1, 2, 4], <<"1">>),
        request(<<"subtract">>, [42, 23], <<"2">>)
    ]),
    ?assertEqual(
        [
            #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => <<"1">>, <<"result">> => 7},
            #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => <<"2">>, <<"result">> => 19}
        ],
        Decoded
    ).

%% Unparseable JSON is still a successful HTTP exchange carrying -32700.
malformed_json_returns_parse_error(Config) ->
    {Status, _Headers, Body} = post(Config, <<"{\"jsonrpc\": \"2.0\", \"baz]">>),
    ?assertEqual(200, Status),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => null,
            <<"error">> => #{<<"code">> => -32700, <<"message">> => <<"Parse error">>}
        },
        json_rpc_test_support:decode(Body)
    ).

%% Nothing was parsed, so the code is -32600 rather than -32700, and the body
%% is a JSON-RPC envelope rather than empty.
oversize_body_returns_413_with_envelope(Config) ->
    Big = binary:copy(<<"x">>, ?MAX_BODY + 1024),
    Body = <<"{\"jsonrpc\":\"2.0\",\"method\":\"echo\",\"params\":[\"", Big/binary, "\"]}">>,
    {Status, _Headers, RespBody} = post(Config, Body),
    ?assertEqual(413, Status),
    ?assertMatch(
        #{<<"error">> := #{<<"code">> := -32600}},
        json_rpc_test_support:decode(RespBody)
    ).

%% A body arriving in several chunks must be cut off near the cap, not at
%% roughly twice it. 8x the cap in one request would previously buffer far
%% past the limit before the check ran.
oversize_body_does_not_overshoot(Config) ->
    Big = binary:copy(<<"x">>, ?MAX_BODY * 8),
    {Status, _Headers, _Body} = post(Config, Big),
    ?assertEqual(413, Status).

wrong_method_returns_405_with_envelope(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = gun:get(Conn, "/rpc", []),
    {response, nofin, Status, Headers} = gun:await(Conn, StreamRef, 5000),
    {ok, Body} = gun:await_body(Conn, StreamRef, 5000),
    ?assertEqual(405, Status),
    ?assertEqual(<<"POST">>, proplists:get_value(<<"allow">>, Headers)),
    ?assertMatch(
        #{<<"error">> := #{<<"code">> := -32600}},
        json_rpc_test_support:decode(Body)
    ).

wrong_content_type_returns_415_with_envelope(Config) ->
    Body = json_rpc_test_support:encode(request(<<"subtract">>, [1, 2], 1)),
    {Status, _Headers, RespBody} = post(Config, Body, [{<<"content-type">>, <<"text/plain">>}]),
    ?assertEqual(415, Status),
    ?assertMatch(
        #{<<"error">> := #{<<"code">> := -32600}},
        json_rpc_test_support:decode(RespBody)
    ).

content_type_with_charset_is_accepted(Config) ->
    Body = json_rpc_test_support:encode(request(<<"subtract">>, [42, 23], 1)),
    Headers = [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
    ?assertMatch({200, _Headers, _Body}, post(Config, Body, Headers)).

%% `application/vnd.example+json' and friends are JSON by the structured
%% suffix rule, and clients do send them.
structured_json_suffix_is_accepted(Config) ->
    Body = json_rpc_test_support:encode(request(<<"subtract">>, [42, 23], 1)),
    Headers = [{<<"content-type">>, <<"application/vnd.example+json">>}],
    ?assertMatch({200, _Headers, _Body}, post(Config, Body, Headers)).

handler_timeout_returns_200(Config) ->
    json_rpc_test_support:with_env(handler_timeout_ms, 150, fun() ->
        {Status, _Headers, Body} = post(
            Config, json_rpc_test_support:encode(request(<<"slow">>, [2000], 7))
        ),
        ?assertEqual(200, Status),
        ?assertEqual(
            #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => 7,
                <<"error">> => #{
                    <<"code">> => -32603,
                    <<"message">> => <<"Internal error">>,
                    <<"data">> => #{<<"reason">> => <<"timeout">>}
                }
            },
            json_rpc_test_support:decode(Body)
        )
    end).

%% A crashing handler must not disturb the keep-alive connection.
crash_isolation_keeps_connection(Config) ->
    lists:foreach(
        fun(Method) ->
            ?assertMatch(
                #{<<"error">> := #{<<"code">> := -32603}},
                rpc(Config, request(Method, [], 1))
            )
        end,
        [<<"crash">>, <<"crash_exit">>, <<"crash_throw">>]
    ),
    ?assertMatch(
        #{<<"result">> := 19},
        rpc(Config, request(<<"subtract">>, [42, 23], 2))
    ).

%% A result JSON cannot represent must become -32603 rather than a 500 or a
%% dropped connection.
unencodable_result_degrades(Config) ->
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"error">> => #{<<"code">> => -32603, <<"message">> => <<"Internal error">>}
        },
        rpc(Config, request(<<"unencodable">>, [], 1))
    ),
    %% And the connection is still usable.
    ?assertMatch(#{<<"result">> := 19}, rpc(Config, request(<<"subtract">>, [42, 23], 2))).

%% One bad element must not cost the whole batch. Its siblings are delivered.
unencodable_batch_element_degrades_alone(Config) ->
    Decoded = rpc(Config, [
        request(<<"subtract">>, [42, 23], 1),
        request(<<"unencodable_in_batch">>, [], 2),
        request(<<"sum">>, [1, 2, 4], 3)
    ]),
    ?assertEqual(
        [
            #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => 19},
            #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => 2,
                <<"error">> => #{<<"code">> => -32603, <<"message">> => <<"Internal error">>}
            },
            #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 3, <<"result">> => 7}
        ],
        Decoded
    ).

context_reports_http_transport(Config) ->
    ?assertEqual(
        #{
            <<"transport">> => <<"http">>,
            <<"has_connection_pid">> => true,
            <<"has_request_id">> => true
        },
        maps:get(<<"result">>, rpc(Config, request(<<"context">>, [], 1)))
    ).

%% An upstream proxy's request id must survive into the request context so it
%% can be correlated across services.
request_id_header_is_honoured(Config) ->
    Body = json_rpc_test_support:encode(request(<<"context">>, [], 1)),
    Headers = [
        {<<"content-type">>, <<"application/json">>},
        {<<"x-request-id">>, <<"trace-abc-123">>}
    ],
    {200, _RespHeaders, RespBody} = post(Config, Body, Headers),
    ?assertMatch(
        #{<<"result">> := #{<<"has_request_id">> := true}},
        json_rpc_test_support:decode(RespBody)
    ).

keepalive_serves_many_requests(Config) ->
    lists:foreach(
        fun(N) ->
            ?assertEqual(
                #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => N, <<"result">> => 19},
                rpc(Config, request(<<"subtract">>, [42, 23], N))
            )
        end,
        lists:seq(1, 25)
    ).
