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

-module(json_rpc_ws_SUITE).

%% The WebSocket transport: framing, concurrency, server push, and shutdown.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([
    call_round_trip/1,
    notification_produces_no_frame/1,
    batch_round_trip/1,
    malformed_json_returns_parse_error/1,
    subprotocol_offer_is_accepted/1,
    binary_frame_closes_connection/1,
    oversize_frame_closes_connection/1,
    handler_timeout_echoes_id/1,
    crash_isolation_keeps_connection/1,
    unencodable_result_degrades/1,
    slow_call_does_not_block_connection/1,
    in_flight_cap_sheds_load/1,
    context_reports_websocket_transport/1,
    push_delivers_notification/1,
    publish_fans_out/1,
    handler_subscribes_own_connection/1,
    subscriptions_cleaned_up_on_close/1
]).

-define(PORT, 18083).

all() ->
    [
        call_round_trip,
        notification_produces_no_frame,
        batch_round_trip,
        malformed_json_returns_parse_error,
        subprotocol_offer_is_accepted,
        binary_frame_closes_connection,
        oversize_frame_closes_connection,
        handler_timeout_echoes_id,
        crash_isolation_keeps_connection,
        unencodable_result_degrades,
        slow_call_does_not_block_connection,
        in_flight_cap_sheds_load,
        context_reports_websocket_transport,
        push_delivers_notification,
        publish_fans_out,
        handler_subscribes_own_connection,
        subscriptions_cleaned_up_on_close
    ].

init_per_suite(Config) ->
    ok = json_rpc_test_support:start_app(?PORT, #{}),
    Config.

end_per_suite(_Config) ->
    ok = json_rpc_test_support:stop_app().

init_per_testcase(_TestCase, Config) ->
    [{conn, json_rpc_test_support:connect(?PORT)} | Config].

end_per_testcase(_TestCase, Config) ->
    gun:close(?config(conn, Config)).

%%% Helpers

upgrade(Config) ->
    Conn = ?config(conn, Config),
    {Conn, json_rpc_test_support:ws_upgrade(Conn)}.

request(Method, Params, Id) ->
    #{jsonrpc => <<"2.0">>, method => Method, params => Params, id => Id}.

send(Conn, StreamRef, Term) ->
    json_rpc_test_support:ws_send(Conn, StreamRef, Term).

recv(Conn, StreamRef) ->
    json_rpc_test_support:ws_recv(Conn, StreamRef).

%% The connection processes Ranch knows about, which for this suite are the
%% WebSocket handlers.
connection_pids(Expected) ->
    ok = json_rpc_test_support:wait_until(
        fun() -> length(ranch:procs(json_rpc_listener, connections)) =:= Expected end,
        2000
    ),
    lists:sort(ranch:procs(json_rpc_listener, connections)).

%%% Cases

call_round_trip(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    send(Conn, StreamRef, request(<<"subtract">>, [42, 23], 1)),
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => 19},
        recv(Conn, StreamRef)
    ).

notification_produces_no_frame(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    send(Conn, StreamRef, #{
        jsonrpc => <<"2.0">>, method => <<"update">>, params => [1, 2, 3]
    }),
    ok = json_rpc_test_support:ws_expect_silence(Conn, StreamRef).

batch_round_trip(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    send(Conn, StreamRef, [
        request(<<"sum">>, [1, 2, 4], <<"1">>),
        request(<<"subtract">>, [42, 23], <<"2">>)
    ]),
    ?assertEqual(
        [
            #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => <<"1">>, <<"result">> => 7},
            #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => <<"2">>, <<"result">> => 19}
        ],
        recv(Conn, StreamRef)
    ).

malformed_json_returns_parse_error(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    gun:ws_send(Conn, StreamRef, {text, <<"{not valid json">>}),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => null,
            <<"error">> => #{<<"code">> => -32700, <<"message">> => <<"Parse error">>}
        },
        recv(Conn, StreamRef)
    ).

%% RFC 6455 lets the server select no subprotocol by omitting the header.
%% Refusing the upgrade over an offer breaks common clients for no benefit.
subprotocol_offer_is_accepted(Config) ->
    Conn = ?config(conn, Config),
    StreamRef = json_rpc_test_support:ws_upgrade(
        Conn, [{<<"sec-websocket-protocol">>, <<"foo">>}]
    ),
    send(Conn, StreamRef, request(<<"subtract">>, [42, 23], 1)),
    ?assertMatch(#{<<"result">> := 19}, recv(Conn, StreamRef)).

%% JSON-RPC is defined over text. Closing beats silently dropping the frame,
%% which would leave the client waiting for a reply that never comes.
binary_frame_closes_connection(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    gun:ws_send(Conn, StreamRef, {binary, <<"junk">>}),
    ?assertMatch(
        {close, 1003, _Reason},
        json_rpc_test_support:ws_recv_frame(Conn, StreamRef)
    ).

oversize_frame_closes_connection(Config) ->
    json_rpc_test_support:with_env(ws_max_frame_bytes, 4096, fun() ->
        {Conn, StreamRef} = upgrade(Config),
        gun:ws_send(Conn, StreamRef, {text, binary:copy(<<"x">>, 8192)}),
        ?assertMatch(
            {close, 1009, _Reason},
            json_rpc_test_support:ws_recv_frame(Conn, StreamRef)
        )
    end).

handler_timeout_echoes_id(Config) ->
    json_rpc_test_support:with_env(handler_timeout_ms, 150, fun() ->
        {Conn, StreamRef} = upgrade(Config),
        send(Conn, StreamRef, request(<<"slow">>, [2000], 1)),
        ?assertEqual(
            #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => 1,
                <<"error">> => #{
                    <<"code">> => -32603,
                    <<"message">> => <<"Internal error">>,
                    <<"data">> => #{<<"reason">> => <<"timeout">>}
                }
            },
            recv(Conn, StreamRef)
        )
    end).

crash_isolation_keeps_connection(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    lists:foreach(
        fun(Method) ->
            send(Conn, StreamRef, request(Method, [], 1)),
            ?assertMatch(#{<<"error">> := #{<<"code">> := -32603}}, recv(Conn, StreamRef))
        end,
        [<<"crash">>, <<"crash_exit">>, <<"crash_throw">>]
    ),
    send(Conn, StreamRef, request(<<"subtract">>, [42, 23], 2)),
    ?assertMatch(#{<<"result">> := 19}, recv(Conn, StreamRef)).

%% Encoding used to happen outside any try/catch on this path, so a result
%% JSON could not represent killed the connection and every call on it.
unencodable_result_degrades(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    send(Conn, StreamRef, request(<<"unencodable">>, [], 1)),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"error">> => #{<<"code">> => -32603, <<"message">> => <<"Internal error">>}
        },
        recv(Conn, StreamRef)
    ),
    send(Conn, StreamRef, request(<<"subtract">>, [42, 23], 2)),
    ?assertMatch(#{<<"result">> := 19}, recv(Conn, StreamRef)).

%% The point of dispatching each frame in its own process: a fast call sent
%% after a slow one must not wait behind it.
slow_call_does_not_block_connection(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    send(Conn, StreamRef, request(<<"slow">>, [1500], <<"slow">>)),
    send(Conn, StreamRef, request(<<"subtract">>, [42, 23], <<"fast">>)),
    %% The fast reply must arrive first, well inside the slow call's sleep.
    ?assertEqual(
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => <<"fast">>, <<"result">> => 19},
        recv(Conn, StreamRef)
    ),
    ?assertMatch(#{<<"id">> := <<"slow">>}, recv(Conn, StreamRef)).

%% Past the in-flight cap a call is refused immediately rather than queued,
%% so a client that pipelines without limit cannot grow the node unbounded.
in_flight_cap_sheds_load(Config) ->
    json_rpc_test_support:with_env(ws_max_in_flight, 2, fun() ->
        {Conn, StreamRef} = upgrade(Config),
        lists:foreach(
            fun(N) -> send(Conn, StreamRef, request(<<"slow">>, [800], N)) end,
            lists:seq(1, 5)
        ),
        Replies = [recv(Conn, StreamRef) || _ <- lists:seq(1, 5)],
        Shed = [R || #{<<"error">> := #{<<"code">> := -32000}} = R <- Replies],
        Served = [R || #{<<"result">> := _} = R <- Replies],
        ?assertEqual(5, length(Shed) + length(Served)),
        %% Some are shed and some are served; the exact split depends on
        %% scheduling, so assert only that both happened.
        ?assertMatch([_ | _], Shed),
        ?assertMatch([_ | _], Served)
    end).

context_reports_websocket_transport(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    send(Conn, StreamRef, request(<<"context">>, [], 1)),
    ?assertEqual(
        #{
            <<"transport">> => <<"websocket">>,
            <<"has_connection_pid">> => true,
            <<"has_request_id">> => true
        },
        maps:get(<<"result">>, recv(Conn, StreamRef))
    ).

push_delivers_notification(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    [Pid] = connection_pids(1),
    ok = json_rpc:push(Pid, <<"event">>, #{<<"k">> => 1}),
    Notification = recv(Conn, StreamRef),
    %% A Notification carries no id, which is what makes it one.
    ?assertNot(maps:is_key(<<"id">>, Notification)),
    ?assertEqual(
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"event">>,
            <<"params">> => #{<<"k">> => 1}
        },
        Notification
    ).

publish_fans_out(Config) ->
    {Conn1, Stream1} = upgrade(Config),
    Conn2 = json_rpc_test_support:connect(?PORT),
    try
        Stream2 = json_rpc_test_support:ws_upgrade(Conn2),
        [PidA, PidB] = connection_pids(2),
        ok = json_rpc:subscribe(PidA, news),
        ok = json_rpc:subscribe(PidB, news),
        ok = json_rpc:publish(news, <<"hi">>, [1, 2]),
        Expected = #{
            <<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"hi">>, <<"params">> => [1, 2]
        },
        ?assertEqual(Expected, recv(Conn1, Stream1)),
        ?assertEqual(Expected, recv(Conn2, Stream2)),
        %% After unsubscribing, only the remaining subscriber is delivered to.
        ok = json_rpc:unsubscribe(PidB, news),
        ok = json_rpc:publish(news, <<"hi2">>, [3]),
        ?assertMatch(#{<<"method">> := <<"hi2">>}, recv(Conn1, Stream1)),
        ok = json_rpc_test_support:ws_expect_silence(Conn2, Stream2)
    after
        gun:close(Conn2)
    end.

%% The reason arity-2 handlers take a context: a handler subscribing the
%% connection it was called on, which it otherwise has no way to name.
handler_subscribes_own_connection(Config) ->
    {Conn, StreamRef} = upgrade(Config),
    send(Conn, StreamRef, request(<<"subscribe_self">>, [<<"prices">>], 1)),
    ?assertMatch(#{<<"result">> := <<"subscribed">>}, recv(Conn, StreamRef)),
    ok = json_rpc:publish(prices, <<"tick">>, #{<<"px">> => 42}),
    ?assertMatch(#{<<"method">> := <<"tick">>}, recv(Conn, StreamRef)).

%% `pg' monitors its members, so a disconnect cleans up subscriptions with no
%% help from terminate/3.
subscriptions_cleaned_up_on_close(Config) ->
    {Conn, _StreamRef} = upgrade(Config),
    [Pid] = connection_pids(1),
    ok = json_rpc:subscribe(Pid, cleanup_topic),
    ?assertEqual([Pid], json_rpc_ws:subscribers(cleanup_topic)),
    MonitorRef = erlang:monitor(process, Pid),
    gun:close(Conn),
    receive
        {'DOWN', MonitorRef, process, Pid, _Reason} -> ok
    after 5000 ->
        erlang:error(connection_did_not_exit)
    end,
    ok = json_rpc_test_support:wait_until(
        fun() -> json_rpc_ws:subscribers(cleanup_topic) =:= [] end, 2000
    ).
