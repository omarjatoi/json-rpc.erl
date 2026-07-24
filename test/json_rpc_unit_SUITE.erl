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

-module(json_rpc_unit_SUITE).

%% The pure modules, tested without an application or a socket: the codec,
%% error construction and classification, request validation, and the worker
%% pool. These are the pieces every other suite depends on being correct.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0]).

-export([
    encode_round_trip/1,
    encode_rejects_unrepresentable/1,
    encode_iodata_avoids_flattening/1,
    decode_rejects_malformed/1,
    decode_rejects_trailing_bytes/1,
    error_constructors/1,
    server_error_range_is_enforced/1,
    reserved_range_classification/1,
    from_term_shapes/1,
    throw_error_raises/1,
    response_constructors/1,
    parse_call/1,
    parse_notification/1,
    parse_invalid/1,
    id_for_error/1,
    worker_runs_concurrently/1,
    worker_contains_every_exception_class/1,
    worker_kills_at_deadline/1,
    worker_preserves_order/1
]).

all() ->
    [{group, codec}, {group, errors}, {group, requests}, {group, worker}].

groups() ->
    [
        {codec, [], [
            encode_round_trip,
            encode_rejects_unrepresentable,
            encode_iodata_avoids_flattening,
            decode_rejects_malformed,
            decode_rejects_trailing_bytes
        ]},
        {errors, [], [
            error_constructors,
            server_error_range_is_enforced,
            reserved_range_classification,
            from_term_shapes,
            throw_error_raises,
            response_constructors
        ]},
        {requests, [], [
            parse_call,
            parse_notification,
            parse_invalid,
            id_for_error
        ]},
        {worker, [], [
            worker_runs_concurrently,
            worker_contains_every_exception_class,
            worker_kills_at_deadline,
            worker_preserves_order
        ]}
    ].

%%% Codec

encode_round_trip(_Config) ->
    Term = #{<<"a">> => [1, 2.5, true, null, <<"s">>], <<"b">> => #{<<"c">> => -1}},
    {ok, Encoded} = json_rpc_json:encode(Term),
    ?assertEqual({ok, Term}, json_rpc_json:decode(Encoded)).

%% The whole reason the codec returns tagged tuples: these terms would raise
%% out of an encoder, on a path where raising costs a connection.
encode_rejects_unrepresentable(_Config) ->
    lists:foreach(
        fun(Term) -> ?assertMatch({error, _Reason}, json_rpc_json:encode(Term)) end,
        [{a, b}, self(), make_ref(), fun() -> ok end, <<255, 254>>]
    ).

encode_iodata_avoids_flattening(_Config) ->
    {ok, IoData} = json_rpc_json:encode_iodata(#{<<"k">> => 1}),
    ?assertEqual(<<"{\"k\":1}">>, iolist_to_binary(IoData)).

decode_rejects_malformed(_Config) ->
    lists:foreach(
        fun(Binary) -> ?assertEqual({error, parse_error}, json_rpc_json:decode(Binary)) end,
        [<<"{">>, <<>>, <<"{'a':1}">>, <<34, 255, 34>>]
    ).

%% A document followed by junk is not a document. Accepting it would let a
%% smuggled second payload ride along unnoticed.
decode_rejects_trailing_bytes(_Config) ->
    ?assertEqual({error, parse_error}, json_rpc_json:decode(<<"{\"a\":1} trailing">>)).

%%% Errors

error_constructors(_Config) ->
    ?assertEqual(#{code => -32700, message => <<"Parse error">>}, json_rpc_error:parse_error()),
    ?assertEqual(
        #{code => -32600, message => <<"Invalid Request">>}, json_rpc_error:invalid_request()
    ),
    ?assertEqual(
        #{code => -32601, message => <<"Method not found">>},
        json_rpc_error:method_not_found()
    ),
    ?assertEqual(
        #{code => -32602, message => <<"Invalid params">>}, json_rpc_error:invalid_params()
    ),
    ?assertEqual(
        #{code => -32602, message => <<"Invalid params">>, data => #{hint => 1}},
        json_rpc_error:invalid_params(#{hint => 1})
    ),
    ?assertEqual(
        #{code => -32603, message => <<"Internal error">>}, json_rpc_error:internal_error()
    ),
    ?assertEqual(
        #{code => -32603, message => <<"Internal error">>, data => #{reason => timeout}},
        json_rpc_error:internal_error(#{reason => timeout})
    ),
    ?assertEqual(-32601, json_rpc_error:code(json_rpc_error:method_not_found())).

%% server_error/2 is the guarded constructor for the implementation-defined
%% band, so it must refuse codes outside it.
server_error_range_is_enforced(_Config) ->
    ?assertEqual(
        #{code => -32000, message => <<"Server error">>},
        json_rpc_error:server_error(-32000, <<"Server error">>)
    ),
    ?assertEqual(
        #{code => -32099, message => <<"Server error">>},
        json_rpc_error:server_error(-32099, <<"Server error">>)
    ),
    ?assertError(badarg, json_rpc_error:server_error(-32100, <<"too low">>)),
    ?assertError(badarg, json_rpc_error:server_error(-1, <<"too high">>)).

%% The framework owns -32768..-32100. The server-error band below it is
%% delegated to the implementation and must not be treated as reserved.
reserved_range_classification(_Config) ->
    lists:foreach(
        fun(Code) -> ?assert(json_rpc_error:is_reserved(Code)) end,
        [-32768, -32700, -32603, -32601, -32600, -32100]
    ),
    lists:foreach(
        fun(Code) -> ?assertNot(json_rpc_error:is_reserved(Code)) end,
        [-32099, -32050, -32000, -31999, -1, 0, 1]
    ).

from_term_shapes(_Config) ->
    ?assertEqual({ok, #{code => 1, message => <<"m">>}}, json_rpc_error:from_term({1, <<"m">>})),
    ?assertEqual(
        {ok, #{code => 1, message => <<"m">>, data => x}},
        json_rpc_error:from_term({1, <<"m">>, x})
    ),
    ?assertEqual(
        {ok, #{code => 1, message => <<"m">>}},
        json_rpc_error:from_term(#{code => 1, message => <<"m">>})
    ),
    %% Stray members are dropped so a handler cannot smuggle extra keys into
    %% the error object.
    ?assertEqual(
        {ok, #{code => 1, message => <<"m">>}},
        json_rpc_error:from_term(#{code => 1, message => <<"m">>, sneaky => true})
    ),
    lists:foreach(
        fun(Term) -> ?assertEqual(not_an_error, json_rpc_error:from_term(Term)) end,
        [oops, {1, "not a binary"}, {<<"code">>, <<"m">>}, #{code => 1}, 42]
    ).

throw_error_raises(_Config) ->
    ?assertThrow(
        {jsonrpc_error, #{code := -32000, message := <<"m">>}},
        json_rpc_error:throw_error(-32000, <<"m">>)
    ),
    ?assertThrow(
        {jsonrpc_error, #{code := -32000, message := <<"m">>, data := d}},
        json_rpc_error:throw_error(-32000, <<"m">>, d)
    ).

response_constructors(_Config) ->
    ?assertEqual(
        #{jsonrpc => <<"2.0">>, id => 1, result => ok},
        json_rpc_response:result(1, ok)
    ),
    Error = json_rpc_error:internal_error(),
    ?assertEqual(
        #{jsonrpc => <<"2.0">>, id => 1, error => Error},
        json_rpc_response:error(1, Error)
    ),
    ?assertEqual(
        #{jsonrpc => <<"2.0">>, id => 1, error => #{code => -1, message => <<"m">>}},
        json_rpc_response:error(1, -1, <<"m">>)
    ),
    ?assertEqual(
        #{jsonrpc => <<"2.0">>, id => 1, error => #{code => -1, message => <<"m">>, data => d}},
        json_rpc_response:error(1, -1, <<"m">>, d)
    ).

%%% Request validation

parse_call(_Config) ->
    ?assertEqual(
        {call, 1, <<"m">>, [1, 2]},
        json_rpc_request:parse(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"m">>,
            <<"params">> => [1, 2],
            <<"id">> => 1
        })
    ),
    %% Absent params is the same as empty positional params.
    ?assertEqual(
        {call, 1, <<"m">>, []},
        json_rpc_request:parse(#{
            <<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"m">>, <<"id">> => 1
        })
    ).

parse_notification(_Config) ->
    ?assertEqual(
        {notification, <<"m">>, []},
        json_rpc_request:parse(#{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"m">>})
    ).

parse_invalid(_Config) ->
    Invalid = json_rpc_error:invalid_request(),
    ?assertEqual({invalid, null, Invalid}, json_rpc_request:parse(#{})),
    ?assertEqual({invalid, null, Invalid}, json_rpc_request:parse(<<"nope">>)),
    %% A usable id is echoed even when the rest of the envelope is broken.
    ?assertEqual(
        {invalid, <<"keep">>, Invalid},
        json_rpc_request:parse(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => <<"keep">>})
    ),
    %% An id of the wrong type is not usable, so it cannot be echoed.
    ?assertEqual(
        {invalid, null, Invalid},
        json_rpc_request:parse(#{
            <<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"m">>, <<"id">> => true
        })
    ).

id_for_error(_Config) ->
    ?assertEqual(1, json_rpc_request:id_for_error(#{<<"id">> => 1})),
    ?assertEqual(<<"a">>, json_rpc_request:id_for_error(#{<<"id">> => <<"a">>})),
    ?assertEqual(null, json_rpc_request:id_for_error(#{<<"id">> => null})),
    ?assertEqual(null, json_rpc_request:id_for_error(#{})),
    ?assertEqual(null, json_rpc_request:id_for_error(#{<<"id">> => true})),
    %% A batch has no single id to attribute an error to.
    ?assertEqual(null, json_rpc_request:id_for_error([#{<<"id">> => 1}])).

%%% Worker pool

%% Four 200ms sleeps finishing well inside 800ms is only possible if they
%% overlap, which is what lets a batch cost one timeout instead of N.
worker_runs_concurrently(_Config) ->
    Funs = [fun() -> timer:sleep(200) end || _ <- lists:seq(1, 4)],
    Started = erlang:monotonic_time(millisecond),
    Results = json_rpc_worker:run_many(Funs, 2000),
    Elapsed = erlang:monotonic_time(millisecond) - Started,
    ?assertEqual(4, length(Results)),
    ?assert(Elapsed < 700),
    lists:foreach(fun(#{outcome := O}) -> ?assertMatch({ok, ok}, O) end, Results).

worker_contains_every_exception_class(_Config) ->
    Funs = [
        fun() -> error(boom) end,
        fun() -> exit(boom) end,
        fun() -> throw(boom) end,
        fun() -> ok end
    ],
    [Error, Exit, Throw, Fine] = json_rpc_worker:run_many(Funs, 2000),
    ?assertMatch(#{outcome := {error, {crash, error, boom, _Stack}}}, Error),
    ?assertMatch(#{outcome := {error, {crash, exit, boom, _Stack}}}, Exit),
    ?assertMatch(#{outcome := {error, {crash, throw, boom, _Stack}}}, Throw),
    ?assertMatch(#{outcome := {ok, ok}}, Fine).

worker_kills_at_deadline(_Config) ->
    Self = self(),
    Funs = [
        fun() ->
            Self ! {started, self()},
            timer:sleep(10000)
        end,
        fun() -> quick end
    ],
    [Slow, Quick] = json_rpc_worker:run_many(Funs, 200),
    ?assertMatch(#{outcome := {error, timeout}}, Slow),
    ?assertMatch(#{outcome := {ok, quick}}, Quick),
    %% The abandoned worker must actually be dead, not merely unwaited-for.
    receive
        {started, Pid} ->
            ok = json_rpc_test_support:wait_until(
                fun() -> not is_process_alive(Pid) end, 2000
            )
    after 2000 ->
        erlang:error(worker_never_started)
    end.

%% Results come back positionally, however out of order they complete.
worker_preserves_order(_Config) ->
    Funs = [
        fun() ->
            timer:sleep(150),
            first
        end,
        fun() -> second end,
        fun() ->
            timer:sleep(75),
            third
        end
    ],
    ?assertMatch(
        [#{outcome := {ok, first}}, #{outcome := {ok, second}}, #{outcome := {ok, third}}],
        json_rpc_worker:run_many(Funs, 2000)
    ).
