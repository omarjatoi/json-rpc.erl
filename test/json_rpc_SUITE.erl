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

-export([
    test_rpc_call_positional_params/1,
    test_notification/1,
    test_non_existent_method/1,
    test_invalid_json/1,
    test_invalid_request_object/1,
    test_batch_invalid_json/1,
    test_empty_array/1,
    test_invalid_batch/1,
    test_rpc_call_batch/1,
    test_notification_batch/1,
    test_authentication/1,
    test_explicit_null_id_is_call/1,
    test_all_notification_batch_no_response/1,
    test_reserved_method_name/1,
    test_invalid_params_type/1,
    test_handler_throws_jsonrpc_error/1
]).

-define(PORT, 8080).
-define(HOST, "localhost").

%%% Common Test callbacks

all() ->
    [
        test_rpc_call_positional_params,
        test_notification,
        test_non_existent_method,
        test_invalid_json,
        test_invalid_request_object,
        test_batch_invalid_json,
        test_empty_array,
        test_invalid_batch,
        test_rpc_call_batch,
        test_notification_batch,
        test_authentication,
        test_explicit_null_id_is_call,
        test_all_notification_batch_no_response,
        test_reserved_method_name,
        test_invalid_params_type,
        test_handler_throws_jsonrpc_error
    ].

init_per_suite(Config) ->
    Pid = start_server(),
    [{server, Pid} | Config].

end_per_suite(Config) ->
    Pid = ?config(server, Config),
    stop_server(Pid),
    ok.

init_per_testcase(_TestCase, Config) ->
    Client = setup_client(),
    [{client, Client} | Config].

end_per_testcase(_TestCase, Config) ->
    Client = ?config(client, Config),
    cleanup_client(Client),
    ok.

%%% Fixtures

start_server() ->
    case json_rpc_server:start_link(?PORT) of
        {ok, Pid} ->
            %% start_link links the server to the init_per_suite process. CT
            %% runs each test case in a fresh process, so the suite process
            %% exits between init_per_suite and init_per_testcase and would
            %% take the linked server down with it. Unlink so the server
            %% survives across test cases; end_per_suite stops it explicitly.
            unlink(Pid),
            register_methods(Pid),
            Pid;
        {error, Reason} ->
            erlang:error({server_start_failed, Reason})
    end.

register_methods(Pid) ->
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
        fun({Name, Fun}) ->
            gen_server:call(Pid, {register_method, Name, Fun})
        end,
        Methods
    ).

stop_server(Pid) ->
    gen_server:stop(Pid).

setup_client() ->
    case json_rpc_client:connect(?HOST, ?PORT) of
        {ok, Client} ->
            Client;
        {error, Reason} ->
            erlang:error({client_connection_failed, Reason})
    end.

cleanup_client(Client) ->
    json_rpc_client:close(Client).

%%% Test cases

test_rpc_call_positional_params(Config) ->
    Client = ?config(client, Config),
    ?assertEqual({ok, 19, 1}, json_rpc_client:call(Client, <<"subtract">>, [42, 23], 1)),
    ?assertEqual({ok, -19, 2}, json_rpc_client:call(Client, <<"subtract">>, [23, 42], 2)).

test_notification(Config) ->
    Client = ?config(client, Config),
    ?assertEqual(ok, json_rpc_client:notify(Client, <<"update">>, [1, 2, 3, 4, 5])),
    ?assertEqual(ok, json_rpc_client:notify(Client, <<"foobar">>, [])).

test_non_existent_method(Config) ->
    Client = ?config(client, Config),
    ?assertEqual(
        {error, #{<<"code">> => -32601, <<"message">> => <<"Method not found">>}, <<"1">>},
        json_rpc_client:call(Client, <<"foobar">>, [], <<"1">>)
    ).

test_invalid_json(Config) ->
    Client = ?config(client, Config),
    ?assertEqual(
        {error, #{<<"code">> => -32700, <<"message">> => <<"Parse error">>}, null},
        json_rpc_client:raw_request(
            Client,
            <<"{\"jsonrpc\": \"2.0\", \"method\": \"foobar\", \"params\": \"bar\", \"baz]">>
        )
    ).

test_invalid_request_object(Config) ->
    Client = ?config(client, Config),
    %% A non-binary method (here, a number) is an invalid Request object.
    %% We use raw_request to bypass the client-side is_binary(Method) guard.
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}">>,
    ?assertEqual(
        {error, #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}, null},
        json_rpc_client:raw_request(Client, Payload)
    ).

test_batch_invalid_json(Config) ->
    Client = ?config(client, Config),
    ?assertEqual(
        {error, #{<<"code">> => -32700, <<"message">> => <<"Parse error">>}, null},
        json_rpc_client:raw_request(
            Client,
            <<"[{\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"},{\"jsonrpc\": \"2.0\", \"method\"]">>
        )
    ).

test_empty_array(Config) ->
    Client = ?config(client, Config),
    %% Per spec, empty batch must return a single Invalid Request object,
    %% NOT an array. batch/2 short-circuits on [] so we use raw_request.
    ?assertEqual(
        {error, #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}, null},
        json_rpc_client:raw_request(Client, <<"[]">>)
    ).

test_invalid_batch(Config) ->
    Client = ?config(client, Config),
    ?assertEqual(
        {ok, [
            {error, #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}, null},
            {error, #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}, null},
            {error, #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}, null}
        ]},
        json_rpc_client:raw_request(Client, <<"[1,2,3]">>)
    ).

test_rpc_call_batch(Config) ->
    Client = ?config(client, Config),
    Batch = [
        {<<"sum">>, [1, 2, 4], <<"1">>},
        {<<"subtract">>, [42, 23], <<"2">>},
        {<<"foo">>, [], <<"9">>}
    ],
    ?assertEqual(
        {ok, [
            {ok, 7, <<"1">>},
            {ok, 19, <<"2">>},
            {error, #{<<"code">> => -32601, <<"message">> => <<"Method not found">>}, <<"9">>}
        ]},
        json_rpc_client:batch(Client, Batch)
    ).

test_notification_batch(Config) ->
    Client = ?config(client, Config),
    Batch = [
        {<<"notify_sum">>, [1, 2, 4], undefined},
        {<<"notify_hello">>, [7], undefined}
    ],
    ?assertEqual(ok, json_rpc_client:batch(Client, Batch)).

test_authentication(Config) ->
    Client = ?config(client, Config),
    AuthFun = fun
        (#{<<"auth">> := <<"secret_token">>}) -> ok;
        (_) -> error
    end,
    json_rpc_server:set_auth(AuthFun),

    AuthClient = json_rpc_client:set_auth(Client, <<"secret_token">>),

    ?assertEqual(
        {error, #{<<"code">> => -32000, <<"message">> => <<"Authentication failed">>}, 1},
        json_rpc_client:call(Client, <<"subtract">>, [42, 23], 1)
    ),

    ?assertEqual(
        {ok, 19, 2},
        json_rpc_client:call(AuthClient, <<"subtract">>, [42, 23], 2)
    ),

    %% Reset auth so subsequent tests in the suite are unaffected.
    json_rpc_server:set_auth(fun(_) -> ok end).

%% Per spec, an explicit "id": null is a (discouraged but valid) call;
%% the server must respond with id: null, NOT silently drop it as if it
%% were a notification.
test_explicit_null_id_is_call(Config) ->
    Client = ?config(client, Config),
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42,23], \"id\": null}">>,
    ?assertEqual(
        {ok, 19, null},
        json_rpc_client:raw_request(Client, Payload)
    ).

%% A batch consisting entirely of notifications must produce no response
%% on the wire. Use a fresh socket and a short recv timeout to assert
%% that nothing comes back.
test_all_notification_batch_no_response(_Config) ->
    {ok, Socket} = gen_tcp:connect(?HOST, ?PORT, [binary, {packet, 0}, {active, false}]),
    Payload =
        <<"[",
          "{\"jsonrpc\": \"2.0\", \"method\": \"notify_sum\", \"params\": [1,2,4]},",
          "{\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", \"params\": [7]}",
          "]">>,
    ok = gen_tcp:send(Socket, Payload),
    ?assertEqual({error, timeout}, gen_tcp:recv(Socket, 0, 200)),
    gen_tcp:close(Socket).

%% Method names beginning with "rpc." are reserved for rpc-internal use
%% and must be rejected with -32601 Method not found when invoked from a
%% client.
test_reserved_method_name(Config) ->
    Client = ?config(client, Config),
    ?assertEqual(
        {error, #{<<"code">> => -32601, <<"message">> => <<"Method not found">>}, <<"r1">>},
        json_rpc_client:call(Client, <<"rpc.foo">>, [], <<"r1">>)
    ).

%% "params", when present, must be an array or object. Anything else
%% (here, a number) yields -32602 Invalid params.
test_invalid_params_type(Config) ->
    Client = ?config(client, Config),
    Payload = <<"{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": 42, \"id\": \"p1\"}">>,
    ?assertEqual(
        {error, #{<<"code">> => -32602, <<"message">> => <<"Invalid params">>}, <<"p1">>},
        json_rpc_client:raw_request(Client, Payload)
    ).

%% Handlers may surface application-level errors by throwing the
%% structured tuple {jsonrpc_error, Code, Msg[, Data]}; the resulting
%% error object must reach the client verbatim, including the Data field.
test_handler_throws_jsonrpc_error(Config) ->
    Client = ?config(client, Config),
    ?assertEqual(
        {error,
            #{
                <<"code">> => -32602,
                <<"message">> => <<"bad arg">>,
                <<"data">> => #{<<"detail">> => <<"oops">>}
            },
            <<"e1">>},
        json_rpc_client:call(Client, <<"throw_error">>, [], <<"e1">>)
    ).
