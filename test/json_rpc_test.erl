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

-module(json_rpc_test).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 8080).
-define(HOST, "localhost").

%%% Test fixtures

start_server() ->
    case json_rpc_server:start_link(?PORT) of
        {ok, Pid} ->
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
        {<<"update">>, fun(_) -> ok end}
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

%%% Test generators

json_rpc_test_() ->
    {setup, fun start_server/0, fun stop_server/1,
        {foreach, fun setup_client/0, fun cleanup_client/1, [
            % fun test_rpc_call_positional_params/1,
            % fun test_notification/1,
            % fun test_non_existent_method/1,
            % fun test_invalid_json/1,
            % fun test_invalid_request_object/1,
            % fun test_batch_invalid_json/1,
            fun test_empty_array/1
            % fun test_invalid_batch/1,
            % fun test_rpc_call_batch/1,
            % fun test_notification_batch/1,
            % fun test_authentication/1
        ]}}.

%%% Individual tests

test_rpc_call_positional_params(Client) ->
    [
        ?_assertEqual({ok, 19, 1}, json_rpc_client:call(Client, <<"subtract">>, [42, 23], 1)),
        ?_assertEqual({ok, -19, 2}, json_rpc_client:call(Client, <<"subtract">>, [23, 42], 2))
    ].

test_notification(Client) ->
    [
        ?_assertEqual(ok, json_rpc_client:notify(Client, <<"update">>, [1, 2, 3, 4, 5])),
        ?_assertEqual(ok, json_rpc_client:notify(Client, <<"foobar">>, []))
    ].

test_non_existent_method(Client) ->
    [
        ?_assertEqual(
            {error, #{<<"code">> => -32601, <<"message">> => <<"Method not found">>}, <<"1">>},
            json_rpc_client:call(Client, <<"foobar">>, [], <<"1">>)
        )
    ].

test_invalid_json(Client) ->
    [
        ?_assertEqual(
            {error, parse_error},
            json_rpc_client:send_and_receive(
                Client,
                <<"{\"jsonrpc\": \"2.0\", \"method\": \"foobar, \"params\": \"bar\", \"baz]">>
            )
        )
    ].

test_invalid_request_object(Client) ->
    [
        ?_assertEqual(
            {error, #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}, null},
            json_rpc_client:call(Client, 1, <<"bar">>, null)
        )
    ].

test_batch_invalid_json(Client) ->
    [
        ?_assertEqual(
            {error, parse_error},
            json_rpc_client:send_and_receive(
                Client,
                <<"[{\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"},{\"jsonrpc\": \"2.0\", \"method\"]">>
            )
        )
    ].

test_empty_array(Client) ->
    [
        ?_assertEqual(
            {ok, [{error, #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}, null}]},
            json_rpc_client:batch(Client, [])
        )
    ].

test_invalid_batch(Client) ->
    [
        ?_assertEqual(
            {ok, [
                {error, #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}, null},
                {error, #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}, null},
                {error, #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}, null}
            ]},
            json_rpc_client:send_and_receive(Client, <<"[1,2,3]">>)
        )
    ].

test_rpc_call_batch(Client) ->
    Batch = [
        {<<"sum">>, [1, 2, 4], <<"1">>},
        {<<"notify_hello">>, [7], undefined},
        {<<"subtract">>, [42, 23], <<"2">>},
        {<<"foo.get">>, #{<<"name">> => <<"myself">>}, <<"5">>},
        {<<"get_data">>, [], <<"9">>}
    ],
    [
        ?_assertEqual(
            {ok, [
                {ok, 7, <<"1">>},
                {ok, 19, <<"2">>},
                {error, #{<<"code">> => -32601, <<"message">> => <<"Method not found">>}, <<"5">>},
                {ok, [<<"hello">>, 5], <<"9">>}
            ]},
            json_rpc_client:batch(Client, Batch)
        )
    ].

test_notification_batch(Client) ->
    Batch = [
        {<<"notify_sum">>, [1, 2, 4], undefined},
        {<<"notify_hello">>, [7], undefined}
    ],
    [
        ?_assertEqual(ok, json_rpc_client:batch(Client, Batch))
    ].

test_authentication(Client) ->
    AuthFun = fun
        (#{<<"auth">> := <<"secret_token">>}) -> ok;
        (_) -> error
    end,
    json_rpc_server:set_auth(AuthFun),

    AuthClient = json_rpc_client:set_auth(Client, <<"secret_token">>),

    Tests = [
        ?_assertEqual(
            {error, #{<<"code">> => -32000, <<"message">> => <<"Authentication failed">>}, 1},
            json_rpc_client:call(Client, <<"subtract">>, [42, 23], 1)
        ),

        ?_assertEqual(
            {ok, 19, 2},
            json_rpc_client:call(AuthClient, <<"subtract">>, [42, 23], 2)
        )
    ],

    json_rpc_server:set_auth(undefined),
    Tests.
