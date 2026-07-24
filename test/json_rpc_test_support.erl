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

-module(json_rpc_test_support).

%% Shared plumbing for the suites: application lifecycle, method
%% registration, and thin wrappers over `gun' so the cases read as protocol
%% assertions rather than as HTTP bookkeeping.

-export([
    start_app/2,
    stop_app/0,
    register_methods/0,
    method_specs/0,
    encode/1,
    decode/1,
    connect/1,
    post/3,
    post/4,
    rpc/3,
    ws_upgrade/1,
    ws_upgrade/2,
    ws_send/3,
    ws_recv/2,
    ws_recv_frame/2,
    ws_expect_silence/2,
    with_env/3,
    wait_until/2
]).

-define(HOST, "localhost").

%%% Application lifecycle

%% The app has to be loaded before set_env, or ensure_all_started reloads the
%% .app file and puts the packaged defaults back over the top.
start_app(Port, ExtraEnv) ->
    _ = application:load(json_rpc),
    ok = application:set_env(json_rpc, port, Port),
    maps:foreach(fun(K, V) -> ok = application:set_env(json_rpc, K, V) end, ExtraEnv),
    {ok, _Started} = application:ensure_all_started(json_rpc),
    {ok, _GunStarted} = application:ensure_all_started(gun),
    register_methods(),
    ok.

stop_app() ->
    application:stop(json_rpc).

method_specs() ->
    [
        {<<"subtract">>, {json_rpc_test_methods, subtract}},
        {<<"sum">>, {json_rpc_test_methods, sum}},
        {<<"get_data">>, {json_rpc_test_methods, get_data}},
        {<<"echo">>, {json_rpc_test_methods, echo}},
        {<<"update">>, {json_rpc_test_methods, update}},
        {<<"notify_sum">>, {json_rpc_test_methods, notify_sum}},
        {<<"notify_hello">>, {json_rpc_test_methods, notify_hello}},
        {<<"slow">>, {json_rpc_test_methods, slow}},
        {<<"crash">>, {json_rpc_test_methods, crash}},
        {<<"crash_exit">>, {json_rpc_test_methods, crash_exit}},
        {<<"crash_throw">>, {json_rpc_test_methods, crash_throw}},
        {<<"throw_error">>, {json_rpc_test_methods, throw_error}},
        {<<"throw_error_object">>, {json_rpc_test_methods, throw_error_object}},
        {<<"throw_reserved">>, {json_rpc_test_methods, throw_reserved}},
        {<<"throw_server_error">>, {json_rpc_test_methods, throw_server_error}},
        {<<"return_error_pair">>, {json_rpc_test_methods, return_error_pair}},
        {<<"return_error_triple">>, {json_rpc_test_methods, return_error_triple}},
        {<<"return_error_object">>, {json_rpc_test_methods, return_error_object}},
        {<<"return_ok_tuple">>, {json_rpc_test_methods, return_ok_tuple}},
        {<<"unencodable">>, {json_rpc_test_methods, unencodable}},
        {<<"unencodable_in_batch">>, {json_rpc_test_methods, unencodable_in_batch}},
        {<<"context">>, {json_rpc_test_methods, context}},
        {<<"subscribe_self">>, {json_rpc_test_methods, subscribe_self}}
    ].

register_methods() ->
    lists:foreach(
        fun({Name, Handler}) -> ok = json_rpc:register(Name, Handler) end,
        method_specs()
    ).

%%% JSON

encode(Term) ->
    {ok, Binary} = json_rpc_json:encode(Term),
    Binary.

decode(Binary) ->
    {ok, Term} = json_rpc_json:decode(Binary),
    Term.

%%% HTTP

connect(Port) ->
    {ok, Pid} = gun:open(?HOST, Port, #{transport => tcp, protocols => [http], retry => 0}),
    {ok, http} = gun:await_up(Pid, 5000),
    Pid.

json_headers() ->
    [{<<"content-type">>, <<"application/json">>}, {<<"accept">>, <<"application/json">>}].

%% POST a raw body, returning the status, headers, and undecoded body.
post(Conn, Path, Body) ->
    post(Conn, Path, Body, json_headers()).

post(Conn, Path, Body, Headers) ->
    StreamRef = gun:post(Conn, Path, Headers, Body),
    case gun:await(Conn, StreamRef, 5000) of
        {response, fin, Status, RespHeaders} ->
            {Status, RespHeaders, <<>>};
        {response, nofin, Status, RespHeaders} ->
            {ok, RespBody} = gun:await_body(Conn, StreamRef, 5000),
            {Status, RespHeaders, RespBody}
    end.

%% Issue a payload as an Erlang term and return the decoded reply, or
%% `no_response' when the server answered 204.
rpc(Conn, Path, Term) ->
    case post(Conn, Path, encode(Term)) of
        {204, _Headers, _Body} -> no_response;
        {200, _Headers, Body} -> decode(Body)
    end.

%%% WebSocket

ws_upgrade(Conn) ->
    ws_upgrade(Conn, []).

ws_upgrade(Conn, Headers) ->
    StreamRef = gun:ws_upgrade(Conn, "/ws", Headers),
    receive
        {gun_upgrade, Conn, StreamRef, [<<"websocket">>], _RespHeaders} ->
            StreamRef;
        {gun_response, Conn, StreamRef, _IsFin, Status, _RespHeaders} ->
            erlang:error({ws_upgrade_failed, Status});
        {gun_error, Conn, StreamRef, Reason} ->
            erlang:error({ws_upgrade_error, Reason})
    after 5000 ->
        erlang:error(ws_upgrade_timeout)
    end.

ws_send(Conn, StreamRef, Term) ->
    gun:ws_send(Conn, StreamRef, {text, encode(Term)}).

%% Receive one text frame and decode it.
ws_recv(Conn, StreamRef) ->
    case gun:await(Conn, StreamRef, 5000) of
        {ws, {text, Binary}} -> decode(Binary);
        Other -> erlang:error({unexpected_ws_message, Other})
    end.

%% Receive one frame of any kind, undecoded.
ws_recv_frame(Conn, StreamRef) ->
    receive
        {gun_ws, Conn, StreamRef, Frame} -> Frame
    after 5000 ->
        erlang:error(ws_frame_timeout)
    end.

ws_expect_silence(Conn, StreamRef) ->
    case gun:await(Conn, StreamRef, 300) of
        {error, timeout} -> ok;
        Other -> erlang:error({unexpected_frame, Other})
    end.

%%% Misc

%% Run `Fun' with an application environment key temporarily changed.
with_env(Key, Value, Fun) ->
    Saved = application:get_env(json_rpc, Key),
    ok = application:set_env(json_rpc, Key, Value),
    try
        Fun()
    after
        case Saved of
            {ok, Previous} -> application:set_env(json_rpc, Key, Previous);
            undefined -> application:unset_env(json_rpc, Key)
        end
    end.

%% Poll until `Fun' returns true, or fail after the budget in milliseconds.
wait_until(Fun, Budget) when Budget =< 0 ->
    case Fun() of
        true -> ok;
        false -> erlang:error(condition_not_met)
    end;
wait_until(Fun, Budget) ->
    case Fun() of
        true ->
            ok;
        false ->
            timer:sleep(25),
            wait_until(Fun, Budget - 25)
    end.
