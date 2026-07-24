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

-module(json_rpc_app_SUITE).

%% Application lifecycle: configuration validation, graceful drain, and
%% telemetry. Cases here start and stop the application themselves, so the
%% suite owns it rather than init_per_suite.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
    invalid_config_fails_start/1,
    every_config_key_has_a_default/1,
    configurable_paths/1,
    restart_is_clean/1,
    in_flight_request_survives_drain/1,
    drain_returns_503_envelope/1,
    websocket_drain_sends_going_away/1,
    telemetry_request_events/1,
    telemetry_parse_error_event/1
]).

-define(PORT, 18085).

all() ->
    [
        invalid_config_fails_start,
        every_config_key_has_a_default,
        configurable_paths,
        restart_is_clean,
        in_flight_request_survives_drain,
        drain_returns_503_envelope,
        websocket_drain_sends_going_away,
        telemetry_request_events,
        telemetry_parse_error_event
    ].

init_per_testcase(invalid_config_fails_start, Config) ->
    _ = application:load(json_rpc),
    Config;
init_per_testcase(configurable_paths, Config) ->
    ok = json_rpc_test_support:start_app(?PORT, #{
        http_path => "/jsonrpc", ws_path => "/socket"
    }),
    Config;
init_per_testcase(_TestCase, Config) ->
    ok = json_rpc_test_support:start_app(?PORT, #{}),
    Config.

end_per_testcase(_TestCase, _Config) ->
    _ = application:stop(json_rpc),
    ok = application:set_env(json_rpc, http_path, "/rpc"),
    ok = application:set_env(json_rpc, ws_path, "/ws"),
    ok.

%%% Cases

%% Misconfiguration must fail the start naming the key, rather than surfacing
%% later as a listener that will not bind.
invalid_config_fails_start(_Config) ->
    %% Start the dependencies first so the only thing that can fail the start
    %% is the validation under test.
    {ok, _Cowboy} = application:ensure_all_started(cowboy),
    {ok, _Telemetry} = application:ensure_all_started(telemetry),
    json_rpc_test_support:with_env(port, 0, fun() ->
        ?assertMatch(
            {error, {{invalid_config, port, 0, _Reason}, _Mfa}},
            application:start(json_rpc)
        )
    end),
    json_rpc_test_support:with_env(http_path, "rpc", fun() ->
        ?assertMatch(
            {error, {{invalid_config, http_path, "rpc", _Reason}, _Mfa}},
            application:start(json_rpc)
        )
    end).

%% `get/1' raises for a key with no default, so every key the config module
%% knows about has to be present in the .app file's env block.
every_config_key_has_a_default(_Config) ->
    lists:foreach(
        fun(Key) -> ?assertNotException(error, badarg, json_rpc_config:get(Key)) end,
        json_rpc_config:keys()
    ).

configurable_paths(_Config) ->
    Conn = json_rpc_test_support:connect(?PORT),
    try
        Request = #{
            jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [42, 23], id => 1
        },
        ?assertMatch(
            #{<<"result">> := 19},
            json_rpc_test_support:rpc(Conn, "/jsonrpc", Request)
        ),
        %% The default path must no longer be routed.
        ?assertMatch(
            {404, _Headers, _Body},
            json_rpc_test_support:post(Conn, "/rpc", json_rpc_test_support:encode(Request))
        )
    after
        gun:close(Conn)
    end.

restart_is_clean(_Config) ->
    ok = application:stop(json_rpc),
    {ok, _Started} = application:ensure_all_started(json_rpc),
    json_rpc_test_support:register_methods(),
    Conn = json_rpc_test_support:connect(?PORT),
    try
        ?assertMatch(
            #{<<"result">> := 2},
            json_rpc_test_support:rpc(Conn, "/rpc", #{
                jsonrpc => <<"2.0">>, method => <<"subtract">>, params => [5, 3], id => 1
            })
        )
    after
        gun:close(Conn)
    end.

%% A request already being served when shutdown starts must be allowed to
%% finish, not cut off mid-response.
in_flight_request_survives_drain(_Config) ->
    Conn = json_rpc_test_support:connect(?PORT),
    Payload = json_rpc_test_support:encode(#{
        jsonrpc => <<"2.0">>, method => <<"slow">>, params => [800], id => 1
    }),
    StreamRef = gun:post(
        Conn, "/rpc", [{<<"content-type">>, <<"application/json">>}], Payload
    ),
    %% Let the handler start sleeping before the teardown begins.
    timer:sleep(150),
    Stopper = spawn_stopper(),
    {response, nofin, 200, _Headers} = gun:await(Conn, StreamRef, 5000),
    {ok, Body} = gun:await_body(Conn, StreamRef, 5000),
    ?assertMatch(
        #{<<"result">> := <<"done">>, <<"id">> := 1},
        json_rpc_test_support:decode(Body)
    ),
    await_stopper(Stopper),
    gun:close(Conn).

%% Work arriving during the drain window is refused with a parseable envelope
%% rather than an empty body.
drain_returns_503_envelope(_Config) ->
    Conn = json_rpc_test_support:connect(?PORT),
    Payload = json_rpc_test_support:encode(#{
        jsonrpc => <<"2.0">>, method => <<"slow">>, params => [600], id => 1
    }),
    _InFlight = gun:post(
        Conn, "/rpc", [{<<"content-type">>, <<"application/json">>}], Payload
    ),
    timer:sleep(150),
    Stopper = spawn_stopper(),
    timer:sleep(150),
    %% A second connection, opened after the drain began, is refused.
    Late = json_rpc_test_support:connect(?PORT),
    {Status, _Headers, Body} = json_rpc_test_support:post(Late, "/rpc", Payload),
    ?assertEqual(503, Status),
    ?assertMatch(
        #{<<"error">> := #{<<"code">> := -32000}},
        json_rpc_test_support:decode(Body)
    ),
    gun:close(Late),
    await_stopper(Stopper),
    gun:close(Conn).

websocket_drain_sends_going_away(_Config) ->
    Conn = json_rpc_test_support:connect(?PORT),
    StreamRef = json_rpc_test_support:ws_upgrade(Conn),
    Stopper = spawn_stopper(),
    ?assertMatch(
        {close, 1001, _Reason},
        json_rpc_test_support:ws_recv_frame(Conn, StreamRef)
    ),
    await_stopper(Stopper),
    gun:close(Conn).

telemetry_request_events(_Config) ->
    Self = self(),
    Events = [[json_rpc, request, stop], [json_rpc, request, exception]],
    ok = telemetry:attach_many(
        make_ref(),
        Events,
        fun(Event, Measurements, Metadata, _Cfg) ->
            Self ! {telemetry, Event, Measurements, Metadata}
        end,
        undefined
    ),
    _ = json_rpc:dispatch(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"subtract">>,
        <<"params">> => [42, 23],
        <<"id">> => 1
    }),
    receive
        {telemetry, [json_rpc, request, stop], Measurements, Metadata} ->
            ?assertMatch(#{duration := _Duration}, Measurements),
            ?assertMatch(#{method := <<"subtract">>, outcome := ok}, Metadata)
    after 5000 ->
        erlang:error(no_request_stop_event)
    end,
    _ = json_rpc:dispatch(#{
        <<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"crash">>, <<"id">> => 2
    }),
    receive
        {telemetry, [json_rpc, request, exception], _M, ExceptionMetadata} ->
            ?assertMatch(#{method := <<"crash">>, kind := crash}, ExceptionMetadata)
    after 5000 ->
        erlang:error(no_request_exception_event)
    end.

telemetry_parse_error_event(_Config) ->
    Self = self(),
    ok = telemetry:attach(
        make_ref(),
        [json_rpc, parse_error],
        fun(_Event, Measurements, Metadata, _Cfg) ->
            Self ! {telemetry_parse_error, Measurements, Metadata}
        end,
        undefined
    ),
    Conn = json_rpc_test_support:connect(?PORT),
    try
        {200, _Headers, _Body} = json_rpc_test_support:post(Conn, "/rpc", <<"{bad json">>),
        receive
            {telemetry_parse_error, Measurements, Metadata} ->
                ?assertEqual(#{count => 1}, Measurements),
                ?assertEqual(#{transport => http}, Metadata)
        after 5000 ->
            erlang:error(no_parse_error_event)
        end
    after
        gun:close(Conn)
    end.

%%% Helpers

%% Stop the application from another process so the case can keep observing
%% the connection while the drain runs.
spawn_stopper() ->
    Self = self(),
    spawn(fun() ->
        ok = application:stop(json_rpc),
        Self ! {stopped, self()}
    end).

await_stopper(Stopper) ->
    receive
        {stopped, Stopper} -> ok
    after 10000 ->
        erlang:error(stop_did_not_complete)
    end.
