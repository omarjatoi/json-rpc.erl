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

-module(json_rpc_config).

%% Thin synchronous accessor for the json_rpc application's environment.
%% No process; just reads `application:get_env/2' and validates each value
%% against its expected type and constraints. Defaults live in the app
%% file (`json_rpc.app.src') under the `env' key.

-export([get/1, get/2, validate_all/0]).

-type key() ::
    port
    | max_body_bytes
    | max_connections
    | num_acceptors
    | idle_timeout_ms
    | request_timeout_ms
    | drain_timeout_ms
    | max_methods
    | ws_max_frame_bytes
    | ws_idle_timeout_ms.

-export_type([key/0]).

%% @doc Fetch a validated config value. Raises `error(badarg)' if the key is
%% unknown to the application env (i.e. has no default).
-spec get(key()) -> term().
get(Key) ->
    case application:get_env(json_rpc, Key) of
        {ok, Value} ->
            validate(Key, Value);
        undefined ->
            erlang:error(badarg, [Key])
    end.

%% @doc Fetch a validated config value, returning `Default' if the key is
%% absent. The default itself is validated.
-spec get(key(), term()) -> term().
get(Key, Default) ->
    case application:get_env(json_rpc, Key) of
        {ok, Value} -> validate(Key, Value);
        undefined -> validate(Key, Default)
    end.

%% @doc Walk every known key, validating each. Returns `ok' or raises with
%% `{invalid_config, Key, Value, Reason}'. Call this before starting the
%% supervisor so misconfiguration fails fast with a clear error.
-spec validate_all() -> ok.
validate_all() ->
    Keys = [
        port,
        max_body_bytes,
        max_connections,
        num_acceptors,
        idle_timeout_ms,
        request_timeout_ms,
        drain_timeout_ms,
        max_methods,
        ws_max_frame_bytes,
        ws_idle_timeout_ms
    ],
    lists:foreach(fun(K) -> _ = ?MODULE:get(K) end, Keys),
    ok.

%% Internal

validate(port, V) when is_integer(V), V >= 1, V =< 65535 ->
    V;
validate(port, V) ->
    bad(port, V, <<"must be an integer in 1..65535">>);
validate(max_body_bytes, V) when is_integer(V), V > 0 ->
    V;
validate(max_body_bytes, V) ->
    bad(max_body_bytes, V, <<"must be a positive integer">>);
validate(max_connections, V) when is_integer(V), V > 0 ->
    V;
validate(max_connections, V) ->
    bad(max_connections, V, <<"must be a positive integer">>);
validate(num_acceptors, V) when is_integer(V), V > 0 ->
    V;
validate(num_acceptors, V) ->
    bad(num_acceptors, V, <<"must be a positive integer">>);
validate(idle_timeout_ms, V) when is_integer(V), V > 0 ->
    V;
validate(idle_timeout_ms, V) ->
    bad(idle_timeout_ms, V, <<"must be a positive integer">>);
validate(request_timeout_ms, V) when is_integer(V), V > 0 ->
    V;
validate(request_timeout_ms, V) ->
    bad(request_timeout_ms, V, <<"must be a positive integer">>);
validate(drain_timeout_ms, V) when is_integer(V), V >= 0 ->
    V;
validate(drain_timeout_ms, V) ->
    bad(drain_timeout_ms, V, <<"must be a non-negative integer">>);
validate(max_methods, V) when is_integer(V), V > 0 ->
    V;
validate(max_methods, V) ->
    bad(max_methods, V, <<"must be a positive integer">>);
validate(ws_max_frame_bytes, V) when is_integer(V), V > 0 ->
    V;
validate(ws_max_frame_bytes, V) ->
    bad(ws_max_frame_bytes, V, <<"must be a positive integer">>);
validate(ws_idle_timeout_ms, V) when is_integer(V), V > 0 ->
    V;
validate(ws_idle_timeout_ms, V) ->
    bad(ws_idle_timeout_ms, V, <<"must be a positive integer">>);
validate(Key, _V) ->
    erlang:error(badarg, [Key]).

-spec bad(key(), term(), binary()) -> no_return().
bad(Key, Value, Reason) ->
    erlang:error({invalid_config, Key, Value, Reason}).
