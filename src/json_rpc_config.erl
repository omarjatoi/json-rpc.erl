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

-moduledoc """
Validated access to the `json_rpc` application environment.

There is no process here: every read goes through `application:get_env/2`
and is checked against the key's declared shape. Defaults live in the `env`
block of `json_rpc.app.src`, so `get/1` never has to invent one — an
unknown key is a programming error and raises `badarg`.

`validate_all/0` runs at application start so a misconfigured node fails
immediately, with the offending key named, instead of crashing a listener
later with something less actionable.

Reads are cheap but not free. Transport handlers snapshot the values they
need once at connection setup rather than calling `get/1` per request.
""".

-compile({no_auto_import, [get/1]}).

-export([get/1, keys/0, validate_all/0]).

-doc "Every configuration key understood by the application.".
-type key() ::
    port
    | http_path
    | ws_path
    | max_body_bytes
    | max_connections
    | num_acceptors
    | idle_timeout_ms
    | request_timeout_ms
    | handler_timeout_ms
    | drain_timeout_ms
    | max_keepalive_requests
    | max_methods
    | max_batch_size
    | ws_max_frame_bytes
    | ws_idle_timeout_ms
    | ws_max_in_flight
    | methods.

-export_type([key/0]).

%% Declared shape of each key. `{integer, Min, Max}' bounds are inclusive.
-define(POS_INT, {integer, 1, infinity}).
-define(NON_NEG_INT, {integer, 0, infinity}).

-doc "The list of keys `validate_all/0` walks. Ordered for readable errors.".
-spec keys() -> [key()].
keys() ->
    [
        port,
        http_path,
        ws_path,
        max_body_bytes,
        max_connections,
        num_acceptors,
        idle_timeout_ms,
        request_timeout_ms,
        handler_timeout_ms,
        drain_timeout_ms,
        max_keepalive_requests,
        max_methods,
        max_batch_size,
        ws_max_frame_bytes,
        ws_idle_timeout_ms,
        ws_max_in_flight,
        methods
    ].

-doc """
Fetch a validated configuration value.

Raises `badarg` for a key the application does not define, and
`{invalid_config, Key, Value, Reason}` for a defined key holding a value
that fails validation.
""".
-spec get(key()) -> term().
get(Key) ->
    case application:get_env(json_rpc, Key) of
        {ok, Value} -> validate(Key, Value);
        undefined -> erlang:error(badarg, [Key])
    end.

-doc """
Validate every key, raising on the first bad one.

Called from the application start callback in `m:json_rpc_app`, so
misconfiguration fails the start rather than a request.
""".
-spec validate_all() -> ok.
validate_all() ->
    lists:foreach(fun(Key) -> _ = get(Key) end, keys()),
    ok.

%%% Internal

%% The declared shape of each key, in one place so adding a knob is a
%% one-line change here plus a default in json_rpc.app.src.
shape(port) -> {integer, 1, 65535};
shape(http_path) -> path;
shape(ws_path) -> path;
shape(max_body_bytes) -> ?POS_INT;
shape(max_connections) -> ?POS_INT;
shape(num_acceptors) -> ?POS_INT;
shape(idle_timeout_ms) -> ?POS_INT;
shape(request_timeout_ms) -> ?POS_INT;
shape(handler_timeout_ms) -> ?POS_INT;
shape(drain_timeout_ms) -> ?NON_NEG_INT;
shape(max_keepalive_requests) -> ?POS_INT;
shape(max_methods) -> ?POS_INT;
shape(max_batch_size) -> ?POS_INT;
shape(ws_max_frame_bytes) -> ?POS_INT;
shape(ws_idle_timeout_ms) -> ?POS_INT;
shape(ws_max_in_flight) -> ?POS_INT;
shape(methods) -> method_list;
shape(_Other) -> unknown.

validate(Key, Value) ->
    case shape(Key) of
        unknown -> erlang:error(badarg, [Key]);
        Shape -> check(Shape, Key, Value)
    end.

check({integer, Min, Max}, Key, Value) when is_integer(Value) ->
    case Value >= Min andalso (Max =:= infinity orelse Value =< Max) of
        true -> Value;
        false -> bad(Key, Value, range_message(Min, Max))
    end;
check({integer, Min, Max}, Key, Value) ->
    bad(Key, Value, range_message(Min, Max));
%% Cowboy's router wants a string path; accept only absolute ones so a typo
%% shows up at boot instead of as a silent 404.
check(path, _Key, [$/ | _] = Value) ->
    Value;
check(path, Key, Value) ->
    bad(Key, Value, <<"must be an absolute path string, e.g. \"/rpc\"">>);
check(method_list, Key, Value) when is_list(Value) ->
    case lists:all(fun is_method_spec/1, Value) of
        true -> Value;
        false -> bad(Key, Value, <<"must be a list of {Name :: binary(), {Module, Function}}">>)
    end;
check(method_list, Key, Value) ->
    bad(Key, Value, <<"must be a list of {Name :: binary(), {Module, Function}}">>).

is_method_spec({Name, {Module, Function}}) when
    is_binary(Name), is_atom(Module), is_atom(Function)
->
    true;
is_method_spec(_Other) ->
    false.

range_message(Min, infinity) ->
    iolist_to_binary(io_lib:format("must be an integer >= ~p", [Min]));
range_message(Min, Max) ->
    iolist_to_binary(io_lib:format("must be an integer in ~p..~p", [Min, Max])).

-spec bad(key(), term(), binary()) -> no_return().
bad(Key, Value, Reason) ->
    erlang:error({invalid_config, Key, Value, Reason}).
