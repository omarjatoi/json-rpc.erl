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

-module(json_rpc_telemetry).

-moduledoc """
`telemetry` events emitted by the server.

Attach with `telemetry:attach/4` or `telemetry:attach_many/4`. Durations are
in **microseconds** — unlike the `telemetry` convention of native units,
because everything measured here is already sampled in microseconds.

`[json_rpc, request, stop]`
: Measures `duration`. Metadata: `method`, `transport`, `request_id`,
  `outcome`, `error_code`.

`[json_rpc, request, exception]`
: Measures `duration`. Metadata: `method`, `transport`, `request_id`, `kind`.

`[json_rpc, batch, stop]`
: Measures `duration` and `size`. Metadata: `transport`, `request_id`.

`[json_rpc, parse_error]`
: Measures `count`. Metadata: `transport`.

`[json_rpc, ws, connection, open]`
: Measures `count`. Metadata: `peer`.

`[json_rpc, ws, connection, close]`
: Measures `count`. Metadata: `reason`.

`outcome` is `ok` or `error`; `error_code` is the JSON-RPC code on an error
and `undefined` on success. `kind` is `timeout` or `crash`. A request that
times out or crashes emits `exception`, never `stop`.

Requests are reported per method call, so one batch of ten calls produces
ten `request` events and one `batch` event.
""".

-export([
    request_stop/4,
    request_exception/4,
    batch_stop/3,
    parse_error/1,
    ws_connection_open/1,
    ws_connection_close/1
]).

-type transport() :: http | websocket | internal.
-type duration() :: non_neg_integer().

-export_type([transport/0, duration/0]).

-doc "A method call completed, successfully or with a JSON-RPC error.".
-spec request_stop(
    Method :: binary(),
    Duration :: duration(),
    Outcome :: ok | {error, json_rpc_error:code()},
    Context :: map()
) -> ok.
request_stop(Method, Duration, Outcome, Context) ->
    execute(
        [json_rpc, request, stop],
        #{duration => Duration},
        request_metadata(Method, Context, outcome_metadata(Outcome))
    ).

-doc "A method call was killed at the deadline or died with an exception.".
-spec request_exception(
    Method :: binary(),
    Duration :: duration(),
    Kind :: timeout | crash,
    Context :: map()
) -> ok.
request_exception(Method, Duration, Kind, Context) ->
    execute(
        [json_rpc, request, exception],
        #{duration => Duration},
        request_metadata(Method, Context, #{kind => Kind})
    ).

-doc "A batch finished. `Size` counts the elements sent, valid or not.".
-spec batch_stop(Size :: non_neg_integer(), Duration :: duration(), Context :: map()) -> ok.
batch_stop(Size, Duration, Context) ->
    execute(
        [json_rpc, batch, stop],
        #{duration => Duration, size => Size},
        #{
            transport => maps:get(transport, Context, internal),
            request_id => maps:get(request_id, Context, undefined)
        }
    ).

-doc "A payload could not be decoded as JSON.".
-spec parse_error(transport()) -> ok.
parse_error(Transport) ->
    execute([json_rpc, parse_error], #{count => 1}, #{transport => Transport}).

-doc "A WebSocket connection was established.".
-spec ws_connection_open(Peer :: term()) -> ok.
ws_connection_open(Peer) ->
    execute([json_rpc, ws, connection, open], #{count => 1}, #{peer => Peer}).

-doc "A WebSocket connection was torn down.".
-spec ws_connection_close(Reason :: term()) -> ok.
ws_connection_close(Reason) ->
    execute([json_rpc, ws, connection, close], #{count => 1}, #{reason => Reason}).

%%% Internal

request_metadata(Method, Context, Extra) ->
    maps:merge(
        #{
            method => Method,
            transport => maps:get(transport, Context, internal),
            request_id => maps:get(request_id, Context, undefined)
        },
        Extra
    ).

outcome_metadata(ok) ->
    #{outcome => ok, error_code => undefined};
outcome_metadata({error, Code}) ->
    #{outcome => error, error_code => Code}.

%% A handler attached by the operator runs inline; telemetry itself already
%% detaches one that raises, so nothing here needs to guard the call.
execute(Event, Measurements, Metadata) ->
    telemetry:execute(Event, Measurements, Metadata).
