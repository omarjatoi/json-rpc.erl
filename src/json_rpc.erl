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

-module(json_rpc).

-moduledoc """
A JSON-RPC 2.0 server over HTTP and WebSocket.

This is the entry point. Everything below it — the registry, the dispatcher,
the transports — is reachable, but this module is the supported surface.

## Getting started

```erlang
{ok, _} = application:ensure_all_started(json_rpc),
ok = json_rpc:register(<<"subtract">>, {my_handlers, subtract}).
```

```sh
curl -sX POST http://localhost:8080/rpc \\
    -H 'content-type: application/json' \\
    -d '{"jsonrpc":"2.0","method":"subtract","params":[42,23],"id":1}'
# {"jsonrpc":"2.0","id":1,"result":19}
```

## Writing a handler

A handler is a `{Module, Function}` pair taking the request's `params`:

```erlang
subtract([A, B]) -> A - B.
```

Its return value becomes the Response:

- `{error, Error}` — an error Response. `Error` may be an error object from
  `m:json_rpc_error`, a `{Code, Message}` pair, or `{Code, Message, Data}`.
- `{ok, Result}` — a successful Response carrying `Result`.
- anything else — a successful Response carrying that term.

Raising works too: `json_rpc_error:throw_error/2,3` reports a protocol error
from anywhere in the call stack, and an outright crash becomes
`-32603 Internal error` with the reason logged rather than sent.

Registering the same function name at arity 2 instead gets the request
context as well — see `t:json_rpc_dispatcher:context/0`, and
`m:json_rpc_ws` for what the `connection_pid` in it is for.

## Declaring methods in configuration

Methods listed in the `methods` environment key are registered at start and
re-registered if the registry ever restarts, which methods registered at
runtime are not:

```erlang
{json_rpc, [{methods, [{<<"subtract">>, {my_handlers, subtract}}]}]}
```

## Configuration

Every key, with its default, set via `application:set_env/3` or `sys.config`
before the application starts.

| Key | Default | Meaning |
| --- | --- | --- |
| `port` | `8080` | TCP port for the listener |
| `http_path` | `"/rpc"` | Route for the HTTP endpoint |
| `ws_path` | `"/ws"` | Route for the WebSocket endpoint |
| `max_connections` | `1024` | Ranch's connection cap |
| `num_acceptors` | `10` | Acceptor processes |
| `max_body_bytes` | `1048576` | Largest accepted HTTP body |
| `idle_timeout_ms` | `60000` | Cowboy idle timeout |
| `request_timeout_ms` | `10000` | Wait for a request line on an idle keep-alive socket |
| `max_keepalive_requests` | `1000` | Requests per keep-alive connection |
| `handler_timeout_ms` | `10000` | Deadline for handler execution |
| `max_batch_size` | `100` | Largest accepted batch |
| `max_methods` | `1024` | Registry size cap |
| `methods` | `[]` | Statically declared methods |
| `drain_timeout_ms` | `5000` | Shutdown drain budget |
| `ws_max_frame_bytes` | `1048576` | Largest accepted WebSocket frame |
| `ws_idle_timeout_ms` | `60000` | WebSocket idle timeout |
| `ws_max_in_flight` | `32` | Concurrent dispatches per WebSocket connection |

## Deployment

The library terminates no TLS and performs no authentication. It is built to
sit plaintext behind an L7 proxy that does both. To authenticate inside the
BEAM, put a `cowboy_middleware` ahead of the handlers.
""".

-export([
    start/0,
    stop/0,
    register/2,
    unregister/1,
    methods/0,
    dispatch/1,
    push/3,
    subscribe/2,
    unsubscribe/2,
    publish/3
]).

-doc "Start the application and everything it depends on.".
-spec start() -> {ok, [atom()]} | {error, term()}.
start() ->
    application:ensure_all_started(json_rpc).

-doc "Stop the application, draining in-flight work first.".
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(json_rpc).

-doc """
Register `Handler` under `Name`, replacing any existing entry.

The module must export the function at arity 1 or 2; that is checked here, so
a typo fails at registration rather than on the first call. Names in the
specification-reserved `rpc.` namespace are refused.
""".
-spec register(json_rpc_methods:method_name(), json_rpc_methods:handler()) ->
    ok | {error, json_rpc_methods:register_error()}.
register(Name, Handler) ->
    json_rpc_methods:register_method(Name, Handler).

-doc "Remove `Name` from the registry.".
-spec unregister(json_rpc_methods:method_name()) -> ok | {error, term()}.
unregister(Name) ->
    json_rpc_methods:unregister_method(Name).

-doc "Every registered method name.".
-spec methods() -> [json_rpc_methods:method_name()].
methods() ->
    json_rpc_methods:list_methods().

-doc """
Dispatch an already-decoded payload directly, bypassing the transports.

Useful for testing handlers, and for carrying JSON-RPC over a transport this
library does not implement.
""".
-spec dispatch(json_rpc_json:value()) -> json_rpc_dispatcher:reply().
dispatch(Payload) ->
    json_rpc_dispatcher:dispatch(Payload).

-doc "Send a Notification to one WebSocket connection. See `json_rpc_ws:push/3`.".
-spec push(pid(), binary(), json_rpc_json:value() | undefined) ->
    ok | {error, json_rpc_json:encode_error()}.
push(ConnectionPid, Method, Params) ->
    json_rpc_ws:push(ConnectionPid, Method, Params).

-doc "Subscribe a WebSocket connection to a topic. See `json_rpc_ws:subscribe/2`.".
-spec subscribe(pid(), json_rpc_ws:topic()) -> ok.
subscribe(ConnectionPid, Topic) ->
    json_rpc_ws:subscribe(ConnectionPid, Topic).

-doc "Unsubscribe a WebSocket connection from a topic.".
-spec unsubscribe(pid(), json_rpc_ws:topic()) -> ok.
unsubscribe(ConnectionPid, Topic) ->
    json_rpc_ws:unsubscribe(ConnectionPid, Topic).

-doc "Broadcast a Notification to a topic's subscribers. See `json_rpc_ws:publish/3`.".
-spec publish(json_rpc_ws:topic(), binary(), json_rpc_json:value() | undefined) ->
    ok | {error, json_rpc_json:encode_error()}.
publish(Topic, Method, Params) ->
    json_rpc_ws:publish(Topic, Method, Params).
