# json-rpc

A [JSON-RPC 2.0](https://www.jsonrpc.org/specification) server in Erlang,
served over HTTP and WebSocket by [Cowboy](https://github.com/ninenines/cowboy).

- **Complete protocol coverage** — calls, notifications, batches, and every
  error the specification defines, including the awkward corners: id echoing
  on malformed envelopes, `"id": null` as a call rather than a notification,
  and silence for notifications whatever the handler does.
- **No native dependencies.** JSON is handled by OTP's own `json` module, so
  there is no NIF in the tree and no C toolchain at build time.
- **Failure containment.** Every handler runs in its own monitored process
  under a deadline. A crash, a hang, or a result JSON cannot represent
  degrades one call to `-32603` and leaves the connection serving.
- **Observable.** `telemetry` events per request, batch, and connection, plus
  a correlation id on every handler log line.

Requires OTP 27 or newer.

## Install

```erlang
{deps, [{json_rpc, "1.0.0"}]}.
```

## Quick start

```erlang
{ok, _} = application:ensure_all_started(json_rpc),
ok = json_rpc:register(<<"subtract">>, {my_handlers, subtract}).
```

```erlang
-module(my_handlers).
-export([subtract/1]).

subtract([A, B]) -> A - B.
```

```sh
curl -sX POST http://localhost:8080/rpc \
    -H 'content-type: application/json' \
    -d '{"jsonrpc":"2.0","method":"subtract","params":[42,23],"id":1}'
# {"jsonrpc":"2.0","id":1,"result":19}
```

## Endpoints

| Route | Purpose |
| --- | --- |
| `POST /rpc` | One-shot call, notification, or batch |
| `GET /ws` | Persistent JSON-RPC channel over WebSocket text frames |

Both paths are configurable (`http_path`, `ws_path`).

A well-formed payload always answers `200 OK`, with JSON-RPC errors in the
body — a `-32601` is a successful HTTP exchange carrying an application-level
failure. The other statuses concern the HTTP request itself, and every one of
them still carries a JSON-RPC error envelope so clients parse one shape:

| Status | When |
| --- | --- |
| `204 No Content` | Notification, or a batch of nothing but notifications |
| `405 Method Not Allowed` | Anything other than `POST` |
| `413 Content Too Large` | Body exceeded `max_body_bytes` |
| `415 Unsupported Media Type` | `Content-Type` was not JSON |
| `503 Service Unavailable` | Server is draining for shutdown |

The WebSocket endpoint takes payloads as text frames and answers with text
frames; notifications produce no frame at all. Binary frames close the
connection with `1003`, since JSON-RPC is defined over text and silently
dropping the frame would leave the client waiting forever.

## Writing handlers

A handler is a `{Module, Function}` pair called with the request's `params`.
Its return value becomes the response:

| Return | Response |
| --- | --- |
| `{error, Error}` | An error response |
| `{ok, Result}` | A success response carrying `Result` |
| anything else | A success response carrying that term |

`Error` may be an error object from `json_rpc_error`, a `{Code, Message}`
pair, or `{Code, Message, Data}`. Bare tuples are not JSON-encodable, so
using `{ok, _}` and `{error, _}` as control costs nothing a handler could
legitimately want to return.

Raising works too, from anywhere in the call stack:

```erlang
withdraw(#{<<"amount">> := Amount}) when Amount =< 0 ->
    json_rpc_error:throw_error(-32000, <<"amount must be positive">>);
withdraw(#{<<"amount">> := Amount}) ->
    {ok, do_withdraw(Amount)}.
```

An outright crash, a timeout, or an unencodable result all become
`-32603 Internal error`, with the real reason logged and reported over
telemetry but never sent to the client.

### Error codes

The specification reserves `-32768..-32000` but delegates `-32099..-32000` to
the implementation as *server errors*. Handlers may use that range and any
application-defined code outside the reserved band. A handler that tries to
emit a framework-owned code — `-32700`, `-32600`, `-32601`, `-32602`,
`-32603`, or anything in `-32768..-32100` — gets `-32603` substituted, so it
can never impersonate a protocol-level failure that a client would act on.

### Handler context

Register the same function name at arity 2 to receive the request context as
well: `transport`, `request_id`, `peer`, and `connection_pid`.

```erlang
subscribe_to_prices(_Params, #{connection_pid := Pid}) ->
    ok = json_rpc_ws:subscribe(Pid, prices),
    <<"subscribed">>.
```

Arity 2 wins when a module exports both, because a handler that wants the
context has no other way to reach it.

### Declaring methods in configuration

Methods listed in the `methods` key are registered at start and re-registered
if the registry ever restarts, which methods registered at runtime are not:

```erlang
{json_rpc, [
    {methods, [{<<"subtract">>, {my_handlers, subtract}}]}
]}
```

## Server push

JSON-RPC is peer-symmetric, so the server may send notifications on its own
initiative over WebSocket:

```erlang
ok = json_rpc:push(ConnectionPid, <<"tick">>, #{<<"px">> => 42}),

ok = json_rpc:subscribe(ConnectionPid, prices),
ok = json_rpc:publish(prices, <<"tick">>, #{<<"px">> => 42}).
```

Subscriptions are held in `pg` and cleaned up automatically when a connection
goes away. `pg` is node-local: to fan out across replicas, either cluster the
BEAM nodes so groups span the cluster, or bridge an external bus (Redis, NATS,
Kafka) into a local `publish/3` on each node. This library ships no bridge.

## Configuration

Set via `application:set_env/3` or `sys.config` before the application starts.
Every value is validated at boot, so a bad one fails the start naming the key
rather than surfacing later as a listener that will not bind.

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

## Concurrency and limits

Batch elements run concurrently under one shared deadline, so a batch of ten
slow calls costs one timeout rather than ten. Responses are reassembled in
request order regardless of completion order.

WebSocket frames are dispatched off the connection process, so a slow call
never stalls the socket: the connection keeps answering pings, can still be
drained, and a fast call sent after a slow one is answered first. Past
`ws_max_in_flight` concurrent dispatches, further calls are shed immediately
with `-32000` rather than queued, which keeps a client that pipelines without
limit from growing the node unbounded.

## Observability

Attach to the `telemetry` events documented in `json_rpc_telemetry`:

```erlang
telemetry:attach(
    my_handler,
    [json_rpc, request, stop],
    fun(_Event, #{duration := Duration}, #{method := Method}, _Config) ->
        my_metrics:observe(Method, Duration)
    end,
    undefined
).
```

Handler logs carry `json_rpc_method`, `json_rpc_id`, and `json_rpc_request_id`
in their logger metadata. `json_rpc_request_id` is taken from an inbound
`x-request-id` header when the upstream proxy sets one, so a trace id follows
the call through.

## Graceful shutdown

Stopping the application installs a `503`-replying route, asks every
WebSocket connection to send a `1001 Going Away` close frame, then waits for
in-flight requests to finish, up to `drain_timeout_ms`.

This is best-effort by construction: a client reconnecting during the window
still gets a `503`. Taking the node out of a load balancer's rotation before
`SIGTERM` is what makes a shutdown invisible; the drain only keeps in-flight
work from being cut off.

## Securing your endpoint

The library terminates no TLS and performs no authentication. It is built to
sit plaintext behind an L7 proxy (Envoy, nginx, HAProxy) that terminates TLS,
enforces rate limits, and applies your auth scheme.

To authenticate inside the BEAM, put a `cowboy_middleware` ahead of the
handlers and short-circuit unauthenticated requests before they reach the
dispatcher.

## Development

The toolchain comes from the Nix flake: `nix develop`, or `make sh` for a
shell. Then `make` to list the tasks:

| Task | Description |
| --- | --- |
| `make build` | Compile |
| `make test` | Run the Common Test suites |
| `make format` | Format with erlfmt |
| `make lint` | Run elvis |
| `make xref` | Cross-reference analysis |
| `make dialyzer` | Static type analysis |
| `make cover` | Test coverage report |
| `make docs` | Build the ex_doc documentation |
| `make check` | Format check, lint, xref, dialyzer, and tests |

The suites are split by concern: `json_rpc_protocol_SUITE` covers
specification conformance at the dispatcher level with no transport in the
way, and the transport, registry, and application suites cover the rest.

## License

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
