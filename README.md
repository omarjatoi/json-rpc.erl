# json-rpc

[JSON-RPC-2.0](https://www.jsonrpc.org/specification) server in Erlang, exposed
over plain HTTP and WebSocket via [Cowboy](https://github.com/ninenines/cowboy).

## Endpoints

The application starts a single Cowboy listener with two routes:

| Route          | Method | Purpose                              |
|----------------|--------|--------------------------------------|
| `POST /rpc`    | POST   | One-shot JSON-RPC call/notification/batch. |
| `GET /ws`      | GET    | Persistent JSON-RPC channel over WebSocket text frames. |

The HTTP transport answers with `200 OK` for any well-formed JSON-RPC payload —
JSON-RPC errors live in the response body, not the HTTP status. The transport
layer itself returns:

- `204 No Content` for notifications and all-notification batches (no body).
- `405 Method Not Allowed` (with `Allow: POST`) for non-`POST` requests to `/rpc`.
- `413 Payload Too Large` (with a `-32700` JSON-RPC body) when the request body
  exceeds `max_body_bytes`.
- `415 Unsupported Media Type` when `Content-Type` is not `application/json`.

The WebSocket endpoint accepts JSON-RPC payloads as text frames and responds
with text frames (notifications produce no frame at all). Malformed JSON
yields a `-32700` envelope as a text frame.

## Usage

### Register a method

```erlang
ok = json_rpc_methods:register_method(
    <<"subtract">>,
    {my_handlers, subtract}
).
```

Handlers are `{Module, Function}` pairs invoked as arity-1 (the function
receives the JSON-RPC `params` value). They may surface application-level
errors by throwing the structured tuple `{jsonrpc_error, Code, Msg}` or
`{jsonrpc_error, Code, Msg, Data}`; the values flow through to the JSON-RPC
`error` object verbatim.

### HTTP examples

A call:

```sh
curl -sS -X POST http://localhost:8080/rpc \
    -H 'Content-Type: application/json' \
    -d '{"jsonrpc":"2.0","method":"subtract","params":[42,23],"id":1}'
# -> {"jsonrpc":"2.0","result":19,"id":1}
```

A notification (no `id` member, expect `204 No Content`):

```sh
curl -sS -i -X POST http://localhost:8080/rpc \
    -H 'Content-Type: application/json' \
    -d '{"jsonrpc":"2.0","method":"update","params":[1,2,3]}'
# -> HTTP/1.1 204 No Content
```

A batch (notifications inside a batch are silently dropped from the response
array; an all-notification batch yields `204 No Content`):

```sh
curl -sS -X POST http://localhost:8080/rpc \
    -H 'Content-Type: application/json' \
    -d '[
        {"jsonrpc":"2.0","method":"sum","params":[1,2,4],"id":"1"},
        {"jsonrpc":"2.0","method":"notify_hello","params":[7]},
        {"jsonrpc":"2.0","method":"subtract","params":[42,23],"id":"2"}
    ]'
# -> [{"jsonrpc":"2.0","result":7,"id":"1"},
#     {"jsonrpc":"2.0","result":19,"id":"2"}]
```

## Configuration

Set these via `application:set_env/3` (or `sys.config`) before
`application:ensure_all_started(json_rpc)`.

| Key                | Default       | Meaning |
|--------------------|---------------|---------|
| `port`             | `8080`        | TCP port for the Cowboy listener. |
| `max_body_bytes`   | `1_048_576`   | Per-request body cap. Overflow → `413` with a `-32700` body. |
| `max_connections`  | `1_024`       | `ranch`'s `max_connections` for the listener. |
| `num_acceptors`    | `10`          | Number of acceptor processes. |
| `idle_timeout_ms`  | `60_000`      | Cowboy `idle_timeout` (applies to keep-alive HTTP and WebSocket). |

## Securing your endpoint

The library ships **no** authentication, no TLS listener, and no `Authorization`
parsing of any kind. By design it expects to sit plaintext behind an L7 proxy
(Envoy, nginx, HAProxy, …) that terminates TLS, enforces rate limits, and
applies whatever auth scheme you use (mTLS, bearer tokens, OIDC, …). If you
need to authenticate inside the BEAM, slot a Cowboy middleware in front of the
two handlers; full recipes will land in a later phase.

## Development

`erlang` and `rebar3` are the two required development dependencies; `erlfmt` is useful but not mandatory. There is a [`flake.nix`](./flake.nix) with a development shell with both dependencies present, you can run the shell with `nix develop` (you may want to pass `--command /bin/zsh` on macOS), or via `make sh`.

There is a [`Makefile`](./Makefile) with the common tasks; run `make` to see
them:

```
Run tasks for json-rpc

  build      compile the json-rpc application
  clean      run rebar3 clean and delete the build dir
  deps       get dependencies for the project
  format     run the erlfmt formatter
  lint       run linter (rebar3_lint)
  xref       run rebar3 xref
  dialyzer   run rebar3 dialyzer
  check      run format, lint, xref, dialyzer (fail fast)
  test       run all common_test suites
  ct         run rebar3 ct
  sh         launch a nix shell with zsh (erlang, rebar3)
```

Alternatively, you can use `rebar3` directly.

|Command|Description|
|-|-|
|`rebar3 update`|Download any dependencies|
|`rebar3 compile`|Build the project|
|`rebar3 ct`|Run all common_test suites|
|`rebar3 xref`|Run cross-reference checks|
|`rebar3 dialyzer`|Run the static type analyzer|
|`rebar3 format`|Run the [Erlang formatter](https://github.com/WhatsApp/erlfmt)|

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
