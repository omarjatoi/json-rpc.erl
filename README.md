# json-rpc

[JSON-RPC-2.0](https://www.jsonrpc.org/specification) server and client implementation in Erlang.

## Usage

### Server

```erlang
% start the server at port 8000
{ok, _} = json_rpc_server:start_link(8000).

% register a method
json_rpc_server:register_method(<<"some_method_name">>, fun module:some_method/1).

% optionally set some authentication for the rpc
json_rpc_server:set_auth(fun(Request) ->
    case Request of
        #{<<"auth">> := <<"secret_token">>} -> ok;
        _ -> error
    end
end).
```

### Client

```erlang
% create a new client
{ok, Client} = json_rpc_client:connect("localhost", 8000).

% if using an auth function, add auth token
AuthenticatedClient = json_rpc_client:set_auth(Client, <<"secret_token">>).

% call "some_method_name" with params {"foo": 1, "bar": 2} and get result
{ok, Result, Id} = json_rpc_client:call(AuthenticatedClient, <<"some_method_name">>, #{foo => 1, bar => 2}, 1).

% call "log" with params {"message": "Hello, World!"}
ok = json_rpc_client:notify(Client2, <<"log">>, #{message => "Hello, World!"}).

% make some batched requests
{ok, Results} = json_rpc_client:batch(Client2, [
    {<<"some_method_name">>, #{foo => 1, bar => 2}, 1},
    {<<"log">>, #{message => "Hello, World!"}, undefined}
]).

% close the client
json_rpc_client:close(Client2).
```

## Development

`erlang` and `rebar3` are the two development dependencies. There is a [`flake.nix`](./flake.nix) with a development shell with both dependencies present, you can run the shell with `nix develop` (you may want to pass `--command /bin/zsh` on macOS).

- Get dependencies
  ```
  rebar3 upgrade --all
  ```
- Build the project
  ```
  rebar3 compile
  ```
- Run all tests
  ```
  rebar3 eunit
  ```
- Run formatter
  ```
  rebar3 format
  ```

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
