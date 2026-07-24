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

-module(json_rpc_error).

-moduledoc """
Construction and classification of JSON-RPC 2.0 `error` objects.

An error object is a map with a `code`, a human-readable `message`, and an
optional `data` member carrying anything JSON-encodable:

```erlang
#{code => -32601, message => <<"Method not found">>}
```

Handlers report application errors either by returning `{error, Error}` from
the handler function or by raising one with `throw_error/2,3`. Both accept
the same shapes.

## Error code ranges

The specification reserves `-32768..-32000`, but delegates `-32099..-32000`
to the implementation as "server errors". This library follows that split:
a handler may emit any code outside `-32768..-32100`, which includes the
whole `-32099..-32000` server-error range as well as all application-defined
codes. A handler that tries to emit a framework-owned code (`-32700`,
`-32600`, `-32601`, `-32602`, `-32603`, or anything else in
`-32768..-32100`) gets `-32603 Internal error` substituted, so a handler can
never impersonate a protocol-level failure.
""".

-include("json_rpc.hrl").

-export([
    new/2,
    new/3,
    parse_error/0,
    invalid_request/0,
    method_not_found/0,
    invalid_params/0,
    invalid_params/1,
    internal_error/0,
    internal_error/1,
    server_error/2,
    is_reserved/1,
    throw_error/2,
    throw_error/3,
    code/1,
    from_term/1
]).

-type code() :: integer().
-type message() :: binary().
-type data() :: json_rpc_json:value().

-doc "A JSON-RPC 2.0 error object.".
-type t() :: #{
    code := code(),
    message := message(),
    data => data()
}.

-export_type([t/0, code/0, message/0, data/0]).

%%% Construction

-doc "Build an error object from a code and message.".
-spec new(code(), message()) -> t().
new(Code, Message) when is_integer(Code), is_binary(Message) ->
    #{code => Code, message => Message}.

-doc """
Build an error object carrying an additional `data` member.

`Data` must be JSON-encodable; if it is not, the transport substitutes a
plain `-32603 Internal error` rather than failing the response.
""".
-spec new(code(), message(), data()) -> t().
new(Code, Message, Data) when is_integer(Code), is_binary(Message) ->
    #{code => Code, message => Message, data => Data}.

-doc "`-32700 Parse error` — the payload was not valid JSON.".
-spec parse_error() -> t().
parse_error() ->
    new(?JSONRPC_PARSE_ERROR, <<"Parse error">>).

-doc "`-32600 Invalid Request` — the payload was not a valid Request object.".
-spec invalid_request() -> t().
invalid_request() ->
    new(?JSONRPC_INVALID_REQUEST, <<"Invalid Request">>).

-doc "`-32601 Method not found` — no handler is registered under that name.".
-spec method_not_found() -> t().
method_not_found() ->
    new(?JSONRPC_METHOD_NOT_FOUND, <<"Method not found">>).

-doc "`-32602 Invalid params`.".
-spec invalid_params() -> t().
invalid_params() ->
    new(?JSONRPC_INVALID_PARAMS, <<"Invalid params">>).

-doc "`-32602 Invalid params`, with `Data` describing what was wrong.".
-spec invalid_params(data()) -> t().
invalid_params(Data) ->
    new(?JSONRPC_INVALID_PARAMS, <<"Invalid params">>, Data).

-doc "`-32603 Internal error`.".
-spec internal_error() -> t().
internal_error() ->
    new(?JSONRPC_INTERNAL_ERROR, <<"Internal error">>).

-doc "`-32603 Internal error`, with `Data` describing the cause.".
-spec internal_error(data()) -> t().
internal_error(Data) ->
    new(?JSONRPC_INTERNAL_ERROR, <<"Internal error">>, Data).

-doc """
An implementation-defined server error.

`Code` must be in `-32099..-32000`; anything else raises `badarg`. Use this
when the failure is the server's fault but is not one of the pre-defined
protocol errors.
""".
-spec server_error(code(), message()) -> t().
server_error(Code, Message) when
    is_integer(Code),
    Code >= ?JSONRPC_SERVER_ERROR_MIN,
    Code =< ?JSONRPC_SERVER_ERROR_MAX
->
    new(Code, Message);
server_error(Code, Message) ->
    erlang:error(badarg, [Code, Message]).

%%% Classification

-doc """
Is `Code` reserved for the framework?

True for the pre-defined protocol range `-32768..-32100`, which handlers may
not emit. The implementation-defined server-error range `-32099..-32000` is
*not* reserved — handlers are free to use it.
""".
-spec is_reserved(code()) -> boolean().
is_reserved(Code) when is_integer(Code) ->
    Code >= ?JSONRPC_RESERVED_MIN andalso Code =< ?JSONRPC_RESERVED_MAX.

-doc "Extract the numeric code from an error object.".
-spec code(t()) -> code().
code(#{code := Code}) ->
    Code.

%%% Raising

-doc """
Raise an error object from inside a handler.

Equivalent to returning `{error, json_rpc_error:new(Code, Message)}`, but
usable from anywhere in the call stack:

```erlang
handle_withdraw(#{<<"amount">> := Amount}) when Amount =< 0 ->
    json_rpc_error:throw_error(-32000, <<"amount must be positive">>).
```
""".
-spec throw_error(code(), message()) -> no_return().
throw_error(Code, Message) ->
    throw({jsonrpc_error, new(Code, Message)}).

-doc "As `throw_error/2`, with an additional `data` member.".
-spec throw_error(code(), message(), data()) -> no_return().
throw_error(Code, Message, Data) ->
    throw({jsonrpc_error, new(Code, Message, Data)}).

%%% Normalisation

-doc """
Normalise the error shapes a handler is allowed to produce.

Accepts an error object, `{Code, Message}`, or `{Code, Message, Data}`, and
returns a canonical error object. Returns `not_an_error` for anything else,
which the dispatcher reports as an internal error rather than guessing.
""".
-spec from_term(term()) -> {ok, t()} | not_an_error.
from_term(#{code := Code, message := Message} = Error) when
    is_integer(Code), is_binary(Message)
->
    %% Drop any stray keys so a handler can't smuggle extra members into the
    %% error object.
    case Error of
        #{data := Data} -> {ok, new(Code, Message, Data)};
        _ -> {ok, new(Code, Message)}
    end;
from_term({Code, Message}) when is_integer(Code), is_binary(Message) ->
    {ok, new(Code, Message)};
from_term({Code, Message, Data}) when is_integer(Code), is_binary(Message) ->
    {ok, new(Code, Message, Data)};
from_term(_Other) ->
    not_an_error.
