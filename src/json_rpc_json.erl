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

-module(json_rpc_json).

-moduledoc """
Total JSON codec built on OTP's `m:json` module.

`json:encode/1` and `json:decode/1` raise on bad input. Every call site here
is on a request path where an exception would cost more than an error return
— a handler that returns a term JSON cannot represent must degrade to a
`-32603` envelope, not tear down the caller's connection. So this module
wraps both directions and returns tagged tuples instead.

Using stdlib `json` rather than a third-party encoder also means the library
carries no native (NIF) dependency.
""".

-export([
    encode/1,
    encode_iodata/1,
    decode/1
]).

-doc """
Any term that can be encoded as JSON: `null`, a boolean, a number, a binary
string, a list, or a map with binary/atom/integer keys.

Kept deliberately loose — handler results are arbitrary user terms and are
validated by attempting to encode them, not by a type check.
""".
-type value() :: term().

-doc "Why an encode failed. `Term` is the first offending subterm.".
-type encode_error() :: {unsupported_type, term()} | {invalid_byte, byte()} | term().

-export_type([value/0, encode_error/0]).

-doc """
Encode `Value` to a binary.

Returns `{error, Reason}` rather than raising when `Value` contains
something JSON cannot represent (a tuple, pid, reference, function, or a
binary that is not valid UTF-8).
""".
-spec encode(value()) -> {ok, binary()} | {error, encode_error()}.
encode(Value) ->
    case encode_iodata(Value) of
        {ok, IoData} -> {ok, iolist_to_binary(IoData)};
        {error, _} = Error -> Error
    end.

-doc """
As `encode/1`, but returns the encoder's `t:iodata/0` directly.

Prefer this where the result is handed straight to a socket — Cowboy accepts
iodata and skips the flattening copy.
""".
-spec encode_iodata(value()) -> {ok, iodata()} | {error, encode_error()}.
encode_iodata(Value) ->
    try
        {ok, json:encode(Value)}
    catch
        error:Reason -> {error, Reason}
    end.

-doc """
Decode a complete JSON document.

Returns `{error, parse_error}` for malformed JSON, trailing bytes after the
document, invalid UTF-8, or an empty input — every case the JSON-RPC
specification answers with `-32700`.
""".
-spec decode(binary()) -> {ok, value()} | {error, parse_error}.
decode(Binary) when is_binary(Binary) ->
    try
        {ok, json:decode(Binary)}
    catch
        error:_Reason -> {error, parse_error}
    end.
