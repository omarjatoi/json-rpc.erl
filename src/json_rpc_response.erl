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

-module(json_rpc_response).

-moduledoc """
Construction of JSON-RPC 2.0 Response objects.

A Response is either a Result (`result` member present) or an Error
(`error` member present) — never both. Both always carry `jsonrpc` and `id`.
Notifications produce no Response at all, represented here as
`no_response`.
""".

-include("json_rpc.hrl").

%% `error/2' and `error/3' name the JSON-RPC concept, not the BIF. The
%% shadowing is deliberate and the auto-import is dropped so every call site
%% is unambiguous.
-compile({no_auto_import, [error/2, error/3]}).

-export([
    result/2,
    error/2,
    error/3,
    error/4
]).

-doc """
The `id` of a Request, as constrained by the specification: a String, a
Number, or Null. `notification` is the internal marker for a Request with no
`id` member at all.
""".
-type id() :: binary() | integer() | float() | null.

-doc "A Response object, ready to be encoded.".
-type t() :: #{
    jsonrpc := binary(),
    id := id(),
    result => json_rpc_json:value(),
    error => json_rpc_error:t()
}.

-export_type([id/0, t/0]).

-doc "Build a successful Response echoing `Id` and carrying `Result`.".
-spec result(id(), json_rpc_json:value()) -> t().
result(Id, Result) ->
    #{jsonrpc => ?JSONRPC_VERSION, id => Id, result => Result}.

-doc "Build an error Response from an existing error object.".
-spec error(id(), json_rpc_error:t()) -> t().
error(Id, Error) when is_map(Error) ->
    #{jsonrpc => ?JSONRPC_VERSION, id => Id, error => Error}.

-doc "Build an error Response from a code and message.".
-spec error(id(), json_rpc_error:code(), json_rpc_error:message()) -> t().
error(Id, Code, Message) ->
    error(Id, json_rpc_error:new(Code, Message)).

-doc "Build an error Response from a code, message, and `data` member.".
-spec error(id(), json_rpc_error:code(), json_rpc_error:message(), json_rpc_error:data()) ->
    t().
error(Id, Code, Message, Data) ->
    error(Id, json_rpc_error:new(Code, Message, Data)).
