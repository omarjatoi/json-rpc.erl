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

-module(json_rpc_request).

-moduledoc """
Validation of a single decoded JSON-RPC 2.0 Request object.

`parse/1` turns one decoded JSON value into exactly one of three shapes:

- `{call, Id, Method, Params}` — a well-formed Request that expects a
  Response.
- `{notification, Method, Params}` — a well-formed Request with no `id`
  member. It must never be answered, whatever the handler does.
- `{invalid, Id, Error}` — not a valid Request object. This *is* answered,
  even when no `id` was present, in which case `Id` is `null`. That follows
  the specification's own worked example, where an object carrying a
  non-string `method` and no `id` still draws an `-32600` response.

The distinction matters: only a Request that survives validation can claim
the silence a Notification is entitled to.
""".

-include("json_rpc.hrl").

-export([parse/1, id_for_error/1]).

-doc "The `params` member: by-position, by-name, or absent (`[]`).".
-type params() :: [json_rpc_json:value()] | #{binary() => json_rpc_json:value()}.

-doc "The result of validating one Request object.".
-type parsed() ::
    {call, json_rpc_response:id(), binary(), params()}
    | {notification, binary(), params()}
    | {invalid, json_rpc_response:id(), json_rpc_error:t()}.

-export_type([params/0, parsed/0]).

-doc """
Validate one decoded Request object.

Extraction is deliberately ordered so that a request carrying a usable `id`
gets that `id` echoed back on the error, even when the rest of the envelope
is malformed — otherwise a client cannot correlate the failure with the call
that caused it.
""".
-spec parse(json_rpc_json:value()) -> parsed().
parse(Request) when is_map(Request), map_size(Request) > 0 ->
    case extract_id(Request) of
        {error, invalid_id} ->
            %% An `id' of the wrong JSON type is not something we can echo,
            %% so the error carries `null'.
            {invalid, null, json_rpc_error:invalid_request()};
        Id ->
            parse_envelope(Request, Id)
    end;
parse(_Request) ->
    {invalid, null, json_rpc_error:invalid_request()}.

-doc """
Best-effort `id` extraction for errors raised before or outside validation.

Used by the transports when they have to answer a payload they could not
dispatch. A batch has no single `id` to attribute an error to, so it yields
`null`.
""".
-spec id_for_error(json_rpc_json:value()) -> json_rpc_response:id().
id_for_error(Payload) when is_map(Payload) ->
    case extract_id(Payload) of
        {error, invalid_id} -> null;
        notification -> null;
        Id -> Id
    end;
id_for_error(_Payload) ->
    null.

%%% Internal

parse_envelope(Request, Id) ->
    case Request of
        #{<<"jsonrpc">> := ?JSONRPC_VERSION, <<"method">> := Method} when
            is_binary(Method), Method =/= <<>>
        ->
            parse_params(Request, Id, Method);
        _ ->
            {invalid, echo(Id), json_rpc_error:invalid_request()}
    end.

parse_params(Request, Id, Method) ->
    case maps:find(<<"params">>, Request) of
        error ->
            request(Id, Method, []);
        {ok, Params} when is_list(Params); is_map(Params) ->
            request(Id, Method, Params);
        {ok, _Other} ->
            %% `params' must be a structured value. A Request that violates
            %% that is not a valid Request object, so it is answered even
            %% when it carries no `id'.
            {invalid, echo(Id), json_rpc_error:invalid_params()}
    end.

request(notification, Method, Params) ->
    {notification, Method, Params};
request(Id, Method, Params) ->
    {call, Id, Method, Params}.

%% Absent `id' marks a Notification. Present, it must be a String, a Number,
%% or Null — booleans, arrays, and objects are rejected outright.
extract_id(Request) ->
    case maps:find(<<"id">>, Request) of
        error -> notification;
        {ok, null} -> null;
        {ok, Id} when is_binary(Id) -> Id;
        {ok, Id} when is_integer(Id) -> Id;
        {ok, Id} when is_float(Id) -> Id;
        {ok, _Other} -> {error, invalid_id}
    end.

echo(notification) -> null;
echo(Id) -> Id.
