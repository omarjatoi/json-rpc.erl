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

-module(json_rpc_transport).

-moduledoc """
The decode-dispatch-encode pipeline shared by the HTTP and WebSocket handlers.

Both transports do the same three things to a payload and differ only in how
bytes arrive and leave, so the pipeline lives here and the handlers stay thin.

## Encoding is total

`encode_reply/1` never raises. A handler result that JSON cannot represent —
a tuple, a pid, a reference, a function, or a binary that is not valid UTF-8
— would otherwise take down the connection process at encode time, losing
every other in-flight call on that connection.

The fast path encodes the whole reply in one pass. Only if that fails does it
fall back to encoding element by element, so a single bad element in a batch
degrades to `-32603` on its own and its siblings are still delivered.
""".

-include_lib("kernel/include/logger.hrl").

-export([
    handle/2,
    encode_reply/1,
    error_body/1,
    error_body/2,
    request_id/1
]).

-doc "Bytes to send back, or nothing at all when the payload was Notifications only.".
-type outcome() :: {reply, iodata()} | no_reply.

-export_type([outcome/0]).

-doc """
Decode `Payload`, dispatch it, and encode the reply.

Returns `no_reply` when the payload consisted only of Notifications; the HTTP
transport turns that into `204 No Content` and the WebSocket transport sends
no frame.
""".
-spec handle(binary(), json_rpc_dispatcher:context()) -> outcome().
handle(Payload, Context) ->
    case json_rpc_json:decode(Payload) of
        {ok, Decoded} ->
            reply(json_rpc_dispatcher:dispatch(Decoded, Context));
        {error, parse_error} ->
            json_rpc_telemetry:parse_error(maps:get(transport, Context, internal)),
            {reply, error_body(json_rpc_error:parse_error())}
    end.

-doc """
Encode a dispatcher reply, degrading unencodable elements to `-32603`.
""".
-spec encode_reply(json_rpc_dispatcher:reply()) -> iodata().
encode_reply(Reply) ->
    case json_rpc_json:encode_iodata(Reply) of
        {ok, IoData} ->
            IoData;
        {error, Reason} ->
            ?LOG_ERROR("json_rpc: reply is not encodable (~p); degrading", [Reason]),
            degrade(Reply)
    end.

-doc "Encode a standalone error Response with a `null` id.".
-spec error_body(json_rpc_error:t()) -> iodata().
error_body(Error) ->
    error_body(null, Error).

-doc "Encode a standalone error Response echoing `Id`.".
-spec error_body(json_rpc_response:id(), json_rpc_error:t()) -> iodata().
error_body(Id, Error) ->
    encode_reply(json_rpc_response:error(Id, Error)).

-doc """
Take the correlation id from an upstream `x-request-id` header, or mint one.

Honouring the inbound header lets a proxy's request id follow the call into
handler logs and telemetry.
""".
-spec request_id(binary() | undefined) -> binary().
request_id(Header) when is_binary(Header), Header =/= <<>> ->
    binary:part(Header, 0, min(byte_size(Header), 128));
request_id(_Absent) ->
    binary:encode_hex(crypto:strong_rand_bytes(8), lowercase).

%%% Internal

reply(no_response) -> no_reply;
reply(Reply) -> {reply, encode_reply(Reply)}.

%% Re-encode one element at a time so the encodable ones survive.
degrade(Responses) when is_list(Responses) ->
    [$[, lists:join($,, [degrade_one(R) || R <- Responses]), $]];
degrade(Response) ->
    degrade_one(Response).

degrade_one(Response) ->
    case json_rpc_json:encode_iodata(Response) of
        {ok, IoData} -> IoData;
        {error, _Reason} -> substitute(Response)
    end.

%% The offending part is the handler's `result' (or an error's `data'), so
%% rebuild the envelope around the id alone. That is always encodable: the id
%% was itself decoded from JSON.
substitute(Response) ->
    Id = maps:get(id, Response, null),
    {ok, IoData} = json_rpc_json:encode_iodata(
        json_rpc_response:error(Id, json_rpc_error:internal_error())
    ),
    IoData.
