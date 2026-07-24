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

-module(json_rpc_http_handler).

-moduledoc """
`POST` endpoint for one-shot JSON-RPC calls, notifications, and batches.

A well-formed payload always answers `200 OK` with the JSON-RPC Response in
the body — protocol errors live in the body, not in the HTTP status, because
a `-32601` is a successful HTTP exchange that carries an application-level
failure.

The status codes that are *not* `200` all concern the HTTP request itself:

| Status | When |
| --- | --- |
| `204 No Content` | Notification, or a batch of nothing but Notifications |
| `405 Method Not Allowed` | Anything other than `POST` |
| `413 Content Too Large` | Body exceeded `max_body_bytes` |
| `415 Unsupported Media Type` | `Content-Type` was not `application/json` |
| `500 Internal Server Error` | A bug in this handler |

Every one of them carries a JSON-RPC error envelope as the body, so a client
can parse the response the same way whatever went wrong.
""".

-behaviour(cowboy_handler).

-include_lib("kernel/include/logger.hrl").

-include("json_rpc.hrl").

-export([init/2]).

-define(JSON_HEADERS, #{<<"content-type">> => <<"application/json">>}).

-doc false.
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req, State0) ->
    %% Snapshot the config the request path needs, so the hot path does not
    %% go back through application:get_env plus validation per request.
    State = State0#{
        max_body_bytes => json_rpc_config:get(max_body_bytes),
        request_id => json_rpc_transport:request_id(
            cowboy_req:header(<<"x-request-id">>, Req)
        )
    },
    try
        handle(Req, State)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("json_rpc: HTTP handler failed ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {ok, respond(500, json_rpc_error:internal_error(), Req), State}
    end.

%%% Internal

handle(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            post(Req, State);
        _Other ->
            Allow = #{<<"allow">> => <<"POST">>},
            Error = json_rpc_error:new(
                ?JSONRPC_INVALID_REQUEST, <<"Invalid Request">>, #{
                    reason => <<"method not allowed">>
                }
            ),
            {ok, respond(405, Allow, Error, Req), State}
    end.

post(Req, State) ->
    case is_json(Req) of
        true ->
            read_body(Req, State);
        false ->
            Error = json_rpc_error:new(
                ?JSONRPC_INVALID_REQUEST, <<"Invalid Request">>, #{
                    reason => <<"content-type must be application/json">>
                }
            ),
            {ok, respond(415, Error, Req), State}
    end.

read_body(Req0, State) ->
    Max = maps:get(max_body_bytes, State),
    case read_body(Req0, Max, <<>>) of
        {ok, Body, Req} ->
            dispatch(Body, Req, State);
        {too_large, Req} ->
            %% Rejected before any parsing happened, so -32700 would be a
            %% lie: nothing was ever parsed.
            Error = json_rpc_error:new(
                ?JSONRPC_INVALID_REQUEST, <<"Invalid Request">>, #{
                    reason => <<"request body too large">>, max_body_bytes => Max
                }
            ),
            {ok, respond(413, Error, Req), State}
    end.

%% Read in chunks and stop as soon as the cap is passed. `length' is set to
%% what is still allowed rather than to the total cap, so a body that keeps
%% coming cannot buffer close to twice the limit before being rejected.
read_body(Req0, Max, Acc) ->
    Remaining = Max - byte_size(Acc),
    case cowboy_req:read_body(Req0, #{length => Remaining + 1, period => 5000}) of
        {ok, Data, Req} ->
            Combined = <<Acc/binary, Data/binary>>,
            case byte_size(Combined) > Max of
                true -> {too_large, Req};
                false -> {ok, Combined, Req}
            end;
        {more, Data, Req} ->
            Combined = <<Acc/binary, Data/binary>>,
            case byte_size(Combined) > Max of
                true -> {too_large, Req};
                false -> read_body(Req, Max, Combined)
            end
    end.

dispatch(Body, Req, State) ->
    Context = #{
        transport => http,
        request_id => maps:get(request_id, State),
        connection_pid => self(),
        peer => peer(Req)
    },
    case json_rpc_transport:handle(Body, Context) of
        {reply, IoData} ->
            {ok, cowboy_req:reply(200, ?JSON_HEADERS, IoData, Req), State};
        no_reply ->
            {ok, cowboy_req:reply(204, #{}, <<>>, Req), State}
    end.

respond(Status, Error, Req) ->
    respond(Status, #{}, Error, Req).

respond(Status, Headers, Error, Req) ->
    Body = json_rpc_transport:error_body(Error),
    cowboy_req:reply(Status, maps:merge(?JSON_HEADERS, Headers), Body, Req).

peer(Req) ->
    try
        cowboy_req:peer(Req)
    catch
        _Class:_Reason -> undefined
    end.

%% Accept `application/json' and any `application/*+json' structured suffix,
%% with or without parameters such as `charset=utf-8'.
is_json(Req) ->
    try cowboy_req:parse_header(<<"content-type">>, Req) of
        {<<"application">>, <<"json">>, _Params} -> true;
        {<<"application">>, Subtype, _Params} -> is_json_suffix(Subtype);
        _Other -> false
    catch
        _Class:_Reason -> false
    end.

is_json_suffix(Subtype) ->
    case binary:match(Subtype, <<"+json">>) of
        nomatch -> false;
        {Start, Length} -> Start + Length =:= byte_size(Subtype)
    end.
