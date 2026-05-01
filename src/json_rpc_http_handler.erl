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

-behaviour(cowboy_handler).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).

-define(DEFAULT_MAX_BODY_BYTES, 1048576).
-define(JSON_HEADERS, #{<<"content-type">> => <<"application/json">>}).

init(Req0, State) ->
    try
        handle(Req0, State)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Unhandled HTTP handler error: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            Body = jiffy:encode(
                json_rpc_dispatcher:create_error_response(null, -32603, <<"Internal error">>)
            ),
            ReqErr = cowboy_req:reply(500, ?JSON_HEADERS, Body, Req0),
            {ok, ReqErr, State}
    end.

handle(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            handle_post(Req0, State);
        _ ->
            Req = cowboy_req:reply(
                405,
                #{<<"allow">> => <<"POST">>},
                <<>>,
                Req0
            ),
            {ok, Req, State}
    end.

handle_post(Req0, State) ->
    case content_type_is_json(Req0) of
        true ->
            handle_json_post(Req0, State);
        false ->
            Req = cowboy_req:reply(415, #{}, <<>>, Req0),
            {ok, Req, State}
    end.

handle_json_post(Req0, State) ->
    MaxBody = maps:get(max_body_bytes, State, ?DEFAULT_MAX_BODY_BYTES),
    case read_full_body(Req0, MaxBody, <<>>) of
        {ok, Body, Req1} ->
            handle_body(Body, Req1, State);
        {too_large, Req1} ->
            %% The body was rejected at the size cap before any parsing, so
            %% -32700 Parse error is wrong. Use -32600 Invalid Request and
            %% keep the 413 status code.
            Body = jiffy:encode(
                json_rpc_dispatcher:create_error_response(null, -32600, <<"Invalid Request">>)
            ),
            Req = cowboy_req:reply(413, ?JSON_HEADERS, Body, Req1),
            {ok, Req, State}
    end.

read_full_body(Req0, MaxBody, Acc) ->
    %% Read in chunks, capping accumulated size at MaxBody.
    ReadOpts = #{length => MaxBody, period => 5000},
    case cowboy_req:read_body(Req0, ReadOpts) of
        {ok, Data, Req1} ->
            Combined = <<Acc/binary, Data/binary>>,
            case byte_size(Combined) > MaxBody of
                true -> {too_large, Req1};
                false -> {ok, Combined, Req1}
            end;
        {more, Data, Req1} ->
            Combined = <<Acc/binary, Data/binary>>,
            case byte_size(Combined) > MaxBody of
                true -> {too_large, Req1};
                false -> read_full_body(Req1, MaxBody, Combined)
            end
    end.

handle_body(Body, Req0, State) ->
    case decode_json(Body) of
        {ok, Parsed} ->
            Timeout = json_rpc_config:get(request_timeout_ms),
            case json_rpc_worker:run(Parsed, Timeout) of
                {ok, Reply} ->
                    send_reply(Reply, Req0, State);
                {error, timeout} ->
                    Id = call_id_for_error(Parsed),
                    ErrBody = jiffy:encode(
                        json_rpc_dispatcher:create_error_response(
                            Id, -32603, <<"Internal error">>, #{reason => timeout}
                        )
                    ),
                    Req = cowboy_req:reply(200, ?JSON_HEADERS, ErrBody, Req0),
                    {ok, Req, State};
                {error, {crash, Class, Reason}} ->
                    ?LOG_ERROR("Handler crashed: ~p:~p", [Class, Reason]),
                    Id = call_id_for_error(Parsed),
                    ErrBody = jiffy:encode(
                        json_rpc_dispatcher:create_error_response(
                            Id, -32603, <<"Internal error">>
                        )
                    ),
                    Req = cowboy_req:reply(200, ?JSON_HEADERS, ErrBody, Req0),
                    {ok, Req, State}
            end;
        {error, parse_error} ->
            ErrBody = jiffy:encode(
                json_rpc_dispatcher:create_error_response(null, -32700, <<"Parse error">>)
            ),
            Req = cowboy_req:reply(200, ?JSON_HEADERS, ErrBody, Req0),
            {ok, Req, State}
    end.

%% For worker timeout/crash on a single-call payload we can preserve the
%% original id so the client can correlate the error. For batches or any
%% other shape we fall back to null — there's no single id to attribute
%% the error to.
call_id_for_error(Parsed) when is_map(Parsed) ->
    case maps:find(<<"id">>, Parsed) of
        {ok, Id} when is_binary(Id); is_integer(Id); is_float(Id); Id =:= null -> Id;
        _ -> null
    end;
call_id_for_error(_Parsed) ->
    null.

decode_json(Body) ->
    try
        {ok, jiffy:decode(Body, [return_maps])}
    catch
        _:_ -> {error, parse_error}
    end.

send_reply(no_response, Req0, State) ->
    Req = cowboy_req:reply(204, #{}, <<>>, Req0),
    {ok, Req, State};
send_reply(Reply, Req0, State) ->
    Body = jiffy:encode(Reply),
    Req = cowboy_req:reply(200, ?JSON_HEADERS, Body, Req0),
    {ok, Req, State}.

content_type_is_json(Req) ->
    try cowboy_req:parse_header(<<"content-type">>, Req) of
        {<<"application">>, <<"json">>, _} -> true;
        _ -> false
    catch
        _:_ -> false
    end.
