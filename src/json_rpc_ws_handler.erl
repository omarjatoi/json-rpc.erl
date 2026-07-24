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

-module(json_rpc_ws_handler).

-moduledoc """
Persistent JSON-RPC channel over WebSocket text frames.

## Frames never block the connection

Each inbound frame is dispatched in its own process and the reply comes back
as a message. The connection process therefore stays responsive while
handlers run: it keeps answering pings, it can still be drained at shutdown,
and one slow call does not stall every other call on the same socket.

Replies are emitted in completion order, not arrival order. That is safe
because JSON-RPC correlates by `id`, and it is the point — a fast call queued
behind a slow one should not wait for it.

`ws_max_in_flight` bounds how many dispatches a single connection may have
running at once. Past the bound a call is answered immediately with a
`-32000` server error rather than being queued, which keeps a client that
pipelines without limit from growing the node's process count without bound.

## Framing

Text frames only. A binary frame closes the connection with `1003`
(Unsupported Data): JSON-RPC is defined over text, and silently dropping the
frame would leave the client waiting for a reply that is never coming.
""".

-behaviour(cowboy_websocket).

-include_lib("kernel/include/logger.hrl").

-include("json_rpc.hrl").

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-define(DRAIN_GROUP, json_rpc_ws_connections).
-define(REPLY_TAG, '$json_rpc_ws_reply').

-doc false.
-spec init(cowboy_req:req(), term()) -> {cowboy_websocket, cowboy_req:req(), map(), map()}.
init(Req, _State) ->
    %% Accept the upgrade whatever subprotocols the client offers. RFC 6455
    %% lets the server select none by simply omitting the response header,
    %% and clients routinely advertise one as a hint; refusing the upgrade
    %% over it breaks interop for no benefit.
    %%
    %% Config is snapshotted here so the per-frame path never re-reads it.
    Opts = #{
        max_frame_size => json_rpc_config:get(ws_max_frame_bytes),
        idle_timeout => json_rpc_config:get(ws_idle_timeout_ms),
        compress => false
    },
    %% `connection_pid' is deliberately not set here. This callback runs in
    %% Cowboy's request process, which is handed off and discarded once the
    %% upgrade completes; the WebSocket loop runs in the connection process.
    %% Capturing self() here would put a dead pid in every handler context.
    State = #{
        max_in_flight => json_rpc_config:get(ws_max_in_flight),
        in_flight => #{},
        context => #{
            transport => websocket,
            request_id => json_rpc_transport:request_id(
                cowboy_req:header(<<"x-request-id">>, Req)
            ),
            peer => cowboy_req:peer(Req)
        }
    },
    {cowboy_websocket, Req, State, Opts}.

-doc false.
-spec websocket_init(map()) -> {cowboy_websocket:commands(), map()}.
websocket_init(#{context := Context} = State) ->
    %% This runs in the connection process, so this is where the pid handlers
    %% are given — the one json_rpc_ws:push/3 and subscribe/2 expect — is
    %% finally known.
    ok = pg:join(json_rpc, ?DRAIN_GROUP, self()),
    json_rpc_telemetry:ws_connection_open(maps:get(peer, Context, undefined)),
    {[], State#{context := Context#{connection_pid => self()}}}.

-doc false.
-spec websocket_handle({text | binary | ping | pong, binary()}, map()) ->
    {cowboy_websocket:commands(), map()}.
websocket_handle({text, Frame}, State) ->
    accept(Frame, State);
websocket_handle({binary, _Data}, State) ->
    {[{close, 1003, <<"binary frames are not supported">>}], State};
websocket_handle(_Frame, State) ->
    {[], State}.

-doc false.
-spec websocket_info(term(), map()) -> {cowboy_websocket:commands(), map()}.
websocket_info({?REPLY_TAG, Pid, Outcome}, #{in_flight := InFlight} = State) ->
    case maps:take(Pid, InFlight) of
        {MonitorRef, Rest} ->
            %% Flushing takes the DOWN that follows this process's exit out
            %% of the mailbox, so it is never mistaken for a failure.
            erlang:demonitor(MonitorRef, [flush]),
            {frames(Outcome), State#{in_flight := Rest}};
        error ->
            {[], State}
    end;
websocket_info({'DOWN', _MonitorRef, process, Pid, Reason}, #{in_flight := InFlight} = State) ->
    %% The dispatch process died without reporting. Everything a handler can
    %% raise is already contained by the worker, so reaching here means it was
    %% killed from outside — and the client is still owed an answer. The id is
    %% not known here: recovering it would mean decoding every frame twice on
    %% the hot path to serve a case that should never happen.
    case maps:take(Pid, InFlight) of
        {_MonitorRef1, Rest} ->
            ?LOG_ERROR("json_rpc: WebSocket dispatch died: ~p", [Reason]),
            Body = json_rpc_transport:error_body(json_rpc_error:internal_error()),
            {[{text, Body}], State#{in_flight := Rest}};
        error ->
            {[], State}
    end;
websocket_info({json_rpc_push, Frame}, State) ->
    %% Server-initiated Notification from json_rpc_ws:push/3 or publish/3,
    %% already encoded.
    {[{text, Frame}], State};
websocket_info(json_rpc_drain, State) ->
    %% The listener is shutting down. 1001 Going Away tells the client to
    %% reconnect elsewhere rather than treating this as an error.
    {[{close, 1001, <<"server shutting down">>}], State};
websocket_info(_Info, State) ->
    {[], State}.

-doc false.
-spec terminate(term(), cowboy_req:req(), term()) -> ok.
terminate(Reason, _Req, _State) ->
    json_rpc_telemetry:ws_connection_close(Reason),
    ok.

%%% Internal

accept(Frame, #{in_flight := InFlight, max_in_flight := Max} = State) when
    map_size(InFlight) >= Max
->
    ?LOG_WARNING("json_rpc: WebSocket connection at in-flight cap ~p, shedding frame", [Max]),
    Error = json_rpc_error:new(
        ?JSONRPC_SERVER_ERROR_MAX,
        <<"Server error">>,
        #{reason => <<"too many concurrent requests">>, max_in_flight => Max}
    ),
    Id = peek_id(Frame),
    {[{text, json_rpc_transport:error_body(Id, Error)}], State};
accept(Frame, #{in_flight := InFlight, context := Context} = State) ->
    Connection = self(),
    %% spawn then monitor, rather than spawn_monitor, so both the reply and
    %% the DOWN are keyed by the same pid. If the process finishes before the
    %% monitor is set up, the reply is already in the mailbox ahead of the
    %% immediate noproc DOWN, and the reply clause flushes that DOWN away.
    Pid = spawn(fun() ->
        Connection ! {?REPLY_TAG, self(), json_rpc_transport:handle(Frame, Context)}
    end),
    MonitorRef = erlang:monitor(process, Pid),
    {[], State#{in_flight := InFlight#{Pid => MonitorRef}}}.

frames({reply, IoData}) -> [{text, IoData}];
frames(no_reply) -> [].

%% Best-effort id for an error that has to be answered without dispatching.
%% This decodes the frame a second time, so it is confined to the shedding
%% path — the hot path must never pay it.
peek_id(Frame) ->
    case json_rpc_json:decode(Frame) of
        {ok, Decoded} -> json_rpc_request:id_for_error(Decoded);
        {error, parse_error} -> null
    end.
