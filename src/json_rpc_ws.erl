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

-module(json_rpc_ws).

%% Public server-push API for the WebSocket transport. JSON-RPC 2.0 is
%% peer-symmetric, so the server is allowed to send Notification objects
%% (no `id' member) on its own initiative. This module exposes:
%%
%%   - `push/3'        — send a Notification to one specific connection.
%%   - `subscribe/2'   — join a connection to a topic.
%%   - `unsubscribe/2' — leave a topic.
%%   - `publish/3'     — broadcast a Notification to every subscriber of a
%%                       topic.
%%
%% Subscriptions are backed by `pg' in the `json_rpc' scope. `pg' monitors
%% its members, so when a WS handler process exits its subscriptions are
%% cleaned up automatically — no `terminate/3' hook needed.
%%
%% Distributed caveat: `pg' is local to this Erlang node by default. To fan
%% out across replicas either cluster the BEAM nodes (e.g. libcluster against
%% a headless Kubernetes service) so `pg' joins span the cluster, or bridge
%% to an external bus (Redis, NATS, Kafka, …) from each node and translate
%% inbound bus messages into local `publish/3' calls. This library does not
%% ship a bus bridge.

-export([
    push/3,
    subscribe/2,
    unsubscribe/2,
    publish/3
]).

-define(SCOPE, json_rpc).

-type topic() :: term().

-export_type([topic/0]).

%% @doc Send a JSON-RPC Notification (a request without an `id' member) to a
%% single WS connection. The frame is delivered to the connection process
%% via `{json_rpc_push, Frame}'; the WS handler turns that into a text
%% frame.
-spec push(pid(), binary(), term()) -> ok.
push(ConnPid, Method, Params) when is_pid(ConnPid), is_binary(Method) ->
    Frame = encode_notification(Method, Params),
    ConnPid ! {json_rpc_push, Frame},
    ok.

%% @doc Join `ConnPid' to the `pg' group identified by `Topic'. Idempotent;
%% `pg' tolerates multiple joins of the same pid.
-spec subscribe(pid(), topic()) -> ok.
subscribe(ConnPid, Topic) when is_pid(ConnPid) ->
    pg:join(?SCOPE, group_name(Topic), ConnPid).

%% @doc Remove `ConnPid' from the `pg' group identified by `Topic'. Returns
%% `ok' even if the pid was not a member.
-spec unsubscribe(pid(), topic()) -> ok.
unsubscribe(ConnPid, Topic) when is_pid(ConnPid) ->
    case pg:leave(?SCOPE, group_name(Topic), ConnPid) of
        ok -> ok;
        not_joined -> ok
    end.

%% @doc Broadcast a JSON-RPC Notification to every connection currently
%% subscribed to `Topic'. Encoding happens once; each subscriber receives
%% the same frame.
-spec publish(topic(), binary(), term()) -> ok.
publish(Topic, Method, Params) when is_binary(Method) ->
    Frame = encode_notification(Method, Params),
    Members = pg:get_members(?SCOPE, group_name(Topic)),
    lists:foreach(
        fun(Pid) -> Pid ! {json_rpc_push, Frame} end,
        Members
    ),
    ok.

%% Internal

group_name(Topic) ->
    {json_rpc_topic, Topic}.

encode_notification(Method, Params) ->
    Notification = notification_object(Method, Params),
    jiffy:encode(Notification).

%% Per the JSON-RPC 2.0 spec, `params' is an OPTIONAL member; omit it
%% entirely when the caller passes `undefined' so we don't ship a
%% `"params": null' object that some clients reject.
notification_object(Method, undefined) ->
    #{jsonrpc => <<"2.0">>, method => Method};
notification_object(Method, Params) ->
    #{jsonrpc => <<"2.0">>, method => Method, params => Params}.
