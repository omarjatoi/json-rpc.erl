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

-moduledoc """
Server-initiated notifications over the WebSocket transport.

JSON-RPC 2.0 is peer-symmetric: either side may send a Notification, which is
a Request with no `id` and therefore no Response. That is what this module
sends — never a Request expecting an answer, because the server has no
correlation table to match one against.

`push/3` targets one connection. `subscribe/2`, `unsubscribe/2`, and
`publish/3` add topic fan-out on top.

A handler registered at arity 2 receives the request context, whose
`connection_pid` is exactly the pid these functions take — that is how a
handler subscribes its own caller:

```erlang
subscribe_to_prices(_Params, #{connection_pid := Pid}) ->
    ok = json_rpc_ws:subscribe(Pid, prices),
    <<"subscribed">>.
```

## Scope is one node

Subscriptions live in `m:pg`, which is node-local by default. To fan out
across replicas, either cluster the BEAM nodes so `pg` groups span the
cluster, or bridge an external bus (Redis, NATS, Kafka) into a local
`publish/3` on each node. This library ships no bridge.
""".

-include("json_rpc.hrl").

-export([
    push/3,
    subscribe/2,
    unsubscribe/2,
    publish/3,
    subscribers/1
]).

-define(SCOPE, json_rpc).

-doc "A topic name. Any term; compared with `=:=`.".
-type topic() :: term().

-export_type([topic/0]).

-doc """
Send a Notification to one connection.

Returns `{error, Reason}` if `Params` cannot be encoded as JSON. Delivery
itself is fire-and-forget: a message to a dead connection is discarded, as
with any Erlang send.

Pass `undefined` for `Params` to omit the member entirely — the
specification makes `params` optional, and some clients reject an explicit
`"params": null`.
""".
-spec push(pid(), binary(), json_rpc_json:value() | undefined) ->
    ok | {error, json_rpc_json:encode_error()}.
push(ConnectionPid, Method, Params) when is_pid(ConnectionPid), is_binary(Method) ->
    case encode_notification(Method, Params) of
        {ok, Frame} ->
            ConnectionPid ! {json_rpc_push, Frame},
            ok;
        {error, _Reason} = Error ->
            Error
    end.

-doc "Join `ConnectionPid` to `Topic`. Idempotent.".
-spec subscribe(pid(), topic()) -> ok.
subscribe(ConnectionPid, Topic) when is_pid(ConnectionPid) ->
    pg:join(?SCOPE, group(Topic), ConnectionPid).

-doc "Remove `ConnectionPid` from `Topic`. Succeeds even if it was not a member.".
-spec unsubscribe(pid(), topic()) -> ok.
unsubscribe(ConnectionPid, Topic) when is_pid(ConnectionPid) ->
    case pg:leave(?SCOPE, group(Topic), ConnectionPid) of
        ok -> ok;
        not_joined -> ok
    end.

-doc """
Send a Notification to every subscriber of `Topic`.

Encoded once and delivered to each subscriber. Connections that have gone
away are dropped from the group automatically — `pg` monitors its members —
so there is nothing to clean up after a disconnect.
""".
-spec publish(topic(), binary(), json_rpc_json:value() | undefined) ->
    ok | {error, json_rpc_json:encode_error()}.
publish(Topic, Method, Params) when is_binary(Method) ->
    case encode_notification(Method, Params) of
        {ok, Frame} ->
            lists:foreach(
                fun(Pid) -> Pid ! {json_rpc_push, Frame} end,
                pg:get_members(?SCOPE, group(Topic))
            );
        {error, _Reason} = Error ->
            Error
    end.

-doc "The connections currently subscribed to `Topic`.".
-spec subscribers(topic()) -> [pid()].
subscribers(Topic) ->
    pg:get_members(?SCOPE, group(Topic)).

%%% Internal

group(Topic) ->
    {json_rpc_topic, Topic}.

encode_notification(Method, Params) ->
    json_rpc_json:encode(notification(Method, Params)).

notification(Method, undefined) ->
    #{jsonrpc => ?JSONRPC_VERSION, method => Method};
notification(Method, Params) ->
    #{jsonrpc => ?JSONRPC_VERSION, method => Method, params => Params}.
