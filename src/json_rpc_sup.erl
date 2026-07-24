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

-module(json_rpc_sup).

-moduledoc """
Root supervisor.

Children start in dependency order and `rest_for_one` keeps them that way:

1. `m:json_rpc_methods` — owns the registry table. Everything dispatched
   needs it.
2. `pg` scope `json_rpc` — backs WebSocket topics and the drain group, so it
   must exist before any connection is accepted.
3. `m:json_rpc_listener` — opens the socket. Started last, because from the
   moment it does, requests arrive.

If either of the first two dies, the listener goes with it rather than
serving requests against a registry that is being rebuilt or a `pg` scope
that has lost its groups. Methods declared in the `methods` environment key
are re-registered on restart; see `m:json_rpc_methods` for what that does and
does not cover.
""".

-behaviour(supervisor).

-export([start_link/0, init/1]).

-doc false.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-doc false.
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => rest_for_one, intensity => 5, period => 10},
    Children = [
        worker(json_rpc_methods, {json_rpc_methods, start_link, []}, 5000),
        worker(json_rpc_pg, {pg, start_link, [json_rpc]}, 5000),
        %% terminate/2 drains for up to drain_timeout_ms, so the shutdown
        %% budget has to exceed it or the supervisor kills the listener
        %% mid-drain and the drain is pointless.
        worker(
            json_rpc_listener,
            {json_rpc_listener, start_link, []},
            json_rpc_config:get(drain_timeout_ms) + 1000
        )
    ],
    {ok, {SupFlags, Children}}.

%%% Internal

worker(Id, {Module, _Function, _Args} = Start, Shutdown) ->
    #{
        id => Id,
        start => Start,
        restart => permanent,
        shutdown => Shutdown,
        type => worker,
        modules => [Module]
    }.
