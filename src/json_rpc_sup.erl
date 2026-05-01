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

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% rest_for_one: if the methods registry crashes and restarts empty, we
    %% also restart the listener so it doesn't keep serving -32601 against a
    %% half-populated registry mid-flight.
    SupFlags = #{strategy => rest_for_one, intensity => 5, period => 10},
    DrainMs = json_rpc_config:get(drain_timeout_ms),
    %% The listener's terminate/2 may take up to drain_timeout_ms; give the
    %% supervisor enough budget to wait for it to finish before brutal-killing.
    ListenerShutdown = DrainMs + 1000,
    Children = [
        #{
            id => json_rpc_methods,
            start => {json_rpc_methods, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [json_rpc_methods]
        },
        %% A `pg' scope is itself a process. Start it before the listener
        %% so the WS handlers can join groups (for server-push) and the
        %% drain group as soon as they accept a connection. If the scope
        %% crashes, rest_for_one will tear the listener down too.
        #{
            id => json_rpc_pg,
            start => {pg, start_link, [json_rpc]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [pg]
        },
        #{
            id => json_rpc_listener,
            start => {json_rpc_listener, start_link, []},
            restart => permanent,
            shutdown => ListenerShutdown,
            type => worker,
            modules => [json_rpc_listener]
        }
    ],
    {ok, {SupFlags, Children}}.
