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

-module(json_rpc_worker).

%% Spawn-monitor-await primitive that runs `json_rpc_dispatcher:dispatch/1'
%% in a short-lived worker process. The caller blocks for at most `Timeout'
%% milliseconds. On timeout the worker is killed and `{error, timeout}' is
%% returned; on a worker crash `{error, {crash, Class, Reason}}' is returned;
%% otherwise the dispatcher's reply is returned in `{ok, Reply}'.

-export([run/2]).

-spec run(Parsed :: term(), Timeout :: timeout()) ->
    {ok, json_rpc_dispatcher:reply()}
    | {error, timeout}
    | {error, {crash, Class :: atom(), Reason :: term()}}.
run(Parsed, Timeout) ->
    Caller = self(),
    {Pid, MRef} = spawn_monitor(fun() ->
        Reply = json_rpc_dispatcher:dispatch(Parsed),
        Caller ! {?MODULE, self(), Reply}
    end),
    receive
        {?MODULE, Pid, Reply} ->
            erlang:demonitor(MRef, [flush]),
            {ok, Reply};
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, {crash, exit, Reason}}
    after Timeout ->
        exit(Pid, kill),
        receive
            {'DOWN', MRef, process, Pid, _} -> ok
        end,
        %% A late result message could already be in our mailbox if the
        %% worker delivered just before the kill landed; flush it.
        receive
            {?MODULE, Pid, _} -> ok
        after 0 -> ok
        end,
        {error, timeout}
    end.
