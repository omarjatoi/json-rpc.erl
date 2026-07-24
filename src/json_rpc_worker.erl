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

-moduledoc """
Runs handler code in short-lived monitored processes under a deadline.

Every handler invocation gets its own process. That buys three things the
connection process cannot provide on its own:

- **A deadline.** A handler that blocks forever is killed at the timeout and
  reported as `{error, timeout}` instead of pinning the connection.
- **Crash isolation.** Any exception class, including `exit`, is contained;
  the caller gets `{error, {crash, ...}}` and stays healthy.
- **Heap isolation.** A handler that builds a large intermediate term has
  that garbage collected away with the process, rather than growing the
  long-lived connection process's heap.

`run_many/2` runs a whole batch *concurrently* under a single shared
deadline. The specification explicitly permits processing a batch as a set
of concurrent tasks, and doing so bounds the worst case for a batch of N
slow calls at one timeout rather than N of them.
""".

-export([run/2, run_many/2]).

-doc "What became of one handler invocation.".
-type outcome() ::
    {ok, term()}
    | {error, timeout}
    | {error, {crash, Class :: error | exit | throw, Reason :: term(), erlang:stacktrace()}}.

-doc "An outcome plus how long the invocation actually took, in microseconds.".
-type result() :: #{outcome := outcome(), duration := non_neg_integer()}.

-export_type([outcome/0, result/0]).

-define(TAG, '$json_rpc_worker').

-doc """
Run a single function under `Timeout` milliseconds and return its outcome.
""".
-spec run(fun(() -> term()), timeout()) -> outcome().
run(Fun, Timeout) ->
    [#{outcome := Outcome}] = run_many([Fun], Timeout),
    Outcome.

-doc """
Run every function concurrently under one shared `Timeout`.

Results come back in the order the functions were given, regardless of the
order they finish in. Functions still running when the deadline passes are
killed and reported as `{error, timeout}`.
""".
-spec run_many([fun(() -> term())], timeout()) -> [result()].
run_many([], _Timeout) ->
    [];
run_many(Funs, Timeout) ->
    Started = erlang:monotonic_time(microsecond),
    Deadline = deadline(Started, Timeout),
    Pending = start_workers(Funs),
    Collected = collect(Pending, Deadline, #{}),
    Finished = erlang:monotonic_time(microsecond),
    [format(Index, Collected, Started, Finished) || Index <- lists:seq(1, length(Funs))].

%%% Internal

deadline(_Started, infinity) ->
    infinity;
deadline(Started, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    Started + Timeout * 1000.

%% #{Pid => {MonitorRef, Index}} for every worker still outstanding.
start_workers(Funs) ->
    Caller = self(),
    Indexed = lists:enumerate(Funs),
    maps:from_list([start_worker(Caller, Index, Fun) || {Index, Fun} <- Indexed]).

start_worker(Caller, Index, Fun) ->
    {Pid, MonitorRef} = spawn_monitor(fun() ->
        Caller ! {?TAG, self(), guarded(Fun)}
    end),
    {Pid, {MonitorRef, Index}}.

%% Catch every exception class inside the worker. Letting `exit' escape used
%% to be meaningful when the caller distinguished the two paths, but both
%% end up as the same `-32603' envelope, so containing everything here keeps
%% the reporting uniform and the stacktrace intact.
guarded(Fun) ->
    try
        {ok, Fun()}
    catch
        Class:Reason:Stacktrace -> {error, {crash, Class, Reason, Stacktrace}}
    end.

collect(Pending, _Deadline, Acc) when map_size(Pending) =:= 0 ->
    Acc;
collect(Pending, Deadline, Acc) ->
    receive
        {?TAG, Pid, Outcome} when is_map_key(Pid, Pending) ->
            {{MonitorRef, Index}, Rest} = maps:take(Pid, Pending),
            erlang:demonitor(MonitorRef, [flush]),
            collect(Rest, Deadline, Acc#{Index => {Outcome, elapsed_now()}});
        {'DOWN', _MonitorRef, process, Pid, Reason} when is_map_key(Pid, Pending) ->
            %% The worker died without reporting: it was killed from the
            %% outside, or ran out of heap. `guarded/1' handles everything
            %% raised by the function itself.
            {{_Ref, Index}, Rest} = maps:take(Pid, Pending),
            Crash = {error, {crash, exit, Reason, []}},
            collect(Rest, Deadline, Acc#{Index => {Crash, elapsed_now()}})
    after time_left(Deadline) ->
        abandon(Pending, Acc)
    end.

time_left(infinity) ->
    infinity;
time_left(Deadline) ->
    max(0, (Deadline - erlang:monotonic_time(microsecond) + 999) div 1000).

elapsed_now() ->
    erlang:monotonic_time(microsecond).

%% Kill everything still running and record it as a timeout. The DOWN is
%% guaranteed to arrive after `exit/2', and a result message may already be
%% in flight, so flush both.
abandon(Pending, Acc) ->
    maps:fold(
        fun(Pid, {MonitorRef, Index}, InnerAcc) ->
            exit(Pid, kill),
            receive
                {'DOWN', MonitorRef, process, Pid, _Reason} -> ok
            end,
            receive
                {?TAG, Pid, _Late} -> ok
            after 0 -> ok
            end,
            InnerAcc#{Index => {{error, timeout}, elapsed_now()}}
        end,
        Acc,
        Pending
    ).

%% Timed-out entries are stamped when the deadline fired, which is the same
%% instant for all of them; completed entries carry their own finish time.
format(Index, Collected, Started, Finished) ->
    {Outcome, FinishedAt} = maps:get(Index, Collected, {{error, timeout}, Finished}),
    #{outcome => Outcome, duration => max(0, FinishedAt - Started)}.
