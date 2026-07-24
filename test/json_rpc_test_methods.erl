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

-module(json_rpc_test_methods).

%% Handlers exercised by the suites. Each one isolates a single behaviour of
%% the handler contract so a failing case names the behaviour that broke.

-export([
    subtract/1,
    sum/1,
    get_data/1,
    echo/1,
    update/1,
    notify_sum/1,
    notify_hello/1,
    slow/1,
    crash/1,
    crash_exit/1,
    crash_throw/1,
    throw_error/1,
    throw_error_object/1,
    throw_reserved/1,
    throw_server_error/1,
    return_error_pair/1,
    return_error_triple/1,
    return_error_object/1,
    return_ok_tuple/1,
    unencodable/1,
    unencodable_in_batch/1,
    context/2,
    subscribe_self/2
]).

%%% Ordinary results

%% Both parameter styles from the specification's worked examples: by
%% position, and by name in either key order.
subtract([A, B]) -> A - B;
subtract(#{<<"minuend">> := Minuend, <<"subtrahend">> := Subtrahend}) -> Minuend - Subtrahend.

sum([A, B, C]) -> A + B + C.

get_data(_Params) -> [<<"hello">>, 5].

echo(Params) -> Params.

update(_Params) -> ok.

notify_sum(_Params) -> ok.

notify_hello(_Params) -> ok.

%% Sleeps for the milliseconds given as a single positional parameter.
slow([Ms]) when is_integer(Ms), Ms >= 0 ->
    timer:sleep(Ms),
    <<"done">>.

%%% Crashes. Each class must be contained and reported as -32603.

crash(_Params) -> error(boom).

crash_exit(_Params) -> exit(boom).

crash_throw(_Params) -> throw(boom).

%%% Raised protocol errors

%% The pre-1.0 tuple shape, still supported.
throw_error(_Params) ->
    throw({jsonrpc_error, -1, <<"bad arg">>, #{<<"detail">> => <<"oops">>}}).

%% The current shape, via the documented helper.
throw_error_object(_Params) ->
    json_rpc_error:throw_error(-1, <<"bad arg">>).

%% A handler must not be able to impersonate a framework error code.
throw_reserved(_Params) ->
    throw({jsonrpc_error, -32601, <<"fake method not found">>}).

%% -32000 is inside the implementation-defined server-error range, so this
%% one must pass through untouched.
throw_server_error(_Params) ->
    json_rpc_error:throw_error(-32000, <<"upstream unavailable">>).

%%% Returned protocol errors

return_error_pair(_Params) -> {error, {-32050, <<"pair">>}}.

return_error_triple(_Params) -> {error, {-32051, <<"triple">>, #{extra => true}}}.

return_error_object(_Params) -> {error, json_rpc_error:new(-32052, <<"object">>)}.

return_ok_tuple(_Params) -> {ok, 42}.

%%% Results JSON cannot represent

%% A bare tuple is not encodable. The transport must degrade this one call to
%% -32603 without losing the connection or the rest of a batch.
unencodable(_Params) -> {'this', 'is', 'a', 'tuple'}.

unencodable_in_batch(_Params) -> #{bad => fun() -> ok end}.

%%% Context-aware handlers, registered at arity 2

context(_Params, Context) ->
    #{
        transport => maps:get(transport, Context, undefined),
        has_connection_pid => is_pid(maps:get(connection_pid, Context, undefined)),
        has_request_id => is_binary(maps:get(request_id, Context, undefined))
    }.

%% The reason arity-2 handlers exist: a handler subscribing its own caller.
subscribe_self([Topic], #{connection_pid := Pid}) ->
    ok = json_rpc_ws:subscribe(Pid, binary_to_atom(Topic)),
    <<"subscribed">>.
