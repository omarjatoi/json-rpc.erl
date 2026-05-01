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

-export([
    subtract/1,
    sum/1,
    get_data/1,
    update/1,
    notify_sum/1,
    notify_hello/1,
    throw_error/1,
    throw_reserved_error/1,
    slow/1
]).

subtract([A, B]) -> A - B.

sum([A, B, C]) -> A + B + C.

get_data(_) -> [<<"hello">>, 5].

update(_) -> ok.

notify_sum(_) -> ok.

notify_hello(_) -> ok.

throw_error(_) ->
    throw({jsonrpc_error, -1, <<"bad arg">>, #{<<"detail">> => <<"oops">>}}).

%% A handler attempting to impersonate a framework-reserved error code. The
%% dispatcher must intercept this and substitute -32603 Internal error.
throw_reserved_error(_) ->
    throw({jsonrpc_error, -32601, <<"fake method not found">>}).

%% Sleeps for the number of milliseconds passed as a single positional param.
slow([Ms]) when is_integer(Ms), Ms >= 0 ->
    timer:sleep(Ms),
    <<"done">>.
