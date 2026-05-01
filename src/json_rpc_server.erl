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

-module(json_rpc_server).

-behaviour(gen_server).

-export([start_link/0, register_method/2, unregister_method/1, get_methods/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {methods = #{} :: json_rpc_dispatcher:methods()}).

%% API

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register_method(binary(), json_rpc_dispatcher:method_fun()) -> ok.
register_method(Name, Fun) when is_binary(Name), is_function(Fun, 1) ->
    gen_server:call(?SERVER, {register_method, Name, Fun}, 5000).

-spec unregister_method(binary()) -> ok.
unregister_method(Name) when is_binary(Name) ->
    gen_server:call(?SERVER, {unregister_method, Name}, 5000).

-spec get_methods() -> json_rpc_dispatcher:methods().
get_methods() ->
    gen_server:call(?SERVER, get_methods, 5000).

%% gen_server callbacks

-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

handle_call({register_method, Name, Fun}, _From, State) ->
    NewMethods = maps:put(Name, Fun, State#state.methods),
    {reply, ok, State#state{methods = NewMethods}};
handle_call({unregister_method, Name}, _From, State) ->
    NewMethods = maps:remove(Name, State#state.methods),
    {reply, ok, State#state{methods = NewMethods}};
handle_call(get_methods, _From, State) ->
    {reply, State#state.methods, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
