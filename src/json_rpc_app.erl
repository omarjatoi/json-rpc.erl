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

-module(json_rpc_app).

-moduledoc """
Application callback module.

Configuration is validated in full before the supervisor starts, so a bad
value fails the application start naming the offending key, rather than
surfacing later as a listener that will not bind or a timeout that behaves
strangely.
""".

-behaviour(application).

-export([start/2, stop/1]).

-doc false.
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Returned rather than raised: a start callback that returns
    %% `{error, Reason}' produces a clean `{error, {invalid_config, ...}}'
    %% from application:start/1, instead of burying the offending key inside
    %% a bad_return wrapper around an EXIT and a stacktrace.
    try json_rpc_config:validate_all() of
        ok -> json_rpc_sup:start_link()
    catch
        error:{invalid_config, _Key, _Value, _Reason} = Invalid ->
            {error, Invalid}
    end.

-doc false.
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
