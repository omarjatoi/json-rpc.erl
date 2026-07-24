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

-module(json_rpc_drain_handler).

-moduledoc """
Answers `503` while the listener is draining.

`m:json_rpc_listener` swaps every route to this handler at the start of
shutdown, so work that arrives during the drain window is refused instead of
being started and then cut off. `Connection: close` stops clients from
holding a keep-alive socket open against a node that is going away.

The body is a JSON-RPC error envelope like every other error this server
emits, so a client can parse the response without special-casing shutdown.
""".

-behaviour(cowboy_handler).

-include("json_rpc.hrl").

-export([init/2]).

-doc false.
-spec init(cowboy_req:req(), State) -> {ok, cowboy_req:req(), State}.
init(Req0, State) ->
    Error = json_rpc_error:new(
        ?JSONRPC_SERVER_ERROR_MAX,
        <<"Server error">>,
        #{reason => <<"server is shutting down">>}
    ),
    Headers = #{
        <<"content-type">> => <<"application/json">>,
        <<"connection">> => <<"close">>
    },
    Req = cowboy_req:reply(503, Headers, json_rpc_transport:error_body(Error), Req0),
    {ok, Req, State}.
