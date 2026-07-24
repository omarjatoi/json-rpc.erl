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

-ifndef(JSON_RPC_HRL).
-define(JSON_RPC_HRL, true).

%% The only protocol version this library speaks.
-define(JSONRPC_VERSION, <<"2.0">>).

%% Pre-defined error codes from the JSON-RPC 2.0 specification.
-define(JSONRPC_PARSE_ERROR, -32700).
-define(JSONRPC_INVALID_REQUEST, -32600).
-define(JSONRPC_METHOD_NOT_FOUND, -32601).
-define(JSONRPC_INVALID_PARAMS, -32602).
-define(JSONRPC_INTERNAL_ERROR, -32603).

%% The specification reserves -32768..-32000 as a whole, but carves out
%% -32099..-32000 as "Server error — reserved for implementation-defined
%% server-errors". Application handlers are welcome to use that sub-range;
%% everything below it belongs to the framework and is rejected if a handler
%% tries to emit it (see `json_rpc_error:is_reserved/1').
-define(JSONRPC_SERVER_ERROR_MIN, -32099).
-define(JSONRPC_SERVER_ERROR_MAX, -32000).
-define(JSONRPC_RESERVED_MIN, -32768).
-define(JSONRPC_RESERVED_MAX, -32100).

-endif.
