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

-module(json_rpc_dispatcher).

-moduledoc """
Routing of decoded JSON-RPC payloads to registered handlers.

Takes one already-decoded JSON value — a single Request object or a batch
array — and produces the Response term the transport should send back, or
`no_response` when the payload consisted only of Notifications.

## Batches

A batch is validated as a whole first: an empty array and an array longer
than `max_batch_size` are both `-32600 Invalid Request`, answered as a single
Response rather than an array. Otherwise every element is validated
independently, and the valid ones run **concurrently** under one shared
deadline. The specification explicitly allows processing a batch as a set of
concurrent tasks, and it means a batch of ten slow calls costs one timeout
rather than ten.

Responses come back in request order. Notifications contribute nothing to the
array, whatever their handler did or how badly it failed.

## Handler contract

A handler is called with the request's `params`, plus the context if it is
registered at arity 2. Its return value is interpreted as:

- `{error, Error}` — an error Response, where `Error` is anything
  `json_rpc_error:from_term/1` accepts.
- `{ok, Result}` — a successful Response carrying `Result`.
- any other term — a successful Response carrying that term.

`{ok, _}` and `{error, _}` are unambiguous as control tuples because a bare
tuple is not JSON-encodable, so no handler can want to return one as a result.

A handler may also raise, via `json_rpc_error:throw_error/2,3` for a
deliberate protocol error, or by crashing. A crash, a timeout, and a handler
that tries to claim a framework-reserved error code all become
`-32603 Internal error`; the real reason is logged and reported over
telemetry, never sent to the client.
""".

-include_lib("kernel/include/logger.hrl").

-include("json_rpc.hrl").

-export([dispatch/1, dispatch/2]).

-doc """
Per-request information handed to arity-2 handlers.

`connection_pid` is the transport's connection process. On the WebSocket
transport that is the pid `json_rpc_ws:push/3` and `json_rpc_ws:subscribe/2`
expect, which is how a handler talks back to its own connection.
""".
-type context() :: #{
    transport := json_rpc_telemetry:transport(),
    request_id := binary() | undefined,
    connection_pid := pid(),
    peer := {inet:ip_address(), inet:port_number()} | undefined
}.

-doc "What a transport should send back. `no_response` means send nothing.".
-type reply() :: no_response | json_rpc_response:t() | [json_rpc_response:t()].

-export_type([context/0, reply/0]).

-doc "Dispatch with an empty context. Handlers registered at arity 2 see `#{}`.".
-spec dispatch(json_rpc_json:value()) -> reply().
dispatch(Payload) ->
    dispatch(Payload, #{}).

-doc "Dispatch `Payload`, passing `Context` to any arity-2 handler it reaches.".
-spec dispatch(json_rpc_json:value(), map()) -> reply().
dispatch(Payload, Context) when is_list(Payload) ->
    dispatch_batch(Payload, Context);
dispatch(Payload, Context) when is_map(Payload), map_size(Payload) > 0 ->
    case run_all([json_rpc_request:parse(Payload)], Context) of
        [] -> no_response;
        [Response] -> Response
    end;
dispatch(_Payload, _Context) ->
    %% A bare scalar, or an empty object: not a Request object and not a
    %% batch, so there is nothing to attribute an id to.
    json_rpc_response:error(null, json_rpc_error:invalid_request()).

%%% Internal

dispatch_batch([], _Context) ->
    json_rpc_response:error(null, json_rpc_error:invalid_request());
dispatch_batch(Elements, Context) ->
    Max = json_rpc_config:get(max_batch_size),
    case length(Elements) > Max of
        true ->
            ?LOG_WARNING("json_rpc: rejecting batch of ~p (max_batch_size=~p)", [
                length(Elements), Max
            ]),
            json_rpc_response:error(
                null,
                json_rpc_error:new(
                    ?JSONRPC_INVALID_REQUEST,
                    <<"Invalid Request">>,
                    #{reason => <<"batch too large">>, max_batch_size => Max}
                )
            );
        false ->
            run_batch(Elements, Context)
    end.

run_batch(Elements, Context) ->
    Started = erlang:monotonic_time(microsecond),
    Responses = run_all([json_rpc_request:parse(E) || E <- Elements], Context),
    Duration = erlang:monotonic_time(microsecond) - Started,
    json_rpc_telemetry:batch_stop(length(Elements), Duration, Context),
    case Responses of
        [] -> no_response;
        _ -> Responses
    end.

%% Resolve every parsed element, run the ones that reach a handler
%% concurrently, then reassemble in request order. Elements that failed
%% validation never reach a worker but still hold their place in the list.
run_all(Parsed, Context) ->
    Resolved = [resolve(P, Context) || P <- Parsed],
    Invocations = [I || {invoke, _Answerable, _Method, _Id, I} <- Resolved],
    Results = json_rpc_worker:run_many(Invocations, json_rpc_config:get(handler_timeout_ms)),
    Collect = fun(Element, Remaining) -> collect(Element, Remaining, Context) end,
    {Responses, []} = lists:mapfoldl(Collect, Results, Resolved),
    [R || R <- Responses, R =/= no_response].

%% One parsed element becomes either a ready-made answer or an invocation to
%% run. `Answerable' records whether the client is owed a Response at all:
%% a Notification is owed nothing, however the handler turns out.
resolve({invalid, Id, Error}, _Context) ->
    %% Answered even without an id — a malformed Request is not a
    %% Notification, because we cannot know it was meant to be one.
    {ready, json_rpc_response:error(Id, Error)};
resolve({notification, Method, Params}, Context) ->
    resolve_method(false, Method, null, Params, Context);
resolve({call, Id, Method, Params}, Context) ->
    resolve_method(true, Method, Id, Params, Context).

resolve_method(Answerable, Method, Id, Params, Context) ->
    case json_rpc_methods:lookup(Method) of
        {ok, Handler} ->
            Invocation = invocation(Handler, Params, #{method => Method, id => Id}, Context),
            {invoke, Answerable, Method, Id, Invocation};
        not_found ->
            answer(Answerable, json_rpc_response:error(Id, json_rpc_error:method_not_found()));
        {error, unavailable} ->
            ?LOG_ERROR("json_rpc: method registry unavailable, cannot dispatch ~ts", [Method]),
            answer(Answerable, json_rpc_response:error(Id, json_rpc_error:internal_error()))
    end.

answer(false, _Response) -> {ready, no_response};
answer(true, Response) -> {ready, Response}.

%% Build the zero-arity thunk the worker will run. Logger metadata is set
%% inside the thunk because metadata is process-local and the worker is a
%% fresh process — this is what puts the correlation id on handler logs
%% without the handler doing anything.
invocation({mfa, Module, Function, Arity}, Params, #{method := Method, id := Id}, Context) ->
    fun() ->
        logger:update_process_metadata(#{
            json_rpc_method => Method,
            json_rpc_id => Id,
            json_rpc_request_id => maps:get(request_id, Context, undefined)
        }),
        case Arity of
            1 -> Module:Function(Params);
            2 -> Module:Function(Params, Context)
        end
    end.

collect({ready, Response}, Results, _Context) ->
    {Response, Results};
collect({invoke, Answerable, Method, Id, _Fun}, [Result | Rest], Context) ->
    {response_for(Answerable, Method, Id, Result, Context), Rest}.

response_for(Answerable, Method, Id, #{outcome := Outcome, duration := Duration}, Context) ->
    Response = outcome_response(Method, Id, Outcome, Duration, Context),
    case Answerable of
        true -> Response;
        false -> no_response
    end.

outcome_response(Method, Id, {ok, Returned}, Duration, Context) ->
    case interpret(Returned) of
        {result, Result} ->
            json_rpc_telemetry:request_stop(Method, Duration, ok, Context),
            json_rpc_response:result(Id, Result);
        {error, Error} ->
            error_response(Method, Id, Error, Duration, Context)
    end;
outcome_response(Method, Id, {error, timeout}, Duration, Context) ->
    ?LOG_WARNING("json_rpc: handler for ~ts timed out after ~p us", [Method, Duration]),
    json_rpc_telemetry:request_exception(Method, Duration, timeout, Context),
    json_rpc_response:error(Id, json_rpc_error:internal_error(#{reason => timeout}));
outcome_response(Method, Id, {error, {crash, throw, Thrown, _Stack}}, Duration, Context) when
    element(1, Thrown) =:= jsonrpc_error
->
    thrown_response(Method, Id, Thrown, Duration, Context);
outcome_response(Method, Id, {error, {crash, Class, Reason, Stacktrace}}, Duration, Context) ->
    ?LOG_ERROR(
        "json_rpc: handler for ~ts raised ~p:~p~n~p", [Method, Class, Reason, Stacktrace]
    ),
    json_rpc_telemetry:request_exception(Method, Duration, crash, Context),
    json_rpc_response:error(Id, json_rpc_error:internal_error()).

%% `throw_error/2,3' raises `{jsonrpc_error, Error}'. The three- and
%% four-element forms are the shapes this library documented before 1.0 and
%% are still accepted so existing handlers keep working.
thrown_response(Method, Id, Thrown, Duration, Context) ->
    Term =
        case Thrown of
            {jsonrpc_error, Error} -> Error;
            {jsonrpc_error, Code, Message} -> {Code, Message};
            {jsonrpc_error, Code, Message, Data} -> {Code, Message, Data};
            _Other -> Thrown
        end,
    case json_rpc_error:from_term(Term) of
        {ok, Error1} ->
            error_response(Method, Id, Error1, Duration, Context);
        not_an_error ->
            ?LOG_ERROR("json_rpc: handler for ~ts threw an unusable error: ~p", [Method, Thrown]),
            json_rpc_telemetry:request_exception(Method, Duration, crash, Context),
            json_rpc_response:error(Id, json_rpc_error:internal_error())
    end.

%% A handler may not emit a framework-owned code. Substituting the internal
%% error keeps a handler from impersonating a protocol-level failure such as
%% "Method not found", which a client would reasonably act on.
error_response(Method, Id, Error, Duration, Context) ->
    Code = json_rpc_error:code(Error),
    case json_rpc_error:is_reserved(Code) of
        true ->
            ?LOG_WARNING(
                "json_rpc: handler for ~ts returned reserved code ~p; substituting ~p",
                [Method, Code, ?JSONRPC_INTERNAL_ERROR]
            ),
            json_rpc_telemetry:request_stop(
                Method, Duration, {error, ?JSONRPC_INTERNAL_ERROR}, Context
            ),
            json_rpc_response:error(Id, json_rpc_error:internal_error());
        false ->
            json_rpc_telemetry:request_stop(Method, Duration, {error, Code}, Context),
            json_rpc_response:error(Id, Error)
    end.

%% Distinguish the control tuples from a plain result. Bare tuples cannot be
%% encoded as JSON, so treating them as control costs nothing a handler could
%% legitimately want.
interpret({error, Term}) ->
    case json_rpc_error:from_term(Term) of
        {ok, Error} -> {error, Error};
        not_an_error -> {error, json_rpc_error:internal_error()}
    end;
interpret({ok, Result}) ->
    {result, Result};
interpret(Result) ->
    {result, Result}.
