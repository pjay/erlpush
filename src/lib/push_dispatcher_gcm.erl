-module(push_dispatcher_gcm).

-behaviour(gen_fsm).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start/2, start_link/2]).
-export([init/1, terminate/3, code_change/4]).
-export([idle/2]).

-define(API_ENDPOINT, "https://android.googleapis.com/gcm/send").
-define(CONNECT_TIMEOUT, 30 * 1000).
-define(REQUEST_TIMEOUT, 60 * 1000).

-record(state, {app, messages, httpc_pid = undefined}).

start(App, Messages) ->
    push_dispatcher_gcm_sup:start_child(App, Messages).

start_link(App, Messages) ->
    gen_fsm:start_link(?MODULE, [App, Messages], []).

init([App, Messages]) ->
    gen_fsm:send_event(self(), send),
    {ok, idle, #state{app = App, messages = Messages}}.

terminate(_Reason, _StateName, State = #state{httpc_pid = HttpcPid}) ->
    case HttpcPid of
        undefined -> ok;
        Pid ->
            inets:stop(httpc, Pid),
            ok
    end.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

idle(send, State = #state{app = App}) ->
    case inets:start(httpc, [{profile, list_to_atom("httpc_gcm_" ++ App:id())}]) of
        {ok, Pid} ->
            send_request(State, Pid);
        {error, {already_started, Pid}} ->
            send_request(State, Pid);
        {error, Reason} ->
            error_logger:error_msg("Cannot start httpc with reason ~p~n", [Reason]),
            {stop, cannot_start_httpc, State}
    end.

build_request_body(Messages) ->
    RegistrationIDs = lists:foldl(fun(Message, Acc) ->
        Reg = Message:registration(),
        lists:append(Acc, [list_to_binary(Reg:value())])
    end, [], Messages),
    Message = hd(Messages),
    Body = [{<<"registration_ids">>, RegistrationIDs}, {<<"data">>, binary_to_term(Message:payload())}],
    jsx:term_to_json(Body).

send_request(State = #state{app = App, messages = Messages}, Pid) ->
    Headers = [{"Authorization", "key=" ++ App:gcm_api_key()}],
    Request = {?API_ENDPOINT, Headers, "application/json", build_request_body(Messages)},
    HTTPOptions = [{connect_timeout, ?CONNECT_TIMEOUT}, {timeout, ?REQUEST_TIMEOUT}],
    Options = [{body_format, binary}],
    {Time, RetValue} = timer:tc(httpc, request, [post, Request, HTTPOptions, Options, Pid]),
    case RetValue of
        {ok, Result} -> process_result(Result, Time, State);
        {error, Reason} -> process_error(Reason, State)
    end.

%% @todo Write this function
process_result({{_HTTPVersion, 200, _Status}, Headers, Body}, Time, State = #state{app = App, messages = Messages}) ->
    % Message was processed successfully
    Response = jsx:json_to_term(Body),
    Success = proplists:get_value(<<"success">>, Response),
    Failure = proplists:get_value(<<"failure">>, Response),
    CanonicalIDs = proplists:get_value(<<"canonical_ids">>, Response),
    case Failure =:= 0 andalso CanonicalIDs =:= 0 of
        true ->
            lists:foreach(fun(Msg) ->
                ModifiedMsg = Msg:set(delivery_time, calendar:universal_time()),
                ModifiedMsg:save()
            end, Messages),
            gen_event:notify(push_dispatcher_logger, {info, App:id(), "Sent ~p messages in ~.3f seconds", [length(Messages), Time/1000000]}),
            {stop, normal, State};
        false ->
            {stop, normal, State}
    end;
process_result({{_HTTPVersion, 400, _Status}, Headers, _Body}, _Time, State) ->
    % Invalid JSON format or invalid fields
    {stop, normal, State};
process_result({{_HTTPVersion, 401, _Status}, Headers, _Body}, _Time, State) ->
    % Error authenticating the sender account
    {stop, normal, State};
process_result({{_HTTPVersion, 500, _Status}, Headers, _Body}, _Time, State) ->
    % Internal server error
    {stop, normal, State};
process_result({{_HTTPVersion, 503, _Status}, Headers, _Body}, _Time, State) ->
    % Server temporary unavailable
    {stop, normal, State};
process_result(_Result, _Time, State) ->
    % Unknown error
    {stop, normal, State}.

process_error({connect_failed, Reason}, State = #state{app = App}) ->
    gen_event:notify(push_dispatcher_logger, {error, App:id(), "Cannot connect to the GCM server - will retry in 10 seconds"}),
    gen_fsm:send_event_after(10 * 1000, send),
    {next_state, idle, State};
process_error({send_failed, Reason}, State = #state{app = App}) ->
    gen_event:notify(push_dispatcher_logger, {error, App:id(), "Cannot send request to the GCM server - will retry in 10 seconds"}),
    gen_fsm:send_event_after(10 * 1000, send),
    {next_state, idle, State};
process_error(Reason, State = #state{app = App}) ->
    gen_event:notify(push_dispatcher_logger, {error, App:id(), "Unknown error when sending request to the GCM server (~p) - will retry in 10 seconds", [Reason]}),
    gen_fsm:send_event_after(10 * 1000, send),
    {next_state, idle, State}.