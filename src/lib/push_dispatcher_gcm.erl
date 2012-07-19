-module(push_dispatcher_gcm).

-behaviour(gen_fsm).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start/2, start_link/2]).
-export([init/1, terminate/3, code_change/4]).
-export([idle/2]).

-define(API_ENDPOINT, "https://android.googleapis.com/gcm/send").
-define(CONNECT_TIMEOUT, 30 * 1000).
-define(REQUEST_TIMEOUT, 60 * 1000).
-define(MAX_BACKOFF_DELAY, 60 * 1000).

-record(state, {app, messages, httpc_pid = undefined, backoff_delay = 1}).

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
            send_request(State#state{httpc_pid = Pid});
        {error, {already_started, Pid}} ->
            send_request(State#state{httpc_pid = Pid});
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

send_request(State = #state{app = App, messages = Messages, httpc_pid = HttpcPid}) ->
    Headers = [{"Authorization", "key=" ++ App:gcm_api_key()}],
    Request = {?API_ENDPOINT, Headers, "application/json", build_request_body(Messages)},
    HTTPOptions = [{connect_timeout, ?CONNECT_TIMEOUT}, {timeout, ?REQUEST_TIMEOUT}],
    Options = [{body_format, binary}],
    {Time, RetValue} = timer:tc(httpc, request, [post, Request, HTTPOptions, Options, HttpcPid]),
    case RetValue of
        {ok, Result} -> process_result(Result, Time, State);
        {error, Reason} -> process_error(Reason, State)
    end.

process_result({{_HTTPVersion, 200, _Status}, Headers, Body}, Time, State = #state{app = App, messages = Messages}) ->
    % Messages were processed successfully
    Response = jsx:json_to_term(Body),
    Success = proplists:get_value(<<"success">>, Response),
    Failure = proplists:get_value(<<"failure">>, Response),
    CanonicalIDs = proplists:get_value(<<"canonical_ids">>, Response),
    case Failure =:= 0 andalso CanonicalIDs =:= 0 of
        true ->
            % No need to further interpret the response, mark all messages as delivered and stop
            lists:foreach(fun(Msg) ->
                ModifiedMsg = Msg:set(delivery_time, calendar:universal_time()),
                ModifiedMsg:save()
            end, Messages),
            gen_event:notify(push_dispatcher_logger, {info, App:id(), "Delivered ~p GCM messages successfully in ~.3f seconds", [length(Messages), Time/1000000]}),
            {stop, normal, State};
        false ->
            gen_event:notify(push_dispatcher_logger, {info, App:id(), "Delivered ~p GCM messages (~p successes, ~p failures) in ~.3f seconds", [length(Messages), Success, Failure, Time/1000000]}),
            ResultItems = proplists:get_value(<<"results">>, Response),
            case process_json_result_items(ResultItems, {1, []}, State) of
                RetryMessages when length(RetryMessages) > 1 ->
                    gen_fsm:send_event_after(500, send),
                    {next_state, idle, State#state{messages = RetryMessages}};
                _ -> {stop, normal, State}
            end
    end;
process_result({{_HTTPVersion, 400, _Status}, Headers, Body}, _Time, State = #state{app = App}) ->
    % Invalid JSON format or invalid fields
    error_logger:error_msg("HTTP Status Code 400 received from GCM server~n~p~n~p~n", [Headers, Body]),
    gen_event:notify(push_dispatcher_logger, {error, App:id(), "Invalid request (JSON format or invalid fields) - please contact the developer"}),
    {stop, normal, State};
process_result({{_HTTPVersion, 401, _Status}, Headers, Body}, _Time, State = #state{app = App}) ->
    % Authentication error
    error_logger:error_msg("HTTP Status Code 401 received from GCM server~n~p~n~p~n", [Headers, Body]),
    gen_event:notify(push_dispatcher_logger, {error, App:id(), "Authentication error - please check the GCM API Key"}),
    {stop, normal, State};
process_result({{_HTTPVersion, StatusCode, _Status}, Headers, _Body}, _Time, State = #state{app = App, backoff_delay = BackoffDelay}) when StatusCode =:= 500; StatusCode =:= 503 ->
    % Internal server error (500) or Server temporary unavailable (503)
    case proplists:get_value("retry-after", Headers) of
        undefined ->
            gen_event:notify(push_dispatcher_logger, {error, App:id(), "GCM server returned HTTP status code ~p without Retry-After - will retry in ~p seconds", [StatusCode, BackoffDelay]}),
            retry_with_exponential_backoff(State);
        RetryAfter ->
            case string:to_integer(RetryAfter) of
                {Int, Rest} when is_integer(Int), length(Rest) =:= 0 ->
                    gen_event:notify(push_dispatcher_logger, {error, App:id(), "GCM server returned HTTP status code ~p with Retry-After - will retry in ~p seconds", [StatusCode, Int]}),
                    gen_fsm:send_event_after(Int * 1000, send),
                    {next_state, idle, State};
                {error, _Reason} ->
                    %% @todo Try to parse an HTTP date
                    gen_event:notify(push_dispatcher_logger, {error, App:id(), "GCM server returned HTTP status code ~p with unparsable Retry-After - will retry in ~p seconds", [StatusCode, BackoffDelay]}),
                    retry_with_exponential_backoff(State)
            end
    end;
process_result(Result, _Time, State = #state{app = App, backoff_delay = BackoffDelay}) ->
    % Unknown error
    error_logger:error_msg("Unknown error received from GCM server~n~p~n", [Result]),
    gen_event:notify(push_dispatcher_logger, {error, App:id(), "GCM server returned an unknown error - will retry in ~p seconds", [BackoffDelay]}),
    retry_with_exponential_backoff(State).

process_error({connect_failed, Reason}, State = #state{app = App, backoff_delay = BackoffDelay}) ->
    gen_event:notify(push_dispatcher_logger, {error, App:id(), "Cannot connect to the GCM server - will retry in ~p seconds", [BackoffDelay]}),
    retry_with_exponential_backoff(State);
process_error({send_failed, Reason}, State = #state{app = App, backoff_delay = BackoffDelay}) ->
    gen_event:notify(push_dispatcher_logger, {error, App:id(), "Cannot send request to the GCM server - will retry in ~p seconds", [BackoffDelay]}),
    retry_with_exponential_backoff(State);
process_error(Reason, State = #state{app = App, backoff_delay = BackoffDelay}) ->
    % Unknown error
    error_logger:error_msg("Unknown error received when connecting or sending request to the GCM server~n~p~n", [Reason]),
    gen_event:notify(push_dispatcher_logger, {error, App:id(), "Unknown error when sending request to the GCM server - will retry in ~p seconds", [BackoffDelay]}),
    retry_with_exponential_backoff(State).

retry_with_exponential_backoff(State = #state{app = App, backoff_delay = BackoffDelay}) ->
    gen_fsm:send_event_after(BackoffDelay * 1000, send),
    NewBackoffDelay = min(BackoffDelay * 2, ?MAX_BACKOFF_DELAY),
    {next_state, idle, State#state{backoff_delay = NewBackoffDelay}}.

process_json_result_items(undefined, {_Index, List}, _State) ->
    List;
process_json_result_items([], {_Index, List}, _State) ->
    List;
process_json_result_items([Item | Rest], {Index, List}, State = #state{messages = Messages}) ->
    MessageId = proplists:get_value(<<"message_id">>, Item),
    RegistrationID = proplists:get_value(<<"registration_id">>, Item),
    Message = lists:nth(Index, Messages),
    case MessageId =/= undefined andalso RegistrationID =/= undefined of
        true ->
            % Replace the old registration ID with the new one
            Reg = Message:registration(),
            ModifiedReg = Reg:set([{value = RegistrationID}]),
            ModifiedReg:save(),
            process_json_result_items(Rest, {Index + 1, List}, State);
        false ->
            case proplists:get_value(<<"error">>, Item) of
                undefined ->
                    case MessageId of
                        Id when is_list(Id) -> update_delivery_time(Message);
                        _ -> ok
                    end,
                    process_json_result_items(Rest, {Index + 1, List}, State);
                <<"InvalidRegistration">> ->
                    delete_associated_registration(Index, Messages),
                    process_json_result_items(Rest, {Index + 1, List}, State);
                <<"Unavailable">> ->
                    process_json_result_items(Rest, {Index + 1, [Message | List]}, State);
                <<"InternalServerError">> ->
                    process_json_result_items(Rest, {Index + 1, [Message | List]}, State);
                <<"NotRegistered">> ->
                    delete_associated_registration(Index, Messages),
                    process_json_result_items(Rest, {Index + 1, List}, State);
                <<"MismatchSenderId">> ->
                    delete_associated_registration(Index, Messages),
                    process_json_result_items(Rest, {Index + 1, List}, State);
                <<"MessageTooBig">> ->
                    % Mark it as delivered, won't retry sending it again
                    update_delivery_time(Message),
                    process_json_result_items(Rest, {Index + 1, List}, State);
                <<"InvalidTtl">> ->
                    % Mark it as delivered, won't retry sending it again
                    update_delivery_time(Message),
                    process_json_result_items(Rest, {Index + 1, List}, State);
                Other ->
                    error_logger:warning_msg("Unknown error '~p' encountered when parsing result items~n", [Other]),
                    % Mark it as delivered, won't retry sending it again
                    update_delivery_time(Message),
                    process_json_result_items(Rest, {Index + 1, List}, State)
            end
    end.

update_delivery_time(Msg) ->
    ModifiedMsg = Msg:set(delivery_time, calendar:universal_time()),
    ModifiedMsg:save().

delete_associated_registration(Index, Messages) ->
    Msg = lists:nth(Index, Messages),
    Reg = Msg:registration(),
    boss_db:delete(Reg:id()).