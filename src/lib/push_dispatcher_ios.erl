-module(push_dispatcher_ios).

-behaviour(gen_fsm).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start/1, start_link/1]).
-export([init/1, terminate/3, code_change/4]).
-export([disconnected/2, connected/2]).

-record(state, {app, socket, backoff_delay = 1}).

start(App) ->
    push_dispatcher_ios_sup:start_child(App).

start_link(App) ->
    gen_fsm:start_link(?MODULE, [App], []).

init([App]) ->
    gen_fsm:send_event(self(), send),
    {ok, disconnected, #state{app = App}}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

disconnected(send, State = #state{app = App, socket = Socket, backoff_delay = BackoffDelay}) ->
    ex_apns:start(),
    case ex_apns:start(list_to_atom("apns_sender_" ++ App:id()), list_to_atom(App:app_mode()), App:cert_path()) of
        {ok, ApnsPid} ->
            %error_logger:info_report("connected"),
            gen_fsm:send_event(self(), send),
            {next_state, connected, State#state{socket = ApnsPid, backoff_delay = 1}};
        {error, {already_started, ApnsPid}} ->
            %error_logger:info_report("connected (already started)"),
            gen_fsm:send_event(self(), send),
            {next_state, connected, State#state{socket = ApnsPid, backoff_delay = 1}};
        {error, Reason} ->
            error_logger:error_report(Reason),
            gen_fsm:send_event_after(BackoffDelay * 1000, send),
            NewBackoffDelay = min(BackoffDelay * 2, 60),
            {next_state, disconnected, State#state{backoff_delay = NewBackoffDelay}}
    end;
disconnected(Event, State) ->
    error_logger:error_report("unknown event ~p received in disconnected state - ignoring", [Event]),
    {next_state, disconnected, State}.

connected(send, State = #state{app = App, socket = Socket}) ->
    %error_logger:info_report("sending notifications"),
    Notifications = boss_db:find(notification, [app_id = App:id(), delivery_time = undefined]),
    lists:map(fun(Notification) -> send_notification(Socket, Notification) end, Notifications),
    %error_logger:info_report("notifications sent"),
    {next_state, connected, State, 3600 * 1000};  % timeout after 1 hour
connected(timeout, State) ->
    %error_logger:info_report("stopping fsm after timeout"),
    {stop, normal, State};
connected(Event, State) ->
    error_logger:error_report("unknown event ~p received in connected state - ignoring", [Event]),
    {next_state, connected, State}.

send_notification(Socket, Notification) ->
    Token = Notification:device_token(),
    ex_apns:send(Socket, Token:value(), binary_to_term(Notification:payload())),
    ModifiedNotification = Notification:set(delivery_time, calendar:universal_time()),
    ModifiedNotification:save().