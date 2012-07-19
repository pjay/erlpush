-module(push_dispatcher_ios).

-behaviour(gen_fsm).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start/1, start_link/1]).
-export([init/1, terminate/3, code_change/4]).
-export([disconnected/2, connected/2]).
-export([send_notifications/2]).

-define(MAX_BACKOFF_DELAY, 60).

-record(state, {app, socket, backoff_delay = 1}).

start(App) ->
    push_dispatcher_ios_sup:start_child(App).

start_link(App) ->
    gen_fsm:start_link({local, list_to_atom("push_dispatcher_ios_" ++ App:id())}, ?MODULE, [App], []).

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
            gen_event:notify(push_dispatcher_logger, {debug, App:id(), "Connected to the APNS server"}),
            gen_fsm:send_event(self(), send),
            {next_state, connected, State#state{socket = ApnsPid, backoff_delay = 1}};
        {error, {already_started, ApnsPid}} ->
            gen_event:notify(push_dispatcher_logger, {debug, App:id(), "Connected to the APNS server (reused existing connection)"}),
            gen_fsm:send_event(self(), send),
            {next_state, connected, State#state{socket = ApnsPid, backoff_delay = 1}};
        {error, Reason} ->
            gen_event:notify(push_dispatcher_logger, {error, App:id(), "Cannot connect to the APNS server - will retry in ~p seconds", [BackoffDelay]}),
            gen_fsm:send_event_after(BackoffDelay * 1000, send),
            NewBackoffDelay = min(BackoffDelay * 2, ?MAX_BACKOFF_DELAY),
            {next_state, disconnected, State#state{backoff_delay = NewBackoffDelay}}
    end;
disconnected(Event, State) ->
    error_logger:error_report("unknown event ~p received in disconnected state - ignoring", [Event]),
    {next_state, disconnected, State}.

connected(send, State = #state{app = App, socket = Socket}) ->
    Notifications = boss_db:find(notification, [app_id = App:id(), delivery_time = undefined]),
    {Time, _} = timer:tc(?MODULE, send_notifications, [Socket, Notifications]),
    gen_event:notify(push_dispatcher_logger, {info, App:id(), "Sent ~p notifications in ~.3f seconds", [length(Notifications), Time/1000000]}),
    {next_state, connected, State, 3600 * 1000};  % timeout after 1 hour
connected(timeout, State = #state{app = App}) ->
    gen_event:notify(push_dispatcher_logger, {debug, App:id(), "Stopping worker after timeout"}),
    {stop, normal, State};
connected(Event, State) ->
    error_logger:error_report("unknown event ~p received in connected state - ignoring", [Event]),
    {next_state, connected, State}.

send_notifications(Socket, Notifications) ->
    lists:map(fun(Notification) -> send_notification(Socket, Notification) end, Notifications).

%% @todo Handle ex_apns and DB errors
send_notification(Socket, Notification) ->
    Token = Notification:device_token(),
    boss_db:transaction(fun() ->
        ModifiedNotification = Notification:set(delivery_time, calendar:universal_time()),
        ModifiedNotification:save(),
        ex_apns:send(Socket, Token:value(), binary_to_term(Notification:payload()))
    end).