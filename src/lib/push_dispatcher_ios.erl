-module(push_dispatcher_ios).

-behaviour(gen_fsm).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start/1, start_link/1]).
-export([init/1, terminate/3, code_change/4]).
-export([connecting/2, sending/2]).

-record(state, {app, socket}).

start(App) ->
    push_dispatcher_ios_sup:start_child(App).

start_link(App) ->
    gen_fsm:start_link(?MODULE, [App], []).

init([App]) ->
    gen_fsm:send_event(self(), send),
    {ok, connecting, #state{app = App}}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

connecting(send, State = #state{app = App}) ->
    ex_apns:start(),
    case ex_apns:start_link('apns_sender', development, App:cert_path()) of
        {ok, ApnsPid} ->
            error_logger:info_report("connected successfully"),
            gen_fsm:send_event(self(), send),
            {next_state, sending, State#state{socket = ApnsPid}};
        {error, {already_started, ApnsPid}} ->
            error_logger:info_report("connected successfully"),
            gen_fsm:send_event(self(), send),
            {next_state, sending, State#state{socket = ApnsPid}};
        {error, Reason} ->
            error_logger:error_report(Reason),
            %boss_flash:add(SessionID, error, "Error sending message", ""),
            {stop, Reason, State}
    end;
connecting(Event, State) ->
    error_logger:error_report("unknown event ~p received in connecting state - ignoring", [Event]),
    {next_state, connecting, State}.

sending(send, State = #state{socket = Socket, app = App}) ->
    Notifications = boss_db:find(notification, [app_id = App:id(), delivery_time = undefined]),
    lists:map(fun(Notification) -> send_notification(Socket, Notification) end, Notifications),
    {stop, normal, State};
sending(Event, State) ->
    error_logger:error_report("unknown event ~p received in sending state - ignoring", [Event]),
    {next_state, sending, State}.

send_notification(Socket, Notification) ->
    Token = Notification:device_token(),
    ex_apns:send(Socket, Token:value(), binary_to_term(Notification:payload())),
    ModifiedNotification = Notification:set(delivery_time, calendar:universal_time()),
    ModifiedNotification:save().