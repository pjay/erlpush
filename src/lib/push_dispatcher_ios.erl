-module(push_dispatcher_ios).

-behaviour(gen_fsm).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start/1, start_link/1]).
-export([init/1, terminate/3]).

-record(state, {app, socket}).

start(App) ->
    push_dispatcher_ios_sup:start_child(App).

start_link(App) ->
    gen_fsm:start_link(?MODULE, [App], []).

init([App]) ->
    error_logger:info_report("init"),
    {ok, connecting, #state{app = App}}.

terminate(_Reason, _StateName, _State) ->
    %% TODO: disconnect ex_apns
    ok.

connecting(_Event, State = #state{app = App}) ->
    error_logger:info_report("STATE connecting"),
    ex_apns:start(),
    case ex_apns:start_link('apns_sender', development, App:cert_path()) of
        {ok, ApnsPid} ->
            {next_state, sending, State#state{socket = ApnsPid}};
        {error, {already_started, ApnsPid}} ->
            {next_state, sending, State#state{socket = ApnsPid}};
        {error, Reason} ->
            error_logger:error_report(Reason),
            %boss_flash:add(SessionID, error, "Error sending message", ""),
            {stop, Reason, State}
    end.

sending(_Event, State = #state{socket = Socket, app = App}) ->
    error_logger:info_report("STATE sending"),
    lists:map(fun(Notification) ->
        Token = Notification:device_token(),
        ex_apns:send(Socket, Token:value(), binary_to_term(Notification:payload())) end, App:notifications()),
    {stop, "Finished", State}.