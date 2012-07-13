-module(push_dispatcher_logger_db).

-behaviour(gen_event).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start_link/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-define(CRITICAL, 0).
-define(ERROR, 1).
-define(WARNING, 2).
-define(INFO, 3).
-define(DEBUG, 4).

start_link() ->
    Ret = gen_event:start_link({local, push_dispatcher_logger}),
    gen_event:add_handler(push_dispatcher_logger, ?MODULE, []),
    Ret.

init(_Args) ->
    {ok, []}.

handle_event({critical, AppId, Format, Data}, State) ->
    log(?CRITICAL, AppId, Format, Data),
    {ok, State};
handle_event({critical, AppId, Message}, State) ->
    log(?CRITICAL, AppId, Message),
    {ok, State};
handle_event({error, AppId, Format, Data}, State) ->
    log(?ERROR, AppId, Format, Data),
    {ok, State};
handle_event({error, AppId, Message}, State) ->
    log(?ERROR, AppId, Message),
    {ok, State};
handle_event({warning, AppId, Format, Data}, State) ->
    log(?WARNING, AppId, Format, Data),
    {ok, State};
handle_event({warning, AppId, Message}, State) ->
    log(?WARNING, AppId, Message),
    {ok, State};
handle_event({info, AppId, Format, Data}, State) ->
    log(?INFO, AppId, Format, Data),
    {ok, State};
handle_event({info, AppId, Message}, State) ->
    log(?INFO, AppId, Message),
    {ok, State};
handle_event({debug, AppId, Format, Data}, State) ->
    log(?DEBUG, AppId, Format, Data),
    {ok, State};
handle_event({debug, AppId, Message}, State) ->
    log(?DEBUG, AppId, Message),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, undefined, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

log(Severity, AppId, Format, Data) ->
    log(Severity, AppId, io_lib:format(Format, Data)).

log(Severity, AppId, Message) ->
    NewRecord = boss_record:new(event, [{app_id, AppId}, {creation_time, calendar:universal_time()}, {severity, Severity}, {message, Message}]),
    case NewRecord:save() of
        {ok, _SavedRecord} -> ok;
        {error, ErrorList} ->
            error_logger:error_report(ErrorList),
            {error, ErrorList}
    end.