-module(push_dispatcher).

-behaviour(gen_server).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start/0, start_link/0, stop/0]).
-export([handle_call/3, init/1, terminate/2]).
-export([send_broadcast/2]).

start() ->
    push_dispatcher_sup:start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{timeout, 5000}]).

stop() ->
    ok.

init([]) ->
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

handle_call({send_broadcast, App, Payload}, _From, State) ->
    lists:map(fun(DeviceToken) -> create_notification(App, DeviceToken, Payload) end, App:device_tokens()),
    WorkerName = list_to_atom("push_dispatcher_ios_" ++ App:id()),
    case lists:member(WorkerName, registered()) of
        true -> gen_fsm:send_event(WorkerName, send);
        false -> push_dispatcher_ios:start(App)
    end,
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

send_broadcast(App, Payload) ->
    gen_server:call(?MODULE, {send_broadcast, App, Payload}).

create_notification(App, DeviceToken, Payload) ->
    %% TODO: handle errors
    Notification = boss_record:new(notification, [{app_id, App:id()}, {device_token_id, DeviceToken:id()}, {payload, term_to_binary(Payload)}]),
    Notification:save().