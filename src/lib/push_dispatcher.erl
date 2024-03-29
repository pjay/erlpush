-module(push_dispatcher).

-behaviour(gen_server).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start/0, start_link/0, stop/0]).
-export([handle_call/3, init/1, terminate/2]).
-export([send_broadcast_ios/2, send_broadcast_gcm/2]).

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

handle_call({send_broadcast_ios, App, Payload}, _From, State) ->
    lists:map(fun(DeviceToken) -> create_notification(App, DeviceToken, Payload) end, App:device_tokens()),
    WorkerName = list_to_atom("push_dispatcher_ios_" ++ App:id()),
    case lists:member(WorkerName, registered()) of
        true -> gen_fsm:send_event(WorkerName, send);
        false -> push_dispatcher_ios:start(App)
    end,
    {reply, ok, State};
handle_call({send_broadcast_gcm, App, Payload}, _From, State) ->
    Messages = lists:foldl(fun(Reg, Acc) ->
        case create_message(App, Reg, Payload) of
            undefined -> Acc;
            Message -> [Message | Acc]
        end
    end, [], App:registrations()),
    % Split the list of messages by slices of 1000 elements (GCM limit) and start a worker for each slice
    lists:foreach(fun (L) -> push_dispatcher_gcm:start(App, L) end, partition_list(lists:reverse(Messages), 1000)),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

send_broadcast_ios(App, Payload) ->
    gen_server:call(?MODULE, {send_broadcast_ios, App, Payload}).

send_broadcast_gcm(App, Payload) ->
    gen_server:call(?MODULE, {send_broadcast_gcm, App, Payload}).

%% @todo Handle errors
create_notification(App, DeviceToken, Payload) ->
    Notification = boss_record:new(notification, [{app_id, App:id()}, {device_token_id, DeviceToken:id()}, {payload, term_to_binary(Payload)}]),
    Notification:save().

%% @todo Handle errors
create_message(App, Registration, Payload) ->
    Message = boss_record:new(message, [{app_id, App:id()}, {registration_id, Registration:id()}, {payload, term_to_binary(Payload)}]),
    case Message:save() of
        {ok, SavedMessage} -> SavedMessage;
        {error, _ErrorList} -> undefined
    end.

partition_list(List, Size) when Size =:= 0 ->
    erlang:error(badarg, [List, Size]);
partition_list(List, Size) when Size > length(List) ->
    [List];
partition_list(List, Size) ->
    partition_list(List, Size, []).

partition_list(List, Size, Acc) when Size > length(List) ->
    lists:reverse([List|Acc]);
partition_list(List, Size, Acc) ->
    case lists:split(Size, List) of
        {List1, List2} when length(List2) > 0 ->
            partition_list(List2, Size, [List1|Acc]);
        {List1, _List2} ->
            lists:reverse([List1|Acc])
    end.