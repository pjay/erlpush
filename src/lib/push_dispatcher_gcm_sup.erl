-module(push_dispatcher_gcm_sup).

-behaviour(supervisor).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start_link/0, start_child/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(App, Messages) ->
    supervisor:start_child(?MODULE, [App, Messages]).

init([]) ->
    %% @todo Error handling when starting the required applications
    ssl:start(),
    inets:start(),
    {ok, {{simple_one_for_one, 10, 10}, [
        {push_dispatcher_gcm, {push_dispatcher_gcm, start_link, []}, transient, 5000, worker, [push_dispatcher_gcm]}
    ]}}.