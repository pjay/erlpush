-module(push_dispatcher_ios_sup).

-behaviour(supervisor).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(App) ->
    supervisor:start_child(?MODULE, [App]).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10}, [
        {push_dispatcher_ios, {push_dispatcher_ios, start_link, []}, transient, 5000, worker, [push_dispatcher_ios]}
    ]}}.