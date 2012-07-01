-module(push_dispatcher_sup).

-behaviour(supervisor).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, [
        {push_dispatcher, {push_dispatcher, start_link, []}, permanent, 5000, worker, [push_dispatcher]}
    ]}}.