-module(push_dispatcher_sup).

-behaviour(supervisor).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DispatcherSpec = {push_dispatcher, {push_dispatcher, start_link, []}, permanent, 5000, worker, [push_dispatcher]},
    IosSupSpec = {push_dispatcher_ios_sup, {push_dispatcher_ios_sup, start_link, []}, permanent, 5000, supervisor, [push_dispatcher_ios_sup]},
    DbLoggerSpec = {push_dispatcher_logger_db, {push_dispatcher_logger_db, start_link, []}, permanent, 5000, worker, dynamic},
    {ok, {{one_for_one, 10, 10}, [DispatcherSpec, IosSupSpec, DbLoggerSpec]}}.