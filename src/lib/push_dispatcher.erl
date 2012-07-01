-module(push_dispatcher).

-behaviour(gen_server).

-author('Philippe Jayet <philippe@easybox.ch>').

-export([start/0, stop/0, start_link/0, init/1]).

-export([send_broadcast/2]).

start() ->
    push_dispatcher_sup:start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{timeout, 5000}]).

init([]) ->
    {ok, []}.

stop() ->
    ok.

send_broadcast(App, Payload) ->
    
    ok.