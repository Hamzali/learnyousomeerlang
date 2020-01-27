%% API module for sweet interface.
-module(ppool).
-behaviour(application).

-export([async_queue/2, run/2, start/2,
	 start_pool/3, stop/1, stop_pool/1, sync_queue/2]).

start(normal, _Args) -> ppool_supersup:start_link().

stop(_State) -> ok.

start_pool(Name, Limit, {M, F, A}) ->
    ppool_supersup:start_pool(Name, Limit, {M, F, A}).

stop_pool(Name) -> ppool_supersup:stop_pool(Name).

run(Name, Args) -> ppool_serv:run(Name, Args).

sync_queue(Name, Args) ->
    ppool_serv:sync_queue(Name, Args).

async_queue(Name, Args) ->
    ppool_serv:async_queue(Name, Args).
