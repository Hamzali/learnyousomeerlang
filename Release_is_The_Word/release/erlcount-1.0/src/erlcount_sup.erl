-module(erlcount_sup).

-behaviour(supervisor).

-export([init/1, start_link/0]).

start_link() -> supervisor:start_link(?MODULE, []).

init([]) ->
    {ok,
     {{one_for_one, 5, 100}, % strategy
      %% child specs
      [{dispatch, % child name
	{erlcount_dispatch, start_link,
	 []}, % Module Function Args
	transient, % life type
	60000, % shutdown time
	worker, % child type
	[erlcount_dispatch]}]}}. % ?

