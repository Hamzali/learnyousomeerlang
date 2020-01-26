-module(ppool_sup).

-behaviour(supervisor).

-export([init/1, start_link/3]).

start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 36000,
    ChildSpecs = [{serv, % child name?
		  {ppool_serv, start_link,
		   [Name, Limit, self(),
		    MFA]}, % start module function and args
		  permanent, % life type
		  5000, % shutdown time
		  worker, % usage type
		  [ppool_serv]}], % ?
    {ok, {{one_for_all, MaxRestart, MaxTime}, ChildSpecs}}.
