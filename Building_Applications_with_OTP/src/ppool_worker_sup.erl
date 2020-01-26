-module(ppool_worker_sup).

-behaviour(supervisor).

-export([init/1, start_link/1]).

start_link(MFA = {_, _, _}) ->
    supervisor:start_link(?MODULE, MFA).

init({M, F, A}) ->
    %% response to supervisor
    {ok, %% status and reply
     %% supervisor startegy
     {{simple_one_for_one, % strategy
      5, % max restrats
      3600}, % restart time
     %% child specs
     [{ppool_worker, % child name
       {M, F, A}, % child module function and args
       temporary, % child life type
       5000, % shutdown time
       worker, % child type
       [M]}]}}. % no idea

