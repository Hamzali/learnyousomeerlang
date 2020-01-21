-module(my_server).

-export([call/2, cast/2, reply/2, start/2,
	 start_link/2]).

%% public api
start_link(Module, InitialState) ->
    spawn_link(fun () -> init(Module, InitialState) end).

start(Module, InitialState) ->
    spawn(fun () -> init(Module, InitialState) end).

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg},
    receive
      {Ref, Reply} -> erlang:demonitor(Ref, [flush]), Reply;
      {'DOWN', Ref, process, Pid, Reason} ->
	  erlang:error(Reason)
      after 5000 -> erlang:error(timeout)
    end.

cast(Pid, Msg) -> Pid ! Msg, ok.

reply({Pid, Ref}, Reply) -> Pid ! {Ref, Reply}.

%% private api
init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
      {sync, Pid, Ref, Msg} ->
	  loop(Module,
	       Module:handle_call(Msg, {Pid, Ref}, State));
      {async, Msg} ->
	  loop(Module, Module:handle_cast(Msg, State))
    end.
