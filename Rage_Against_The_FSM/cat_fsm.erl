-module(cat_fsm).

-export([send_event/1, start/0]).

%% sync events
start() -> spawn(fun () -> dont_give_a_crap() end).

send_event(Pid) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, some_event},
    receive
      {Ref, Msg} -> {ok, Msg} after 5000 -> {error, timeout}
    end.

dont_give_a_crap() ->
    receive
      {Pid, Ref, _Msg} -> Pid ! {Ref, "does not give crap!"};
      _ -> ok
    end,
    io:format("Still not giving a crap"),
    dont_give_a_crap().
