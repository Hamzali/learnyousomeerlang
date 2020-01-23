-module(dog_fsm).

-export([pet/1, squirrel/1, start/0]).
%% async events
pet(Pid) -> Pid ! pet.

squirrel(Pid) -> Pid ! squirrel.

start() -> spawn(fun barks/0).

barks() ->
    io:format("Dog says: BARK! BARK!~n"),
    receive
      pet -> wag_tails();
      Msg -> confused(Msg, wag_tails)
      after 5000 -> barks()
    end.

wag_tails() ->
    io:format("Dog wags its tail~n"),
    receive
      pet -> sits();
      Msg -> confused(Msg, wag_tails)
      after 5000 -> barks()
    end.

sits() ->
    io:format("Dog is sitting. Gooooood boy!~n"),
    receive
      squirrel -> barks();
      Msg -> confused(Msg, wag_tails)
    end.

confused(State, Message) ->
    io:format("Dog is confused ~p~n", [Message]), State().
