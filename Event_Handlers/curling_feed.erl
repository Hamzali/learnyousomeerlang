-module(curling_feed).

-behaviour(gen_event).

-export([code_change/3, handle_call/2, handle_event/2,
	 handle_info/2, init/1, terminate/2]).

init([Pid]) -> {ok, Pid}.

handle_event(Event, Pid) ->
    Pid ! {curling_feed, Event}, {ok, Pid}.

handle_call(_, State) -> {ok, ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

% example
% 1> c(curling), c(curling_feed).
% {ok,curling_feed}
% 2> {ok, Pid} = curling:start_link("Saskatchewan Roughriders", "Ottawa Roughriders").
% Scoreboard: Team Saskatchewan Roughriders vs. Team Ottawa Roughriders
% {ok,<0.165.0>}
% 3> HandlerId = curling:join_feed(Pid, self()).
% {curling_feed,#Ref<0.0.0.909>}
% 4> curling:add_points(Pid, "Saskatchewan Roughriders", 2).
% Scoreboard: increased score of team Saskatchewan Roughriders by 1
% ok
% Scoreboard: increased score of team Saskatchewan Roughriders by 1
% 5> flush().
% Shell got {curling_feed,{add_points,"Saskatchewan Roughriders",2}}
% ok
% 6> curling:leave_feed(Pid, HandlerId).
% ok
% 7> curling:next_round(Pid).
% Scoreboard: round over
% ok
% 8> flush().
% ok

