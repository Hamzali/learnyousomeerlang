-module(musicians).

-behaviour(gen_server).

-export([start_link/2, stop/1]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-record(state, {name = "", role, skill = good}).

-define(DELAY, 750).

-define(NAME_LIST_SIZE, 10).

start_link(Role, Skill) ->
    gen_server:start_link({local, Role}, ?MODULE,
			  [Role, Skill], []).

stop(Role) -> gen_server:call(Role, stop).

%% GEN SERVER HANDLERS
init([Role, Skill]) ->
    %% to know when the parent dies
    process_flag(trap_exit, true),
    TimeToPlay = rand:uniform(3000),
    Name = pick_name(),
    StrRole = atom_to_list(Role),
    io:format("musician ~s playing ~s entred the room~n",
	      [Name, StrRole]),
    {ok, #state{name = Name, role = StrRole, skill = Skill},
     TimeToPlay}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State, ?DELAY}.

handle_cast(_Msg, State) -> {noreply, State, ?DELAY}.

handle_info(timeout,
	    S = #state{name = N, skill = good}) ->
    io:format("~s produced a sound!~n", [N]),
    {noreply, S, ?DELAY};
handle_info(timeout,
	    S = #state{name = N, skill = bad}) ->
    case rand:uniform(5) of
      1 ->
	  io:format("~s played a bad note!~n", [N]),
	  {stop, bad_note, S};
      _ ->
	  io:format("~s produced a sound!~n", [N]),
	  {noreply, S, ?DELAY}
    end;
handle_info(_Msg, State) -> {noreply, State, ?DELAY}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(normal, _S = #state{name = N, role = R}) ->
    io:format("~s (~s) walk out of the room~n", [N, R]);
terminate(bad_note, _S = #state{name = N, role = R}) ->
    io:format("~s (~s) played a bad note and thrown "
	      "outside~n",
	      [N, R]);
terminate(shutdown, S) ->
    io:format("The manager is mad and fired the whole "
	      "band! ~s just got back to playing in "
	      "the subway~n",
	      [S#state.name]);
terminate(_Reason, S) ->
    io:format("~s has been kicked out (~s)~n",
	      [S#state.name, S#state.role]).

%% PRIVATE METHODS

pick_name() ->
    lists:nth(rand:uniform(?NAME_LIST_SIZE), firstnames())
      ++
      " " ++
	lists:nth(rand:uniform(?NAME_LIST_SIZE), lastnames()).

firstnames() ->
    ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
     "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"].

lastnames() ->
    ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
     "Terese", "Tennelli", "Jamal", "Li", "Perlstein"].
