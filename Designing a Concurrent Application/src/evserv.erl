-module(evserv).
-compile(export_all).

-record(state, {events, clients}).

-record(event, {name="", description="", pid, timeout={{1970, 1, 1},{0,0,0}}}).


start() -> register(?MODULE, Pid=spawn(?MODULE, init, [])), Pid.

start_link() -> register(?MODULE, Pid=spawn_link(?MODULE, init, [])), Pid.

terminate() -> ?MODULE ! shutdown.

%% utility methods
valid_datetime({Date, Time}) ->
	try
		calendar:valid_date(Date) andalso valid_time(Time)
	catch
		error:function_clause -> false
	end;
valid_datetime(_) -> false.

valid_time({H, M, S}) -> valid_time(H, M, S).
valid_time(H, M, S) when H >= 0, H < 24, M >= 0, M < 60, S >= 0, S < 60-> true;
valid_time(_, _, _) -> false.

%% server handlers
handle_add(S=#state{events=Events}, {Pid, MsgRef, {add, Name, Description, TimeOut}}) ->
	case valid_datetime(TimeOut) of
		true -> EventPid = event:start_link(Name, TimeOut),
			NewEvents = orddict:store(Name, #event{name=Name, description=Description, pid=EventPid, timeout=TimeOut}, Events),
		Pid ! {MsgRef, ok},
		loop(S#state{events=NewEvents});
		false -> 
		Pid ! {MsgRef, {error, bad_timeout}}, loop(S)
	end. 

handle_cancel(S=#state{events=Events}, {Pid, MsgRef, {cancel, Name}}) ->
	NewEvents = case orddict:find(Name, Events) of
		{ok, E} -> event:cancel(E#event.pid),
			orddict:erase(Name, Events);
		error -> Events
	end,
	Pid ! {MsgRef, ok},
	loop(S#state{events=NewEvents}).

send_to_clients(Msg, ClientDict) ->
	orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

handle_done(S=#state{events=Events, clients=Clients}, {done, Name}) ->
	NewEvents = case orddict:find(Name, Events) of
		{ok, E} ->
			send_to_clients({done, Name, E#event.description}, Clients),
			orddict:erase(Name, Events);
		error -> Events
	end,
	loop(S#state{events=NewEvents}).

handle_subscribe(S=#state{clients=Clients}, {Pid, MsgRef, {subscribe, Client}}) ->
 	% monitor the status of connected client
	% because don't want to send messages to dead client 
	Ref = erlang:monitor(process, Client),
	NewClients = orddict:store(Ref, Client, Clients),
	Pid ! {MsgRef, ok},
	loop(S#state{clients=NewClients}).
	
loop(S = #state{clients=Clients}) ->
	receive
		M = {_Pid, _MsgRef, {subscribe, _Client}} -> handle_subscribe(S, M);
		M = {_Pid, _MsgRef, {add, _Name, _Description, _TimeOut}} -> handle_add(S, M);
		M = {_Pid, _MsgRef, {cancel, _Name}} -> handle_cancel(S, M);
		M = {done, _Name} -> handle_done(S, M);
		shutdown -> exit(shutdown);
		{'DOWN', Ref, process, _Pid, _Reason} -> 
			loop(S#state{clients=orddict:erase(Ref, Clients)});
		code_change -> ?MODULE:loop(S);
			Unknown -> io:format("Unknown message: ~p~n", [Unknown]),
			loop(S)
	end.

%% initialize the main loop and state
init() -> loop(#state{events=orddict:new(), clients=orddict:new()}).
 
subscribe(Pid) ->
	Ref = erlang:monitor(process, whereis(?MODULE)),
	?MODULE ! {self(), Ref, {subscribe, Pid}},
	receive {Ref, ok} -> {ok, Ref};
	{'DOWN', Ref, process, _Pid, Reason} -> {error, Reason}
	after 5000 -> {error, timeout} end.

add_event(Name, Description, TimeOut) ->
	Ref = make_ref(),
	?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
	receive {Ref, Msg} -> Msg
	after 5000 -> {error, timeout} end.

cancel(Name) -> 
	Ref = make_ref(),
	?MODULE ! {self(), Ref, {cancel, Name}},
	receive {Ref, ok} -> ok
	after 5000 -> {error, timeout} end.

listen(Delay) ->
	receive
		M = {done, _Name, _Description} -> [M | listen(0)]
	after Delay * 1000 -> [] end.

