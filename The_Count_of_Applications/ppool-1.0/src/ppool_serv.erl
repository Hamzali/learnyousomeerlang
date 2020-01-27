-module(ppool_serv).
%% TODO: add queue limit as well
-behaviour(gen_server).

-export([async_queue/2, run/2, start/4, start_link/4,
	 stop/1, sync_queue/2]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

%% Extarnal Interface
start(Name, Limit, Sup, MFA)
    when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE,
		     {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA)
    when is_atom(Name), is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE,
			  {Limit, MFA, Sup}, []).

run(Name, Args) -> gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).

stop(Name) -> gen_server:call(Name, stop).

%% Handlers

%% The friendly supervisor is started dynamically!
-define(SPEC(MFA),
	{worker_sup, {ppool_worker_sup, start_link, [MFA]},
	 temporary, 10000, supervisor, [ppool_worker_sup]}).

-record(state,
	{limit = 0, sup, refs, queue = queue:new()}).

init({Limit, MFA, Sup}) ->
    %% this is wrog deadlock occurs with the supervisor and worker
    % {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    % link(Pid),
    %% instead send a message to itself
    %% handle it on handle_info
    self() ! {start_worker_supervisor, Sup, ?SPEC(MFA)},
    {ok, #state{limit = Limit, refs = gb_sets:empty()}}.

terminate(normal, _State) -> ok.

%% run
handle_call({run, Args}, _From,
	    State = #state{limit = N, refs = R, sup = Sup})
    when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid},
     State#state{limit = N - 1, refs = gb_sets:add(Ref, R)}};
handle_call({run, _Args}, _From,
	    State = #state{limit = N})
    when N =< 0 ->
    {reply, noalloc, State};
%% sync
handle_call({sync, Args}, _From,
	    State = #state{limit = N, refs = R, sup = Sup})
    when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid},
     State#state{limit = N - 1, refs = gb_sets:add(Ref, R)}};
handle_call({sync, Args}, From,
	    State = #state{queue = Q}) ->
    {noreply,
     State#state{queue = queue:in({From, Args}, Q)}};
handle_call(stop, _From, State) ->
    {stop, ok, normal, State};
%% other
handle_call(Msg, _From, State) ->
    io:format("Unexpected call message ~p~n", [Msg]),
    {noreply, State}.

handle_cast({async, Args},
	    State = #state{limit = N, sup = Sup, refs = R})
    when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {noreply,
     State#state{limit = N - 1, refs = gb_sets:add(Ref, R)}};
handle_cast({async, Args},
	    State = #state{limit = N, queue = Q})
    when N =< 0 ->
    {noreply, State#state{queue = queue:in(Args, Q)}};
handle_cast(Msg, State) ->
    io:format("Unexpected cast message ~p~n", [Msg]),
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _},
	    State = #state{refs = Refs}) ->
    case gb_sets:is_element(Ref, Refs) of
      true -> handle_down_worker(Ref, State);
      false -> {noreply, State}
    end;
handle_info({start_worker_supervisor, Sup, ChildSpec},
	    State) ->
    {ok, Pid} = supervisor:start_child(Sup, ChildSpec),
    link(Pid),
    {noreply, State#state{sup = Pid}};
handle_info(Msg, State) ->
    io:format("Unexpected message ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% private methods
handle_down_worker(Ref,
		   State = #state{limit = L, sup = Sup,
				  refs = R}) ->
    case queue:out(State#state.queue) of
      {{value, {From, Args}}, Q} ->
	  {ok, Pid} = supervisor:start_child(Sup, Args),
	  NewRef = erlang:monitor(process, Pid),
	  NewRefs = gb_sets:insert(NewRef,
				   gb_sets:delete(Ref, R)),
	  gen_server:reply(From, {ok, Pid}),
	  {noreply, State#state{queue = Q, refs = NewRefs}};
      {{value, Args}, Q} ->
	  {ok, Pid} = supervisor:start_child(Sup, Args),
	  NewRef = erlang:monitor(process, Pid),
	  NewRefs = gb_sets:insert(NewRef,
				   gb_sets:delete(Ref, R)),
	  {noreply, State#state{queue = Q, refs = NewRefs}};
      {empty, _} ->
	  {noreply,
	   State#state{limit = L + 1,
		       refs = gb_sets:delete(Ref, R)}}
    end.
