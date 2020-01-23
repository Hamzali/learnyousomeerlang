-module(kitty_gen_server).

-behaviour(gen_server).

%% API
-export([close_shop/1, order_cat/4, return_cat/2,
	 start_link/0]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

start_link() -> gen_server:start_link(?MODULE, [], []).

-record(cat, {name, color = green, description}).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    gen_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    gen_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) -> gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From,
	    Cats) ->
    if Cats =:= [] ->
	   {reply, make_cat(Name, Color, Description), Cats};
       Cats =/= [] -> % got to empty the stock
	   {reply, hd(Cats), tl(Cats)}
    end;
handle_call(terminate, _From, Cats) ->
    {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
    {noreply, [Cat | Cats]}.

%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name = Name, color = Col, description = Desc}.

terminate(normal, Cats) ->
    [io:format("~p was set free.~n", [C#cat.name])
     || C <- Cats],
    exit(normal).

%% No change planned. The function is there for the behaviour,
%% but will not be used. Only a version on the next
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(Msg, Cats) ->
    io:format("Unexpected message ~p~n", [Msg]),
    {noreply, Cats}.
