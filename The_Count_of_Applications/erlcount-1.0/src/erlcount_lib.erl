-module(erlcount_lib).

-export([find_erl/1, regex_count/2]).

-include_lib("kernel/include/file.hrl").

%% Continuation-Passing Style
%% recursive method that can be progressed iteratively
%% for each step it returns a result and a next handler
%% returns done if the job is done
find_erl(Dir) -> find_erl(Dir, queue:new()).

find_erl(Name, Q) ->
    {ok, F = #file_info{}} = file:read_file_info(Name),
    case F#file_info.type of
      directory -> handle_dir(Name, Q);
      regular -> handle_file(Name, Q);
      _Other -> handle_dequeue(Q)
    end.

handle_dir(DirName, Queue) ->
    case file:list_dir(DirName) of
      {ok, []} -> handle_dequeue(Queue);
      {ok, Files} ->
	  NewQueue = enqueue_files(DirName, Files, Queue),
	  handle_dequeue(NewQueue)
    end.

handle_file(File, Queue) ->
    case filename:extension(File) of
      ".erl" ->
	  %% this is the CSP step return
	  {continue, File, fun () -> handle_dequeue(Queue) end};
      _NonErl -> handle_dequeue(Queue)
    end.

enqueue_files(Dir, Files, Queue) ->
    lists:foldl(fun (File, Q) ->
			queue:in(filename:join(Dir, File), Q)
		end,
		Queue, Files).

handle_dequeue(Queue) ->
    case queue:out(Queue) of
      %% CSP done
      {empty, _} -> done;
      {{value, File}, NewQueue} -> find_erl(File, NewQueue)
    end.

regex_count(Re, Str) ->
    case re:run(Str, Re, [global]) of
      nomatch -> 0;
      {match, List} -> length(List)
    end.
