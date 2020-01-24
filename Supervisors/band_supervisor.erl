%% Static Supervison
-module(band_supervisor).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Type) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Type).

%% The band supervisor will allow its band members to make a few
%% mistakes before shutting down all operations, based on what
%% mood he's in. A lenient supervisor will tolerate more mistakes
%% than an angry supervisor, who'll tolerate more than a
%% complete jerk supervisor
init(lenient) -> init({one_for_one, 3, 60});
init(angry) -> init({rest_for_one, 2, 60});
init(jerk) -> init({one_for_all, 1, 60});
%% simple one for one holds childs as a dictionary
%% easy to access however no dynamic action is allowed terminate, restart, add etc.
init(jamband) ->
    {ok,
     {{simple_one_for_one, 3, 60},
      [{jam_musician, {musicians, start_link, []}, temporary,
	1000, worker, [musicians]}]}};
%% others holds children process as a list
%% allows dynamic terminate, restart, add etc.
init({Strategy, MaxRestart, MaxTime}) ->
    Singer = {singer,
	      {musicians, start_link, [singer, good]}, permanent,
	      1000, worker, [musicians]},
    Bass = {bass, {musicians, start_link, [bass, good]},
	    temporary, 1000, worker, [musicians]},
    Drum = {drum, {musicians, start_link, [drum, bad]},
	    transient, 1000, worker, [musicians]},
    Keytar = {keytar,
	      {musicians, start_link, [keytar, good]}, transient,
	      1000, worker, [musicians]},
    ChildSpecs = [Singer, Bass, Drum, Keytar],
    {ok, {{Strategy, MaxRestart, MaxTime}, ChildSpecs}}.

%% dynamic supervisor apis.
% 1> band_supervisor:start_link(lenient).
% {ok,0.709.0>}
% 2> supervisor:which_children(band_supervisor).
% [{keytar,<0.713.0>,worker,[musicians]},
% {drum,<0.715.0>,worker,[musicians]},
% {bass,<0.711.0>,worker,[musicians]},
% {singer,<0.710.0>,worker,[musicians]}]
% 3> supervisor:terminate_child(band_supervisor, drum).
% ok
% 4> supervisor:terminate_child(band_supervisor, singer).
% ok
% 5> supervisor:restart_child(band_supervisor, singer).
% {ok,<0.730.0>}
% 6> supervisor:count_children(band_supervisor).
% [{specs,4},{active,3},{supervisors,0},{workers,4}]
% 7> supervisor:delete_child(band_supervisor, drum).
% ok
% 8> supervisor:restart_child(band_supervisor, drum).
% {error,not_found}
% 9> supervisor:count_children(band_supervisor).
% [{specs,3},{active,3},{supervisors,0},{workers,3}]

%% simple one for one example
% 1> supervisor:start_child(band_supervisor, [djembe, good]).
% Musician Janet Tennelli, playing the djembe entered the room
% {ok,<0.690.0>}
% 2> supervisor:start_child(band_supervisor, [djembe, good]).
% {error,{already_started,<0.690.0>}}

