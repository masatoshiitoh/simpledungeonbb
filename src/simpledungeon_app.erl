-module(simpledungeon_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	start(undefined, undefined).

start(_StartType, _StartArgs) ->
	unlink( spawn_link(db, start, [reset_tables])),
	unlink( spawn_link(ybed, run, [])),
    simpledungeon_sup:start_link().

stop(_State) ->
    ok.
