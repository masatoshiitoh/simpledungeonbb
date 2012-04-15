-module(simpledungeon_app).

-behaviour(application).

%% Application kicker
-export([start_all/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start_all() ->
	application:start(simpledungeon).

start(_StartType, _StartArgs) ->
    Result = simpledungeon_sup:start_link(),
	unlink( spawn_link(db, start, [reset_tables])),
	Result.

stop(_State) ->
    ok.
