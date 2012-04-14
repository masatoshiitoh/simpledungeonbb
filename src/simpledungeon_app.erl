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
    Result = simpledungeon_sup:start_link(),
	unlink( spawn_link(db, start, [reset_tables])),
%%	unlink( spawn_link(ybed_sup, start_link, [])),
	Result.

stop(_State) ->
    ok.
