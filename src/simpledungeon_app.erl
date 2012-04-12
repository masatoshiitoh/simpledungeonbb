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
	spawn(fun() -> db:start(reset_tables) end),
    simpledungeon_sup:start_link().

stop(_State) ->
    ok.
