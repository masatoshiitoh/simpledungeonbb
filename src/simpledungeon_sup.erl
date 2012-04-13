
-module(simpledungeon_sup).
 
-behaviour(supervisor).

-include("yaws.hrl").
-include("yaws_api.hrl").



%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SimpleDungeon = ?CHILD(simpledungeon, worker),
    CharPoolSup = ?CHILD(charpool_sup, supervisor),
    IdPassword = ?CHILD(id_password, worker),
    BattleObserver = ?CHILD(battle_observer, worker),
    Ybed = ?CHILD(ybed_sup, supervisor),
%%    Mbed = ?CHILD(mbed, worker),
    {ok, { {one_for_one, 5, 10}, [SimpleDungeon, CharPoolSup, IdPassword, BattleObserver, Ybed
	%%, Mbed
	]} }.

