
-module(simpledungeon_sup).
 
-behaviour(supervisor).

-include("yaws.hrl").
-include("yaws_api.hrl").



%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([start_yaws/0]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	db:start(reset_tables),
	start_yaws(),

    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SimpleDungeon = ?CHILD(simpledungeon, worker),
    CharPoolSup = ?CHILD(charpool_sup, supervisor),
    IdPassword = ?CHILD(id_password, worker),
    BattleObserver = ?CHILD(battle_observer, worker),
    {ok, { {one_for_one, 5, 10}, [SimpleDungeon, CharPoolSup, IdPassword, BattleObserver]} }.

start_yaws() ->
	Id = "simpledungeon",

	GconfList = [
		{logdir, "./test/log"},
		{ebin_dir, [".","../ebin"]},
		{id, Id}],

	Docroot = "../docroot",

	SconfList = [
		{port, 8002},
		{listen, {0,0,0,0}},
		{docroot, Docroot},
		{appmods, [{"/service", mmoasp}]}
	],

	{ok, SCList, GC, ChildSpecs} =
	    yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

	[supervisor:start_child(?MODULE, Ch) || Ch <- ChildSpecs],

	%% now configure Yaws
	yaws_api:setconf(GC, SCList).

