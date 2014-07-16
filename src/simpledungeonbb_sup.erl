-module(simpledungeonbb_sup).

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
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

	%% start mnesia db.
	db:change_schema(),
	db:start(reset_tables),

	%% start yaws web server.
	con_yaws:start_yaws(?MODULE),
	
	{ok, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ChildSpec = [notice_mgr(), path_finder(), battle_mgr()],
    {ok, {{one_for_one, 10, 60},ChildSpec}}.

%% ===================================================================
%% process definition
%% ===================================================================

notice_mgr() ->
    ID = notice_mgr,
    StartFunc = {notice_mgr, start_link, []},
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [notice_mgr],
    _ChildSpec = {ID, StartFunc, Restart, Shutdown, Type, Modules}.

path_finder() ->
    ID = path_finder,
    StartFunc = {path_finder, start_link, []},
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [path_finder],
    _ChildSpec = {ID, StartFunc, Restart, Shutdown, Type, Modules}.

battle_mgr() ->
    ID = battle_mgr,
    StartFunc = {battle_mgr, start_link, []},
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [battle_mgr],
    _ChildSpec = {ID, StartFunc, Restart, Shutdown, Type, Modules}.


