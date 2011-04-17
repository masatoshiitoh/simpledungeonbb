%%
%%      Copyright (C) 2010 by Masatoshi Itoh
%%      http://www.simpledungeon.com/
%%
%%  This Program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation; either version 2, or (at your option)
%%  any later version.
%%
%%  This Program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with simple dungeon; see the file COPYING.  If not, write to
%%  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
%%  http://www.gnu.org/copyleft/gpl.html
%%


-module(simpledungeon).
-behavior(supervisor).

-include("yaws.hrl").
-include("yaws_api.hrl").

%%-----------------------------------

-export([start/0]).
%%-export([start/1]).
%%-export([start/2]).
-export([init/1]).
-export([start_yaws/0]).

start() ->
	%% start supervisor, local registerd (name is ?MODULE),
	%% callback is on ?MODULE
	{ok, Pid} = supervisor:start_link({local,?MODULE},?MODULE,[]),
	unlink(Pid),
	%% start database.
	db:start(reset_tables),
	start_yaws(),
	Pid.

start(_) -> start().
start(_Type, Args) -> start(Args).

init(_Args) ->
    ChildSpec = [path_finder(), battle_observer()],
    {ok, {{one_for_one, 10, 60},ChildSpec}}.

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
		{appmods, [{"/service", yaws_if}]}
	],

	{ok, SCList, GC, ChildSpecs} =
	    yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

%%	[supervisor:start_child({local,?MODULE}, Ch) || Ch <- ChildSpecs], %% this code causes crash.
	[supervisor:start_child(?MODULE, Ch) || Ch <- ChildSpecs],
	%% now configure Yaws
	yaws_api:setconf(GC, SCList).

path_finder() ->
    ID = path_finder,
    StartFunc = {path_finder, start, []},
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [path_finder],
    _ChildSpec = {ID, StartFunc, Restart, Shutdown, Type, Modules}.

battle_observer() ->
    ID = battle_observer,
    StartFunc = {battle_observer, start_link, []},
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [battle_observer],
    _ChildSpec = {ID, StartFunc, Restart, Shutdown, Type, Modules}.

