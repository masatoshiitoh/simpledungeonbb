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
-export([init/1]).

start() ->
	%% start supervisor, local registerd (name is ?MODULE),
	%% callback is on ?MODULE
	{ok, Pid} = supervisor:start_link({local,?MODULE},?MODULE,[]),
	unlink(Pid),
	%% start database.
	db:change_schema(),
	db:start(reset_tables),
	%% start yaws web server.
	con_yaws:start_yaws(?MODULE),
	Pid.

%%start(_) -> start().
%%start(_Type, Args) -> start(Args).

init(_Args) ->
    ChildSpec = [notice_mgr(), path_finder(), battle_mgr()],
    {ok, {{one_for_one, 10, 60},ChildSpec}}.

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
    StartFunc = {path_finder, start, []},
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

