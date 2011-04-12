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

%%-----------------------------------

-export([start/1, start/0]).
-export([init/1]).

start_yaws() ->
	Gconf = yaws_config:make_default_gconf(false, 'simpledungeon'),
	Sconf = yaws_config:make_default_sconf(),
	yaws:start(),
	yaws_api:setconf(Gconf, [Sconf]).
	
start(_) -> start().

start() ->
%%	start_yaws(),
	db:start(reset_tables),
    supervisor:start_link({local,?MODULE},?MODULE,[]).

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

init(_Args) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 60,
    ChildSpec = [path_finder(), battle_observer()],
    {ok, {{RestartStrategy, MaxR, MaxT},ChildSpec}}.


