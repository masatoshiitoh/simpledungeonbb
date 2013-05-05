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


-module(actor_pool_supersup).
-behaviour(supervisor).

-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, actor_pool}, ?MODULE, []).

stop() ->
	case whereis(actor_pool) of
		P when is_pid(P) ->
			exit(P, kill);
		_ -> ok
	end.

init([]) ->
	MaxRestart = 6,
	MaxTime = 3600,
	{ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_pool(Name, Limit, MFA) ->
	ChildSpec = {Name,
		{actor_pool_sup, start_link, [Name, Limit, MFA]},
		permanent, 10000, supervisor, [actor_pool_sup]},
	supervisor:start_child(actor_pool, ChildSpec).

stop_pool(Name) ->
	supervisor:terminate_child(actor_pool, Name),
	supervisor:delete_child(actor_pool, Name).


