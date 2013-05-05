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

-module(actor_sup).
-export([start_link/1, init/1]).
-behaviour(supervisor).

start_link(MFA={_,_,_}) ->
	supervisor:start_link(?MODULE, MFA).

init({M,F,A}) ->
	MaxRestart = 5,
	MaxTime = 3600,
	{ok, {{simple_one_for_one, MaxRestart, MaxTime},
		[{actor,
			{M,F,A},
			temporary, 5000, worker, [M]}]}}.


