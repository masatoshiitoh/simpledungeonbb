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

-module(actor_pool).
-export([start_link/0, stop/0, start_pool/3, run/2, stop_pool/1]).

-export([test_it/0, test_it_2/0]).

test_it() ->
	actor_pool:start_link(),
	actor_pool:start_pool(character, 2, {character, start_link, []}).

test_it_2() ->
	actor_pool:run(character, ["cid0001"]).



start_link() ->
	actor_pool_supersup:start_link().

stop() ->
	actor_pool_supersup:stop().

start_pool(Name, Limit, {M,F,A}) ->
	actor_pool_supersup:start_pool(Name, Limit, {M,F,A}).

stop_pool(Name) ->
	actor_pool_supersup:stop_pool(Name).

run(Name, Args) ->
	actor_pool_mgr:run(Name, Args).



