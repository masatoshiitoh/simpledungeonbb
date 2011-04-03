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


-module(move).
-compile(export_all).

%-import(lists, [foreach/2]).

-include_lib("mmoasp.hrl").
%%-include_lib("stdlib/include/qlc.hrl").


mapmove_call({_From, init_move, CurrPos, WayPoints}, R, I) ->
	SelfPid = self(),
	SelfPid ! {timer, {_From, cancel_timer}},
	SelfPid ! {mapmove, {_From, move, CurrPos, WayPoints}},
	{R, task:mk_idle_reset()};

mapmove_call({_From, move, CurrPos, WayPoints}, R, I) ->
	character:db_setpos(R#task_env.cid, CurrPos),
	case WayPoints of
		[] -> 
			io:format("mapmove_call:~p arrived at: ~p~n", [R#task_env.cid, CurrPos]),
			{pos, X, Y} = CurrPos,
			{R, task:mk_idle_update(I)};

		[H | T] -> 			
			io:format("mapmove_call:~p start move: ~p to ~p ~n", [R#task_env.cid, CurrPos, H]),
			{pos, X, Y} = CurrPos,
			mmoasp:notice_move(R#task_env.cid, {transition, CurrPos, H, 1000}, Radius = 100),
			SelfPid = self(),
			F = fun() ->
				SelfPid ! {mapmove, {SelfPid, move, H, T}}
			end,
			{R#task_env{utimer = morningcall:add(1000, F, R#task_env.utimer)},task:mk_idle_update(I)}
	end;

mapmove_call({_From, notice_move, SenderCid, From, To, Duration}, R, I) ->
	{pos, FromX, FromY} = From,
	{pos, ToX, ToY} = To,
	{task:add_event(R,
			[{type, "move"}, {cid, SenderCid},
				{from_x, FromX}, {from_y, FromY},
				{to_x, ToX}, {to_y, ToY},
				{duration, Duration}]),
		task:mk_idle_update(I)}.

