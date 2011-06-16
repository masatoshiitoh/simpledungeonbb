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

%%
%% 'map based move' main api
%%
move(Cid, DestPos) ->
	F = fun(X) ->
		NowPos = {pos, X#session.x, X#session.y},
		{ok, Result} = path_finder:lookup_path(NowPos, DestPos),
		Result
	end,
	{atomic, WayPoints} = world:apply_session(Cid, F),
	case WayPoints of
		[] -> io:format("path unavailable.  no waypoints found.~n", []),
			[];
		[Start| Path] ->
			FS = fun(X) ->
				init_move(X#session.pid, Start, Path)
			end,
			world:apply_session(Cid, FS)
	end.

init_move(Pid, CurrentPos, Path) -> Pid ! {mapmove, {self(), init_move, CurrentPos, Path}}.


set_new_route({_From, init_move, CurrPos, WayPoints}, R, _I) ->
	io:format("set_new_route:OldCurr = ~p, NewWP = ~p~n", [CurrPos, R#task_env.waypoints]),
	%% write currpos and waypoint into R record.
	NewR = R#task_env{waypoints = WayPoints, currpos = CurrPos},
	{NewR, task:mk_idle_reset()}.

set_new_route_and_start_timer({_From, init_move, CurrPos, WayPoints}, R, _I) ->
	SelfPid = self(),
	SelfPid ! {timer, {_From, cancel_timer}},
	SelfPid ! {mapmove, {_From, move}},
	set_new_route({_From, init_move, CurrPos, WayPoints}, R, _I).	

%%
%% initialize move. send move message to itself.
%%

mapmove_call({_From, init_move, CurrPos, WayPoints}, R, _I) when R#task_env.waypoints == [] andalso R#task_env.currpos == undefined ->
	set_new_route_and_start_timer({_From, init_move, CurrPos, WayPoints}, R, _I);

mapmove_call({_From, init_move, CurrPos, WayPoints}, R, _I) ->
	set_new_route({_From, init_move, CurrPos, WayPoints}, R, _I);


%%
%% position updater.
%% proc 'move' message:
%% update currpos, and set new timer to send new move message.
%%
mapmove_call({_From, move}, R, I) ->

	CurrPos = R#task_env.currpos,
	WayPoints = R#task_env.waypoints,

	character:db_setpos(R#task_env.cid, CurrPos),
	case WayPoints of
		[] ->
			io:format("mapmove_call:~p arrived at: ~p~n", [R#task_env.cid, CurrPos]),
			{pos, _X, _Y} = CurrPos,
			NewR = R#task_env{waypoints = [], currpos = undefined},
			{NewR, task:mk_idle_update(I)};

		[H | T] ->
			io:format("mapmove_call:~p start move: ~p to ~p ~n", [R#task_env.cid, CurrPos, H]),
			{pos, _X, _Y} = CurrPos,
			mmoasp:notice_move(R#task_env.cid, {transition, CurrPos, H, 1000}, _Radius = 100),
			SelfPid = self(),
			F = fun() ->
				SelfPid ! {mapmove, {SelfPid, move}}
			end,

			%% update currpos and waypoints with H and T.
			%% these currpos and waypoints are valid in future(at wakeup call).
			NewR = R#task_env{currpos = H, waypoints = T, utimer = morningcall:add(1000, F, R#task_env.utimer)},
			
			{NewR,task:mk_idle_update(I)}
	end;

%%
%% receive others move.
%%
mapmove_call({_From, notice_move, SenderCid, From, To, Duration}, R, I) ->
	{pos, FromX, FromY} = From,
	{pos, ToX, ToY} = To,
	{task:add_event(R,
			[{type, "move"}, {cid, SenderCid},
				{from_x, FromX}, {from_y, FromY},
				{to_x, ToX}, {to_y, ToY},
				{duration, Duration}]),
		task:mk_idle_update(I)}.

