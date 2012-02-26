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
move({map_id, SvId, MapId}, Cid, DestPos) when is_record(DestPos, location) ->
	F = fun(X) ->
		NowPos = X#online_character.location,
		{ok, Result} = path_finder:lookup_path({map_id, SvId, MapId}, NowPos, DestPos),
		Result
	end,
	WayPoints = online_character:apply_online_character(Cid, F),
	case WayPoints of
		[] -> io:format("path unavailable.  no waypoints found.~n", []),
			[];
		[Start| Path] ->
			FS = fun(X) ->
				init_move(X#online_character.pid, Start, Path)
			end,
			online_character:apply_online_character(Cid, FS)
	end.


obsolete_move(Cid, DestPos) when is_record(DestPos, location) ->
	F = fun(X) ->
		NowPos = X#online_character.location,
		{ok, Result} = path_finder:lookup_path({map_id, "hibari", 1}, NowPos, DestPos),
		Result
	end,
	WayPoints = online_character:apply_online_character(Cid, F),
	case WayPoints of
		[] -> io:format("path unavailable.  no waypoints found.~n", []),
			[];
		[Start| Path] ->
			FS = fun(X) ->
				init_move(X#online_character.pid, Start, Path)
			end,
			online_character:apply_online_character(Cid, FS)
	end.

init_move(Pid, CurrentPos, Path) -> Pid ! {mapmove, {self(), init_move, CurrentPos, Path}}.

set_new_route({_From, init_move, CurrPos, WayPoints}, R, _I) when R#task_env.currpos == undefined ->
	%% write currpos and waypoint into R record.
	NewR = R#task_env{waypoints = WayPoints, currpos = CurrPos},
	{NewR, task:mk_idle_reset()};

set_new_route({_From, init_move, _CurrPos, WayPoints}, R, _I) ->
	io:format("set_new_route: overwrite waypoints.~n", []),
	%% write currpos and waypoint into R record.
	NewR = R#task_env{waypoints = WayPoints},
	{NewR, task:mk_idle_reset()}.

set_new_route_and_start_timer({_From, init_move, CurrPos, WayPoints}, R, _I) ->
	SelfPid = self(),
	SelfPid ! {timer, {_From, cancel_timer}},
	SelfPid ! {mapmove, {_From, move}},
	set_new_route({_From, init_move, CurrPos, WayPoints}, R, _I).	

%%
%% initialize move. send move message to itself.
%%

mapmove_call({_From, init_move, CurrPos, WayPoints}, R, _I)
	when R#task_env.waypoints == []
		andalso R#task_env.currpos == undefined ->
	notice_move_list_to_neighbor(R#task_env.cid, [CurrPos | WayPoints]),
	set_new_route_and_start_timer({_From, init_move, CurrPos, WayPoints}, R, _I);


mapmove_call({_From, init_move, CurrPos, WayPoints}, R, _I) ->
	notice_move_list_to_neighbor(R#task_env.cid, [CurrPos | WayPoints]),
	set_new_route({_From, init_move, CurrPos, WayPoints}, R, _I);


%%
%% position updater.
%% proc 'move' message:
%% update currpos, and set new timer to send new move message.
%%
mapmove_call({_From, move}, R, I) ->

	CurrPos = R#task_env.currpos,
	WayPoints = R#task_env.waypoints,

	mmoasp:setpos(R#task_env.cid, CurrPos),
	case WayPoints of
		[] ->
			io:format("mapmove_call:~p arrived at: ~p~n", [R#task_env.cid, CurrPos]),
			{pos, _X, _Y} = CurrPos,
			NewR = R#task_env{waypoints = [], currpos = undefined},
			{NewR, task:mk_idle_update(I)};

		[H | T] ->
			io:format("mapmove_call:~p start move: ~p to ~p ~n", [R#task_env.cid, CurrPos, H]),
			{pos, _X, _Y} = CurrPos,
			
			Distance = mmoasp:distance(CurrPos, H),
			Duration = duration_millisec(Distance),
			mmoasp:notice_move(R#task_env.cid, {transition, CurrPos, H, Duration}, mmoasp:default_distance()),
			SelfPid = self(),
			F = fun() ->
				SelfPid ! {mapmove, {SelfPid, move}}
			end,

			%% update currpos and waypoints with H and T.
			%% these currpos and waypoints are valid in future(at wakeup call).
			NewR = R#task_env{currpos = H, waypoints = T, utimer = morningcall:add(Duration, F, R#task_env.utimer)},
			
			{NewR,task:mk_idle_update(I)}
	end;

%%
%% receive others move.
%%

mapmove_call({_From, notice_move_list, SenderCid, MoveList}, R, I) ->
	%%io:format("mapmove_call:~p got ~p 's move list: ~p ~n", [R#task_env.cid, SenderCid, MoveList]),
	io:format("mapmove_call:~p store move list info: ~p ~n", [R#task_env.cid, json:encode(make_move_list_info(SenderCid, MoveList))]),
	NewMovePathDict = dict:store(SenderCid, make_move_list_info(SenderCid, MoveList), R#task_env.move_path_dict),
	NewR = R#task_env{move_path_dict = NewMovePathDict},
	{NewR, task:mk_idle_update(I)};

mapmove_call({_From, notice_move, SenderCid, From, To, Duration}, R, I) ->
	{pos, FromX, FromY} = From,
	{pos, ToX, ToY} = To,
	{task:add_event(R, make_move_info(SenderCid, From, To)),
		task:mk_idle_update(I)}.

make_move_info(SenderCid, From, To) ->
	{pos, FromX, FromY} = From,
	{pos, ToX, ToY} = To,
	Distance = mmoasp:distance(From, To),
	[{type, "move"}, {cid, SenderCid},
				{from_x, FromX}, {from_y, FromY},
				{to_x, ToX}, {to_y, ToY},
				{duration, duration_millisec(Distance)}].

make_move_list_info(SenderCid, []) ->
	{struct,[{type, "move_list"}, {cid, SenderCid}, {move_path, {array,[]}}]};

make_move_list_info(SenderCid, L) ->
	{struct,[{type, "move_list"}, {cid, SenderCid},
				{move_path, {array, [{struct, X}|| X <- L]}}]}.

duration_millisec(Distance) -> erlang:trunc(Distance * 1000).

make_move_list(SenderCid, _CurrPos, L) ->
	make_move_list([], SenderCid, _CurrPos, L).

make_move_list(Acc, SenderCid, _CurrPos, []) ->
	lists:reverse(Acc);

make_move_list(Acc, SenderCid, CurrPos, [H|T] ) ->
	make_move_list([make_move_info(SenderCid, CurrPos, H) | Acc], SenderCid, H, T).

notice_move_list_to_neighbor(SenderCid, RawWaypoints) ->
	[Start|L] = RawWaypoints,
	MoveList = make_move_list(SenderCid, Start, L),
	mmoasp:notice_move_list(SenderCid, {transition_list, MoveList}, mmoasp:default_distance()).
