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


-module(task).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-compile(export_all).

-include_lib("mmoasp.hrl").

%% for task utilities for PC/NPC

system_call({From, stop_process}, R, I) ->
	io:format("~p: proc stop by stop_process message.~n", [R#task_env.cid]),
	morningcall:cancel_all(R#task_env.utimer),
	From ! {ok, R#task_env.cid},
	{undefined, undefined}.

test_call({From, whoareyou}, R, I) ->
	From ! {iam, R#task_env.cid},
	{R, character:mk_idle_update(I)}.

%% TIMER
timer_call({goodmorning, Id}, R, I) ->
	{_FunResult, NewUTimer} = morningcall:dispatch(Id, R#task_env.utimer),
	{R#task_env{utimer = NewUTimer}, mk_idle_update(I)};

timer_call({_From, cancel_timer}, R, I) ->
	NewUTimer = morningcall:cancel_all(R#task_env.utimer),
	{R#task_env{utimer = NewUTimer}, mk_idle_update(I)}.

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

sensor_call({From, request_list_to_know}, R, I) ->
			From ! {list_to_know,
				task:get_elements(R#task_env.event_queue),
				get_stats(R#task_env.stat_dict)},
			{R#task_env{event_queue = queue:new()}, task:mk_idle_reset()};

sensor_call({_From, notice_login, SenderCid, Name}, R, I) ->
			%%io:format("character: get others login. ~p~n",
			%%	[{notice_login, Name} ]),
			{task:add_event(R,
					[{type, "login"},
						{cid, SenderCid},
						{name, Name}]),
				task:mk_idle_update(I)};
			
sensor_call({_From, notice_logout, SenderCid}, R, I) ->
			%%io:format("character: get others logout. ~p~n",
			%%	[{notice_logout, Cid} ]),
			{task:add_event(R,
					[{type, "logout"}, {cid, SenderCid}]),
				task:mk_idle_update(I)};
			
sensor_call({_From, notice_remove, SenderCid}, R, I) ->
			%%io:format("character: get others removed. ~p~n",
			%%	[{notice_remove, Cid} ]),
			{task:add_event(R,
					[{type, "remove"}, {cid, SenderCid}]),
				task:mk_idle_update(I)}.


%% utilities.

get_stats(L) -> L.

add_event(R, Event) when is_record(R, task_env) ->
	R#task_env{
		event_queue = add_element(Event, R#task_env.event_queue)
	}.

add_element(X, Q) -> queue:in([{id, u:make_new_id()}] ++ X,Q).
get_elements(Q) -> queue:to_list(Q).

mk_idle_reset() -> #idle{}.
mk_idle_update(I) when is_record(I, idle) ->
	I#idle{
		since_last_op = timer:now_diff(erlang:now(), I#idle.last_op)
	}.

%%% TEST CODE ------------------------------------------ %%%
-ifdef(TEST).

task_01_test() ->
	{end_of_run_tests}.

-endif.


