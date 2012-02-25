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


%%% TEST CODE ------------------------------------------ %%%
-ifdef(TEST).

%% TODO: make this work.
%% just now, only scenario test (in test.erl) checks these *_call/3s.
%
%system_call_01_test() ->
%	R = #task_env{},
%	I = mk_idle_reset(),
%
%	{NewR, NewI} = system_call({self(), stop_process}, R, I),
%	
%	?assert(NewR == undefined),
%	?assert(NewI == undefined),
%
%	{end_of_run_tests}.

-endif.

%% for task utilities for PC/NPC

system_call({From, stop_process}, R, _I) ->
%%	io:format("~p: proc stop by stop_process message.~n", [R#task_env.cid]),
	morningcall:cancel_all(R#task_env.utimer),
	From ! {ok, R#task_env.cid},
	{NewR = undefined, NewI = undefined}.

test_call({From, whoareyou}, R, I) ->
	From ! {iam, R#task_env.cid},
	{R, mk_idle_update(I)}.

%% EVENT

event_call({_From, event, OidFrom, OidTo, Event, EventOwner}, R, I) ->
	io:format("character: ~p had event ~p~n", [EventOwner, Event]),
	{add_event(R,
		[{type, erlang:atom_to_list(Event)},
			{cid, EventOwner},
			{from_cid, OidFrom},
			{to_cid, OidTo}]),
		mk_idle_update(I)}.

%% TIMER
timer_call({goodmorning, Id}, R, I) ->
	{_FunResult, NewUTimer} = morningcall:dispatch(Id, R#task_env.utimer),
	{R#task_env{utimer = NewUTimer}, mk_idle_update(I)};

timer_call({_From, cancel_timer}, R, I) ->
	NewUTimer = morningcall:cancel_all(R#task_env.utimer),
	{R#task_env{utimer = NewUTimer}, mk_idle_update(I)}.

sensor_call({From, request_list_to_know}, R, _I) ->
			From ! {list_to_know,
				task:get_elements(R#task_env.event_queue),
				get_stats(R#task_env.stat_dict),
				get_values(R#task_env.move_path_dict)
				},
			io:format("listtoknow: ~p L2KN ~p~n", [From, task:get_elements(R#task_env.event_queue)]),
			io:format("listtoknow: ~p STAT ~p~n", [From, get_stats(R#task_env.stat_dict)]),
			io:format("listtoknow: ~p MVPA ~p~n", [From, get_values(R#task_env.move_path_dict)]),
			
			{R#task_env{event_queue = queue:new(), move_path_dict = dict:new()}, task:mk_idle_reset()};

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

get_values(Dict) ->
	[V || {_K,V} <- dict:to_list(Dict)].

get_stats(L) -> L.

add_event(R, Event) when is_record(R, task_env) ->
	R#task_env{
		event_queue = add_element(Event, R#task_env.event_queue)
	}.

add_element(X, Q) -> queue:in([{id, mmoasp:make_new_id()}] ++ X,Q).
get_elements(Q) -> queue:to_list(Q).

mk_idle_reset() -> #idle{}.
mk_idle_update(I) when is_record(I, idle) ->
	I#idle{
		since_last_op = timer:now_diff(erlang:now(), I#idle.last_op)
	}.

-ifdef(TEST).

get_values_01_test() ->
	Dict = dict:from_list([{"cid0001","1"},{"cid0002","2"}]),
	L = get_values(Dict),
	SL = lists:sort(L),
	?assert(SL == ["1","2"]),
	{end_of_run_tests}.

-endif.

