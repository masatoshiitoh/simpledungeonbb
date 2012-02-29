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


-module(test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile(export_all).

-import(lists, [foreach/2]).

-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
scenario_00_test()-> ok = mmoasp:change_schema().
scenario_01_test()-> {end_of_run_tests}.
%scenario_02_test()-> {end_of_run_tests} = do_trades().
scenario_03_test()-> {end_of_run_tests} = do_talk().
scenario_04_test()-> {end_of_run_tests} = do_setter().
scenario_05_test()-> {end_of_run_tests} = do_npc_move().
scenario_06_test()-> {end_of_run_tests} = do_pc_move().
scenario_07_test()-> {end_of_run_tests} = do_look_around().
scenario_08_test()-> {end_of_run_tests} = do_stat().
scenario_091_test()-> {end_of_run_tests} = do_battle_unarmed_01().
scenario_0921_test()-> {end_of_run_tests} = do_battle_unarmed_021().
scenario_0922_test()-> {end_of_run_tests} = do_battle_unarmed_022().
scenario_0923_test()-> {end_of_run_tests} = do_battle_unarmed_023().

check_record_test() ->
	%% what is this test? why did I put a test for erlang module?
	%% because, I wrote 'erlang:now()' in record 'idle' default value.
	%% This code assumes different 'last_op' values on 
	%% every 'idle' initialization.
	N1 = #idle{},
	receive
		after 500 -> ok
	end,
	N2 = #idle{},
	?assert(N1#idle.last_op /= N2#idle.last_op).

run_tests_with_log()
	->
		mmoasp:change_schema(),
		eunit:test(
		[
		
%%admin,
character,
character_stream,
db,
%%demo,
%%melee,
mmoasp,
morningcall,
mout,
move,
npc,
path_finder,
simpledungeon,
task,
throw,
%%trade,
u,
%%uauth,
unarmed,
%%world,
battle,
battle_observer,
char_kv,
default,
online_character,
test
		],
		[{report,{eunit_surefire,[{dir,"."}]}}]).

run_tests() ->
	scenario_091_test(),
	scenario_0921_test(),
	scenario_0922_test(),
	scenario_0923_test(),

	scenario_00_test(),
	scenario_01_test(),
%	scenario_02_test(),
	scenario_03_test(),
	scenario_04_test(),
	scenario_07_test(),
	scenario_08_test(),
	scenario_06_test(),
	scenario_05_test(),
	{end_of_test}.


up_scenarios() ->
	mmoasp:start(reset_tables),
%	u:wait(100),
	_NpcPid1 = npc:start_npc(#cid{service_name = hibari, id=99990001}),
	{ok, Cid1, Token1} = id_password:login(self(), hibari, "lid00001", "password", {192,168,1,200}),
	{ok, Cid2, Token2} = mmoasp:login(self(), id_password:make_login_id(hibari, "lid00002"), "password", {192,168,1,201}),
	{scenarios, Cid1, Token1, Cid2, Token2, #cid{service_name = hibari, id=99990001}}.

down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}) ->
	id_password:logout(self(), Cid1, {127,0,0,1}),
	id_password:logout(self(), Cid2, {127,0,0,1}),
	npc:stop_npc(Npcid1),
	mmoasp:stop(),
	ok.


repeat(_F, 0) -> {end_of_test};
repeat(F, X) ->
	F(),
	repeat(F, X - 1).

-endif.


%
% test codes
%
% working.


do_battle_unarmed_01() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% look around test
	?assert(1.0 == u:distance(
		{online_character, online_character:get_one(Cid1)},
		{online_character, online_character:get_one(Npcid1)})),
	
	?assert(3 == u:distance(
		{online_character, online_character:get_one(Cid2)},
		{online_character, online_character:get_one(Npcid1)})),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_battle_unarmed_021() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% try unarmed battle.(Cid1 ok / Cid2 fail (too far))
	
	[{R1, _}|_T1] = battle:single(Cid1, Npcid1),
	[{R2, _}|_T2] = battle:single(Cid2, Npcid1),
	?assert(R1 == ok),
	?assert(R2 == ng),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_battle_unarmed_022() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% try unarmed battle.(Cid1 ok)
	
	[{R3, _}|_T3] = battle:single(Cid1, Npcid1, "unarmed"),
	?assert( (R3 == ok) or (R3 == critical) ),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_battle_unarmed_023() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% try unarmed battle.(Cid2 fail (too far))
	[{R4, _}|_T4] = battle:single(Cid2, Npcid1, "unarmed"),
	?assert(R4 == ng),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),

	{end_of_run_tests}.



do_look_around() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% look around test
	?assert(4 == u:distance({session, mmoasp:get_session(Cid1)}, {session, mmoasp:get_session(Cid2)})),

	?assert(sets:from_list(["cid0001"])
		== sets:from_list([X#session.cid || X <- mmoasp:get_neighbor_char_sessions(Cid1, 1)])),

	?assert(sets:from_list(["cid0001", "cid0002"])
		== sets:from_list([X#session.cid || X <- mmoasp:get_neighbor_char_sessions(Cid1, 4)])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_stat() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% update neighbor stat.
	online_character:send_message_by_cid(Cid1, {self(), update_neighbor_status, 10}),
	online_character:send_message_by_cid(Cid2, {self(), update_neighbor_status, 10}),

	% io:format("update request has sent.~n", []),

	mmoasp:wait(200),

	{list_to_know, Actions1, Stats1, MoveInfo1}
		= mmoasp:get_list_to_know(self(), Cid1),
	
	{list_to_know, Actions2, Stats2, MoveInfo2}
		= mmoasp:get_list_to_know(self(), Cid2),

	io:format("list to know for ~p: ~p~n",
		[Cid1, {list_to_know, Actions1, Stats1}]),
	io:format("list to know for ~p: ~p~n",
		[Cid2, {list_to_know, Actions2, Stats2}]),

	%% now, world has 2 characters "cid0001" and "cid0002",
	%% and  1 npc (npc0001).
	CidList1 = lists:flatten(
		[[{K, V} || {K, V} <- ST, K == cid] || ST <- Stats1]),
	?assert(sets:from_list(CidList1)
		== sets:from_list(
			[{cid, "cid0001"}, {cid, "cid0002"}, {cid, #cid{service_name = hibari, id=99990001}}])),

	CidList2 = lists:flatten(
		[[{K, V} || {K, V} <- ST, K == cid] || ST <- Stats2]),
	?assert(sets:from_list(CidList2)
		== sets:from_list(
			[{cid, "cid0001"}, {cid, "cid0002"}, {cid, #cid{service_name = hibari, id=99990001}}])),

	%% "cid0001" knows "cid0001" and "cid0002" login.
	AList1 = lists:flatten(
		[[{K, V} || {K, V} <- ST, K == cid] || ST <- Actions1]),
	?assert(sets:from_list(AList1)
		== sets:from_list([{cid, "cid0001"}, {cid, "cid0002"}])),
	?assert(
		sets:from_list(lists:flatten(
			[[{K, V} || {K, V} <- ST, K == type] || ST <- Actions1]))
		== sets:from_list([{type, "login"}, {type, "login"}])),

	%% "cid0002" knows only "cid0002" login.
	AList2 = lists:flatten(
		[[{K, V} || {K, V} <- ST, K == type] || ST <- Actions2]),
	?assert(sets:from_list(AList2)
		== sets:from_list([{type, "login"}])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.



do_pc_move() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
%	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),
	O = online_character:get_one(Cid1),
	?assert(is_record(O, online_character)),
	?assert(O#online_character.location ==
		#location{
			map_id = #map_id{service_name = hibari, id = 1},
			x = 1,
			y = 1}),
	
	%% moving !
	io:format("order move 1,3 to 3,3 ~p~n", [move:move({map_id, "hibari", 1}, Cid1, {pos, 3,3})]),
	receive
		after 500 -> ok
	end,
	io:format("RE-order move to 1,2 ~p~n", [move:move({map_id, "hibari", 1}, Cid1, {pos, 1,2})]),
	receive
		after 3000 -> ok
	end,
	
	O1 = online_character:get_one(Cid1),
	?assert(is_record(O1, online_character)),
	?assert(O#online_character.location ==
		#location{
			map_id = #map_id{service_name = hibari, id = 1},
			x = 1,
			y = 2}),
	
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),

	receive
		after 1000 -> ok
	end,
%	io:format("after stop npc ~p~n", [db:demo(session)]),

	{end_of_run_tests}.


do_npc_move() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

%	io:format("npc starts at ~p~n", [db:demo(online_character)]),

	O1 = online_character:get_one(Npcid1),
	?assert(is_record(O1, online_character)),
	?assert(O1#online_character.location ==
		#location{
			map_id = #map_id{service_name = hibari, id = 1},
			x = 2,
			y = 1}),
	
%	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),
	
	%% NPC moving !
	io:format("NPC move to 2,2 ~p~n", [move:move({map_id, "hibari", 1}, Npcid1, {pos, 3,1})]),
	receive
		after 1100 -> ok
	end,
	io:format("Latest online_character ~p~n", [online_character:get_one(Npcid1)]),
	
	O2 = online_character:get_one(Npcid1),
	?assert(is_record(O2, online_character)),
	?assert(O2#online_character.location ==
		#location{
			map_id = #map_id{service_name = hibari, id = 1},
			x = 3,
			y = 1}),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),

	receive
		after 1000 -> ok
	end,
%	io:format("after stop npc ~p~n", [db:demo(session)]),
	{end_of_run_tests}.

do_trades() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

%	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),

	%% trade check.
%	io:format("before :~n 1: ~p~n 2: ~p~n", [db:demo(inventory, Cid1),db:demo(inventory, Cid2)]),
	mmoasp:start_trade(Cid1, Cid2),
%%	io:format("trade started... ~p~n1: ~p~n2: ~p~n", [
%%		db:demo(select_trade),
%%		db:demo(u_trade, Cid1),
%%		db:demo(u_trade, Cid2)]),
	mmoasp:set_offer(Cid1, 112, [],[]),
	mmoasp:set_offer(Cid2, 0, [{item_herb, 2}],[item_shield01]),
	mmoasp:confirm_trade(Cid1),
	mmoasp:confirm_trade(Cid2),
%	io:format("after :~n 1: ~p~n 2: ~p~n", [db:demo(inventory, Cid1),db:demo(inventory, Cid2)]),

	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_talk() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	%% talk check
	mmoasp:talk(open,Cid2, "hello all from cid 1234!!! ", 100),
	mmoasp:talk(open,Cid2, "will not appear this message!!! ", 100),
	mmoasp:talk(whisper,Cid2, Cid1, "hello cid 1 from cid 1234, with love :-)"),
	mmoasp:talk(whisper,Cid2, Cid1, "talk, line 2"),

	
	{list_to_know, _A1, _S1, _M1} = mmoasp:get_list_to_know(self(), Cid1),
	%io:format("list_to_json with ~p: ~p~n", [Cid1, mout:list_to_json(A1 ++ S1)]),
	{list_to_know, _A2, _S2, _m2} = mmoasp:get_list_to_know(self(), Cid2),
	%io:format("list_to_json with ~p: ~p~n", [Cid2, mout:list_to_json(A2 ++ S2)]),

	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

do_setter() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
%	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),
	
	%% setter check.
	char_kv:setter(Cid1, "WindowSize", "123,55"),
	char_kv:setter(Cid2, "WindowSize", "99,160"),
%	io:format("setter :~n 1: ~p~n 2: ~p~n", [db:demo(cdata, Cid1),db:demo(cdata, Cid2)]),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


sets_by_list(L) ->
	sets:from_list(L).

sets_by_actions(L, Type) ->
	sets:from_list(lists:flatten(
		[[{K, V} || {K, V} <- ST, K == Type] || ST <- L])).
