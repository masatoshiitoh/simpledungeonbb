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

-export([run_tests_with_log/0, up_scenarios/0, down_scenarios/1]).
-export([repeat/2]).
-export([sets_by_actions/2, sets_by_list/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-import(lists, [foreach/2]).

-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
scenario_00_test()-> {atomic,ok} = db:change_schema().
scenario_01_test()-> {end_of_run_tests} = check_session_data().
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
		db:change_schema(),
		eunit:test(
		[
		
character,
character_stream,
db,
mmoasp,
morningcall,
mout,
move,
npc,
path_finder,
simpledungeon,
task,
throw,
u,
unarmed,
battle,
battle_observer,
con_yaws,
map2d,
sd_api,
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
	_NpcPid1 = npc:start_npc("npc0001"),
	{ok, Cid1, Token1}
		= sd_api:login(self(), "id0001", "pw0001", {192,168,1,200}),
	{ok, Cid2, Token2}
		= sd_api:login(self(), "id0002", "pw0002", {192,168,1,201}),
	{scenarios, Cid1, Token1, Cid2, Token2, "npc0001"}.

down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}) ->
	sd_api:logout(self(), Cid1, Token1),
	sd_api:logout(self(), Cid2, Token2),
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
	?assert(1 == map2d:distance(
		{session, mmoasp:get_session(Cid1)},
		{session, mmoasp:get_session("npc0001")})),
	
	?assert(3 == map2d:distance(
		{session, mmoasp:get_session(Cid2)},
		{session, mmoasp:get_session("npc0001")})),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_battle_unarmed_021() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% try unarmed battle.(Cid1 ok / Cid2 fail (too far))
	
	[{R1, _}|_T1] = battle:single(Cid1, "npc0001"),
	[{R2, _}|_T2] = battle:single(Cid2, "npc0001"),
	?assert(R1 == ok),
	?assert(R2 == ng),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_battle_unarmed_022() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% try unarmed battle.(Cid1 ok)
	
	[{R3, _}|_T3] = battle:single(Cid1, "npc0001", "unarmed"),
	?assert( (R3 == ok) or (R3 == critical) ),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_battle_unarmed_023() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% try unarmed battle.(Cid2 fail (too far))
	[{R4, _}|_T4] = battle:single(Cid2, "npc0001", "unarmed"),
	?assert(R4 == ng),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),

	{end_of_run_tests}.



do_look_around() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% look around test
	?assert(4 == map2d:distance({session, mmoasp:get_session(Cid1)}, {session, mmoasp:get_session(Cid2)})),

	?assert(sets:from_list(["cid0001"])
		== sets:from_list([X#session.cid || X <- map2d:get_neighbor_char_sessions(Cid1, 1)])),

	?assert(sets:from_list(["cid0001", "cid0002"])
		== sets:from_list([X#session.cid || X <- map2d:get_neighbor_char_sessions(Cid1, 4)])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_stat() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% update neighbor stat.
	X1 = mmoasp:get_session(Cid1),
	X1#session.pid ! {self(), update_neighbor_status, 10},
	X2 = mmoasp:get_session(Cid2),
	X2#session.pid ! {self(), update_neighbor_status, 10},

	% io:format("update request has sent.~n", []),

	u:wait(200),

	{list_to_know, Actions1, Stats1, MoveInfo1}
		= sd_api:get_list_to_know(self(), Cid1),
	
	{list_to_know, Actions2, Stats2, MoveInfo2}
		= sd_api:get_list_to_know(self(), Cid2),

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
			[{cid, "cid0001"}, {cid, "cid0002"}, {cid, "npc0001"}])),

	CidList2 = lists:flatten(
		[[{K, V} || {K, V} <- ST, K == cid] || ST <- Stats2]),
	?assert(sets:from_list(CidList2)
		== sets:from_list(
			[{cid, "cid0001"}, {cid, "cid0002"}, {cid, "npc0001"}])),

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
	
	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),
	S0 = mmoasp:get_session(Cid1),
	?assert(is_record(S0, session)),
	?assert(S0#session.x == 1),
	?assert(S0#session.y == 1),
	?assert(S0#session.map == 1),
	
	%% moving !
	io:format("order move 1,3 to 3,3 ~p~n", [move:move({map_id, "hibari", 1}, Cid1, {pos, 3,3})]),
	receive
		after 500 -> ok
	end,
	io:format("RE-order move to 1,2 ~p~n", [move:move({map_id, "hibari", 1}, Cid1, {pos, 1,2})]),
	receive
		after 3000 -> ok
	end,
	
	
	S1 = mmoasp:get_session(Cid1),
	?assert(is_record(S1, session)),
	?assert(S1#session.x == 1),
	?assert(S1#session.y == 2),
	?assert(S1#session.map == 1),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),

	receive
		after 1000 -> ok
	end,
	io:format("after stop npc ~p~n", [db:demo(session)]),

	{end_of_run_tests}.


do_npc_move() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	io:format("npc starts at ~p~n", [db:demo(session)]),
	S0 = mmoasp:get_session(Npcid1),
	?assert(S0#session.x == 2),
	?assert(S0#session.y == 1),
	?assert(S0#session.map == 1),
	
	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),
	
	%% NPC moving !
	io:format("NPC move to 2,2 ~p~n", [move:move({map_id, "hibari", 1}, Npcid1, {pos, 3,1})]),
	receive
		after 1100 -> ok
	end,
	io:format("Latest session ~p~n", [mmoasp:get_session(Npcid1)]),
	
	S1 = mmoasp:get_session(Npcid1),
	?assert(S1#session.x == 3),
	?assert(S1#session.y == 1),
	?assert(S1#session.map == 1),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),

	receive
		after 1000 -> ok
	end,
	io:format("after stop npc ~p~n", [db:demo(session)]),
	{end_of_run_tests}.



% NOT working as test (just only work.).
check_session_data() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	X = map2d:get_neighbor_char_cdata(Cid1, 10),%% at this point, X is #cdata.attr .
%%	io:format("neighbor_char_cdata of ~p: ~p~n", [Cid1, X]),
	
	Me = mmoasp:get_session(Cid1),
	F = fun() ->
		qlc:e(qlc:q(
			[_NewCData = CData#cdata{ attr = CData#cdata.attr ++ [
					{"x", Sess#session.x},{"y", Sess#session.y},{"z", Sess#session.z},{"map", Sess#session.map}
				]}
				|| Sess <- mnesia:table(session),
				%%	Loc#location.cid =/= Cid,
				map2d:distance({session, Sess}, {session, Me}) < 10,
				CData <- mnesia:table(cdata),	
				CData#cdata.cid == Sess#session.cid]))
	end,
	io:format("check internal, get_neighbor_char_cdata ~p~n", [case mnesia:transaction(F) of
		{atomic, Result} -> Result;
		Other -> Other
	end]),

	%% at this point, X cauase badrecord error on mmoasp:gen_stat_from_cdata. because gen_stat_from_cdata requires cdata record.
	io:format("gen_stat_from_cdata of X: ~p~n", [[mmoasp:gen_stat_from_cdata(A) || A <- X]]),


	io:format("map2d:get_neighbor_char_cdata of ~p: ~p~n", [Cid1, map2d:get_neighbor_char_cdata(Cid1, 10)]),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

do_talk() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	%% talk check
	sd_api:talk(open,Cid2, "hello all from cid 1234!!! ", 100),
	sd_api:talk(open,Cid2, "will not appear this message!!! ", 100),
	sd_api:talk(whisper,Cid2, Cid1, "hello cid 1 from cid 1234, with love :-)"),
	sd_api:talk(whisper,Cid2, Cid1, "talk, line 2"),

	
	{list_to_know, _A1, _S1, _M1} = sd_api:get_list_to_know(self(), Cid1),
	%io:format("list_to_json with ~p: ~p~n", [Cid1, mout:list_to_json(A1 ++ S1)]),
	{list_to_know, _A2, _S2, _m2} = sd_api:get_list_to_know(self(), Cid2),
	%io:format("list_to_json with ~p: ~p~n", [Cid2, mout:list_to_json(A2 ++ S2)]),

	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

do_setter() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),
	
	%% setter check.
	sd_api:setter(Cid1, "WindowSize", "123,55"),
	sd_api:setter(Cid2, "WindowSize", "99,160"),
	io:format("setter :~n 1: ~p~n 2: ~p~n", [db:demo(cdata, Cid1),db:demo(cdata, Cid2)]),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


sets_by_list(L) ->
	sets:from_list(L).

sets_by_actions(L, Type) ->
	sets:from_list(lists:flatten(
		[[{K, V} || {K, V} <- ST, K == Type] || ST <- L])).
