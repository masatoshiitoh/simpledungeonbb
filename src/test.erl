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
scenario_00_test()-> {atomic,ok} = mmoasp:change_schema().
scenario_01_test()-> {end_of_run_tests} = check_session_data().
scenario_02_test()-> {end_of_run_tests} = do_trades().
scenario_03_test()-> {end_of_run_tests} = do_talk().
scenario_04_test()-> {end_of_run_tests} = do_setter().
scenario_05_test()-> {end_of_run_tests} = do_npc_move().
scenario_06_test()-> {end_of_run_tests} = do_pc_move().
%scenario_05_test()-> {timeout, 60, fun() -> do_npc_move() end}.
%scenario_06_test()-> {timeout, 60, fun() -> do_pc_move() end}.
scenario_07_test()-> {end_of_run_tests} = do_look_around().
scenario_08_test()-> {end_of_run_tests} = do_stat().
scenario_09_test()-> {end_of_run_tests} = do_battle_unarmed().

check_record_test() ->
	%% what is this test?
	%% because, I wrote 'erlang:now()' in record 'idle' default value.
	%% This code assumes every 'idle' initialization makes
	%% different 'last_op' value.
	N1 = #idle{},
	receive
		after 2000 -> ok
	end,
	N2 = #idle{},
	?assert(N1#idle.last_op /= N2#idle.last_op).

run_tests_with_log()
	->
		mmoasp:change_schema(),
		eunit:test(
		[battle,unarmed,test,u,throw],
		[{report,{eunit_surefire,[{dir,"."}]}}]).

run_tests() ->
	scenario_00_test(),
	scenario_01_test(),
	scenario_02_test(),
	scenario_03_test(),
	scenario_04_test(),
	scenario_07_test(),
	scenario_08_test(),
	scenario_06_test(),
	scenario_05_test(),
	{end_of_test}.


up_scenarios() ->
	db:reset_tables(),
	mmoasp:start(),
	NpcPid1 = npc:start_npc("npc0001"),
	{ok, Cid1, Token1}
		= mmoasp:login(self(), "id0001", "pw0001", {192,168,1,200}),
	{ok, Cid2, Token2}
		= mmoasp:login(self(), "id0002", "pw0002", {192,168,1,201}),

	{scenarios, Cid1, Token1, Cid2, Token2, "npc0001"}.

down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}) ->
	mmoasp:logout(self(), Cid1, Token1),
	mmoasp:logout(self(), Cid2, Token2),
	npc:stop_npc(Npcid1),
	path_finder:stop(),
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



do_battle_unarmed() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% look around test
	?assert(1 == u:distance(
		{session, world:get_session(Cid1)},
		{session, world:get_session("npc0001")})),
	
	?assert(3 == u:distance(
		{session, world:get_session(Cid2)},
		{session, world:get_session("npc0001")})),

	%% try unarmed battle.(Cid1 ok / Cid2 fail (too far))
	
	{R1, _} = battle:single(Cid1, "npc0001"),
	{R2, _} = battle:single(Cid2, "npc0001"),
	?assert(R1 == ok),
	?assert(R2 == ng),

	{R3, _} = battle:single(Cid1, "npc0001", "unarmed"),
	{R4, _} = battle:single(Cid2, "npc0001", "unarmed"),
	?assert(R3 == ok),
	?assert(R4 == ng),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.




do_look_around() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% look around test
	?assert(4 == u:distance({session, world:get_session(Cid1)}, {session, world:get_session(Cid2)})),

	?assert(sets:from_list(["cid0001"])
		== sets:from_list([X#session.oid || X <- mmoasp:get_neighbor_char_sessions(Cid1, 1)])),

	?assert(sets:from_list(["cid0001", "cid0002"])
		== sets:from_list([X#session.oid || X <- mmoasp:get_neighbor_char_sessions(Cid1, 4)])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_stat() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	%% update neighbor stat.
	X1 = world:get_session(Cid1),
	X1#session.pid ! {self(), update_neighbor_status, 10},
	X2 = world:get_session(Cid2),
	X2#session.pid ! {self(), update_neighbor_status, 10},

	% io:format("update request has sent.~n", []),

	receive
		after 200 -> ok
	end,

	{actions_and_stats, Actions1, Stats1}
		= mmoasp:get_list_to_know(self(), Cid1),
	
	{actions_and_stats, Actions2, Stats2}
		= mmoasp:get_list_to_know(self(), Cid2),

	io:format("list to know for ~p: ~p~n",
		[Cid1, {actions_and_stats, Actions1, Stats1}]),
	io:format("list to know for ~p: ~p~n",
		[Cid2, {actions_and_stats, Actions2, Stats2}]),

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
	S0 = world:get_session(Cid1),
	?assert(S0#session.x == 1),
	?assert(S0#session.y == 1),
	?assert(S0#session.map == 1),
	
	%% moving !
	io:format("order move 1,3 to 3,3 ~p~n", [mmoasp:move(Cid1, {pos, 3,3})]),
	receive
		after 500 -> ok
	end,
	io:format("RE-order move to 1,2 ~p~n", [mmoasp:move(Cid1, {pos, 1,2})]),
	receive
		after 3000 -> ok
	end,
	
	
	S1 = world:get_session(Cid1),
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
	S0 = world:get_session(Npcid1),
	?assert(S0#session.x == 2),
	?assert(S0#session.y == 1),
	?assert(S0#session.map == 1),
	
	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),
	
	%% NPC moving !
	io:format("NPC move to 2,2 ~p~n", [mmoasp:move(Npcid1, {pos, 3,1})]),
	receive
		after 1100 -> ok
	end,
	io:format("Latest session ~p~n", [world:get_session(Npcid1)]),
	
	S1 = world:get_session(Npcid1),
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

	X = mmoasp:get_neighbor_char_cdata(Cid1, 10),%% at this point, X is #cdata.attr .
	io:format("neighbor_char_cdata of ~p: ~p~n", [Cid1, X]),
	
	Me = world:get_session(Cid1),
	F = fun() ->
		qlc:e(qlc:q(
			[NewCData = CData#cdata{ attr = CData#cdata.attr ++ [
					{"x", Sess#session.x},{"y", Sess#session.y},{"z", Sess#session.z},{"map", Sess#session.map}
				]}
				|| Sess <- mnesia:table(session),
				%%	Loc#location.cid =/= Cid,
				u:distance({session, Sess}, {session, Me}) < 10,
				CData <- mnesia:table(cdata),	
				CData#cdata.cid == Sess#session.oid]))
	end,
	io:format("check internal, get_neighbor_char_cdata ~p~n", [case mnesia:transaction(F) of
		{atomic, Result} -> Result;
		Other -> Other
	end]),



	%% at this point, X cauase badrecord error on character:gen_stat_from_cdata. because gen_stat_from_cdata requires cdata record.
	io:format("gen_stat_from_cdata of X: ~p~n", [[character:gen_stat_from_cdata(A) || A <- X]]),


	io:format("mmoasp:get_neighbor_char_cdata of ~p: ~p~n", [Cid1, mmoasp:get_neighbor_char_cdata(Cid1, 10)]),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

do_trades() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),

	%% trade check.
	io:format("before :~n 1: ~p~n 2: ~p~n", [db:demo(inventory, Cid1),db:demo(inventory, Cid2)]),
	mmoasp:start_trade(Cid1, Cid2),
	io:format("trade started... ~p~n1: ~p~n2: ~p~n", [
		db:demo(select_trade),
		db:demo(u_trade, Cid1),
		db:demo(u_trade, Cid2)]),
	mmoasp:set_offer(Cid1, 112, [],[]),
	mmoasp:set_offer(Cid2, 0, [{item_herb, 2}],[item_shield01]),
	mmoasp:confirm_trade(Cid1),
	mmoasp:confirm_trade(Cid2),
	io:format("after :~n 1: ~p~n 2: ~p~n", [db:demo(inventory, Cid1),db:demo(inventory, Cid2)]),

	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


do_talk() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	%% talk check
	mmoasp:talk(open,Cid2, "hello all from cid 1234!!! ", 100),
	mmoasp:talk(open,Cid2, "will not appear this message!!! ", 100),
	mmoasp:talk(whisper,Cid2, Cid1, "hello cid 1 from cid 1234, with love :-)"),
	mmoasp:talk(whisper,Cid2, Cid1, "talk, line 2"),

	
	{actions_and_stats, A1, S1} = mmoasp:get_list_to_know(self(), Cid1),
	%io:format("list_to_json with ~p: ~p~n", [Cid1, mout:list_to_json(A1 ++ S1)]),
	{actions_and_stats, A2, S2} = mmoasp:get_list_to_know(self(), Cid2),
	%io:format("list_to_json with ~p: ~p~n", [Cid2, mout:list_to_json(A2 ++ S2)]),

	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

do_setter() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),
	
	%% setter check.
	mmoasp:setter(self(), Cid1, Token1, "WindowSize", "123,55"),
	mmoasp:setter(self(), Cid2, Token2, "WindowSize", "99,160"),
	io:format("setter :~n 1: ~p~n 2: ~p~n", [db:demo(cdata, Cid1),db:demo(cdata, Cid2)]),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


