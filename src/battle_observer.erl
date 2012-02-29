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

-module(battle_observer).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("mmoasp.hrl").
-export([set_one/3, start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%%===============================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_one(CidFrom, CidTo, DamTupple) ->
	gen_server:call(?MODULE, {report, CidFrom, CidTo, DamTupple}, 20000).

stop() ->
	gen_server:cast(?MODULE, stop).

%%===============================

do_report(CidFrom, CidTo, DamTupple) ->
	%%io:format("do_report called ~p, ~p, ~p~n", [CidFrom, CidTo, DamTupple]),
	store_result(CidTo, DamTupple),
	%%io:format("do_report: got report : ~p~n", [make_reports(CidFrom, CidTo, DamTupple)]),
	notice_results(
		CidFrom,
		CidTo,
		make_reports(CidFrom, CidTo, DamTupple),
		_Radiuis = 30).


make_reports(OidFrom, OidTo, DamTupple) ->
	NewHp = char_kv:getter(OidTo, "hp"),
	Typ = char_kv:getter(OidTo, "type"),
	%%io:format("make_reports: calls proc_damage(~p, ~p, ~p,~p, ~p)~n", [OidFrom, OidTo, DamTupple, Typ, NewHp]),
	proc_damage(OidFrom, OidTo, DamTupple, Typ, NewHp).

proc_damage(_OidFrom, OidTo, DamTupple, "npc", NewHp) when NewHp =< 0 ->
	%%npc:stop_npc(OidTo),	%% this will cause crash. do not remove character here.
	[DamTupple, {killed, OidTo}];

proc_damage(_OidFrom, OidTo, DamTupple, "pc", NewHp) when NewHp =< 0->
	[DamTupple, {killed, OidTo}];

proc_damage(_OidFrom, _OidTo, DamTupple, _CharType, _NewHp) ->
	[DamTupple].



notice_results(OidFrom, OidTo, L, Radius) ->
	[notice_result(OidFrom, OidTo, X, Radius) || X <- L].

notice_result(CidFrom, CidTo, {killed, KilledCid}, Radius) ->
	OL = online_character:get_all_neighbors(CidTo, Radius),
	[X#online_character.pid ! {event, {self(), event, CidFrom, CidTo, killed, KilledCid}}
		|| X <- OL],
	{killed, KilledCid};

notice_result(CidFrom, CidTo, {ok, Dam}, Radius) when is_record(CidFrom, cid), is_record(CidTo, cid)->
	OL = online_character:get_all_neighbors(CidTo, Radius),
	[X#online_character.pid ! {self(), attack, CidFrom, CidTo, ok, Dam}
		|| X <- OL],
	{ok, Dam};

notice_result(CidFrom, CidTo, {ng, Dam}, Radius) when is_record(CidFrom, cid), is_record(CidTo, cid) ->
	OL = online_character:get_all_neighbors(CidTo, Radius),
	[X#online_character.pid ! {self(), attack, CidFrom, CidTo, ng, Dam}
		|| X <- OL],
	{ng, Dam};

notice_result(CidFrom, CidTo, {critical, Dam}, Radius)  when is_record(CidFrom, cid), is_record(CidTo, cid)->
	OL = online_character:get_all_neighbors(CidTo, Radius),
	[X#online_character.pid ! {self(), attack, CidFrom, CidTo, critical, Dam}
		|| X <- OL],
	{critical, Dam};

notice_result(CidFrom, CidTo, {fumble, Dam}, Radius)  when is_record(CidFrom, cid), is_record(CidTo, cid)->
	OL = online_character:get_all_neighbors(CidTo, Radius),
	[X#online_character.pid ! {self(), attack, CidFrom, CidTo, fumble, Dam}
		|| X <- OL],
	{fumble, Dam}.

%% store_result series returns {Result, Damage} tapple.
store_result(OidTo, {ok, X})  when is_record(OidTo, cid) ->
	CurrHp = char_kv:getter(OidTo, "hp"),
	char_kv:setter(OidTo, "hp", (CurrHp - X)),
	{ok, X};
store_result(OidTo, {ng, 0}) when is_record(OidTo, cid) ->
	{ng, 0};
store_result(OidTo, {critical, X}) when is_record(OidTo, cid) ->
	CurrHp = char_kv:getter(OidTo, "hp"),
	char_kv:setter(OidTo, "hp", (CurrHp - X)),
	{critical, X};
store_result(OidTo, {fumble, 0}) when is_record(OidTo, cid) ->
	{fumble, 0}.

%%===============================

init([]) ->
	process_flag(trap_exit, true),
	% io:format("~p starting~n", [?MODULE]),
	{ok, 0}.

handle_call({report, CidFrom, CidTo, Result}, _From, N) ->
	{reply, do_report(CidFrom, CidTo, Result), N+1}.

%% handle_call(stop, _From, N) ->
%%	{stop, normal, stopped, N}.

handle_cast(_Msg, N) ->
	% io:format("~p handle_cast unknown=~p~n", [?MODULE,_Msg]), 
	{noreply, N}.

handle_info(_Info, N) ->
	% io:format("~p handle_info unknown=~p~n", [?MODULE,_Info]), 
	{noreply, N}.

terminate(_Reason, _N) ->
%%	io:format("~p stopping reason=~p, State=~p~n", [?MODULE, _Reason, _N]),
	ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.


%%===== TEST CODES ==========================
-ifdef(TEST).

battle_observer_01_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	{list_to_know, _, _, _} = mmoasp:get_list_to_know(self(), Cid1),

	V1 = char_kv:getter(Cid1, "hp"),
	battle_observer:set_one(Npcid1, Cid1, {ok, 2}),
	V2 = char_kv:getter(Cid1, "hp"),

	?assert(V1 - V2 == 2),

	mmoasp:wait(20),

	{list_to_know, Actions1, _Stats1, _MoveList1}
		= mmoasp:get_list_to_know(self(), Cid1),

	?assert(
		test:sets_by_actions(Actions1, attacker)
		== test:sets_by_list([{attacker, #cid{service_name = hibari, id=99990001}}])),

	?assert(
		test:sets_by_actions(Actions1, type)
		== test:sets_by_list([{type, "attack"}])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

battle_observer_pc_knockouted_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	{list_to_know, _, _, _} = mmoasp:get_list_to_know(self(), Cid1),

	?assert(char_kv:getter(Cid1, "type") == "pc"),
	?assert(char_kv:getter(Npcid1, "type") == "npc"),

	V1 = char_kv:getter(Cid1, "hp"),
	battle_observer:set_one(Npcid1, Cid1, {ok, 9999}),
	V2 = char_kv:getter(Cid1, "hp"),

	?assert(V1 - V2 == 9999),

	mmoasp:wait(20),

	{list_to_know, Actions1, _Stats1, _MoveList1}
		= mmoasp:get_list_to_know(self(), Cid1),

%%	io:format("battle_observer_pc_knockouted_test ~p~n", [Actions1]),

	?assert(
		test:sets_by_actions(Actions1, attacker)
		== test:sets_by_list([{attacker, Npcid1}])),

	?assert(
		test:sets_by_actions(Actions1, cid)
		== test:sets_by_list([{cid, Cid1}])),

%%	io:format("battle_observer_pc_knockouted_test: sets_by_actions(type) ~p~n", [test:sets_by_actions(Actions1, type)]),
	?assert(
		test:sets_by_actions(Actions1, type)
		== test:sets_by_list([{type, "attack"}, {type, "killed"}])),


	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


store_result_ok_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = char_kv:getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {ok, 1}),
	V2 = char_kv:getter(Npcid1, "hp"),

	?assert(V1 - V2 == 1),
	?assert(R1 == {ok, 1}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_ok_999_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = char_kv:getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {ok, 999}),
	V2 = char_kv:getter(Npcid1, "hp"),

	?assert(V1 - V2 == 999),
	?assert(R1 == {ok, 999}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_ng_0_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = char_kv:getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {ng, 0}),
	V2 = char_kv:getter(Npcid1, "hp"),

	?assert(V1 - V2 == 0),
	?assert(R1 == {ng, 0}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_fumble_0_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = char_kv:getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {fumble, 0}),
	V2 = char_kv:getter(Npcid1, "hp"),

	?assert(V1 - V2 == 0),
	?assert(R1 == {fumble, 0}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_critical_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = char_kv:getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {critical, 1}),
	V2 = char_kv:getter(Npcid1, "hp"),

	?assert(V1 - V2 == 1),
	?assert(R1 == {critical, 1}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_critical_999_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = char_kv:getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {critical, 999}),
	V2 = char_kv:getter(Npcid1, "hp"),

	?assert(V1 - V2 == 999),
	?assert(R1 == {critical, 999}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_twice_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = char_kv:getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {critical, 999}),
	V2 = char_kv:getter(Npcid1, "hp"),

	?assert(V1 - V2 == 999),
	?assert(R1 == {critical, 999}),

	R2 = store_result(Npcid1, {ok, 1}),
	V3 = char_kv:getter(Npcid1, "hp"),

	?assert(V2 - V3 == 1),
	?assert(R2 == {ok, 1}),

	?assert(V1 - V3 == 1000),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


notice_results_nil_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	{list_to_know, _, _, _} = mmoasp:get_list_to_know(self(), Cid1),

	L2 = notice_results(Npcid1, Cid1, [], 30),
	?assert(L2 == []),

	u:wait(20),

	{list_to_know, Actions1, _Stats1, _MoveInfo1}
		= mmoasp:get_list_to_know(self(), Cid1),
%%	io:format("list to know for ~p: ~p~n",
%%		[Cid1, {list_to_know, Actions1, Stats1}]),

	?assert(
		test:sets_by_actions(Actions1, from_cid)
		== test:sets_by_list([])),

	?assert(
		test:sets_by_actions(Actions1, type)
		== test:sets_by_list([])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

notice_results_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	{list_to_know, _, _, _} = mmoasp:get_list_to_know(self(), Cid1),

	L2 = notice_results(Npcid1, Cid1, [{ok, 999}], 30),
	?assert(L2 == [{ok, 999}]),

	u:wait(20),

	{list_to_know, Actions1, _Stats1, _MoveInfo1}
		= mmoasp:get_list_to_know(self(), Cid1),
%%	io:format("list to know for ~p: ~p~n",
%%		[Cid1, {list_to_know, Actions1, Stats1}]),

	?assert(
		test:sets_by_actions(Actions1, attacker)
		== test:sets_by_list([{attacker, #cid{service_name = hibari, id=99990001}}])),

	?assert(
		test:sets_by_actions(Actions1, type)
		== test:sets_by_list([{type, "attack"}])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

notice_results_2_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	{list_to_know, _, _, _} = mmoasp:get_list_to_know(self(), Cid1),

	L2 = notice_results(Npcid1, Cid1, [{ok, 7}, {ok, 13}], 30),
	?assert(L2 == [{ok, 7}, {ok, 13}]),

	u:wait(20),

	{list_to_know, Actions1, _Stats1, _MoveInfo1}
		= mmoasp:get_list_to_know(self(), Cid1),
%%	io:format("list to know for ~p: ~p~n",
%%		[Cid1, {list_to_know, Actions1, Stats1}]),

	?assert(
		test:sets_by_actions(Actions1, attacker)
		== test:sets_by_list([{attacker, #cid{service_name = hibari, id=99990001}}, {attacker, #cid{service_name = hibari, id=99990001}}])),

	?assert(
		test:sets_by_actions(Actions1, type)
		== test:sets_by_list([{type, "attack"}, {type, "attack"}])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.
