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


-ifdef(TEST).

battle_observer_01_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	{actions_and_stats, _, _} = mmoasp:get_list_to_know(self(), Cid1),

	V1 = u:db_getter(Cid1, "hp"),
	battle_observer:set_one(Npcid1, Cid1, {ok, 2}),
	V2 = u:db_getter(Cid1, "hp"),

	?assert(V1 - V2 == 2),

	receive
		after 200 -> ok
	end,

	{actions_and_stats, Actions1, Stats1}
		= mmoasp:get_list_to_know(self(), Cid1),
	?assert(
		sets:from_list(lists:flatten(
			[[{K, V} || {K, V} <- ST, K == from_cid] || ST <- Actions1]))
		== sets:from_list([{from_cid, "npc0001"}])),
	?assert(
		sets:from_list(lists:flatten(
			[[{K, V} || {K, V} <- ST, K == type] || ST <- Actions1]))
		== sets:from_list([{type, "attack"}])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.


%%===============================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_one(CidFrom, CidTo, DamTupple) ->
	gen_server:call(?MODULE, {report, CidFrom, CidTo, DamTupple}, 20000).

stop() ->
	gen_server:call(?MODULE, stop).

%%===============================

do_report(CidFrom, CidTo, DamTupple) ->
	notice_result(
		CidFrom,
		CidTo,
		store_result(CidTo, DamTupple),
		_Radiuis = 30).

notice_results(OidFrom, OidTo, L, Radius) ->
	[notice_result(OidFrom, OidTo, X, Radius) || X <- L].

notice_result(OidFrom, OidTo, DamTupple, Radius) ->
	{Result, DamageVal} = DamTupple,
	Sessions = mmoasp:get_all_neighbor_sessions(OidTo, Radius),
	[X#session.pid ! {self(), attack, OidFrom, OidTo, Result, DamageVal}
		|| X <- Sessions],
	DamTupple.

%% store_result series returns {Result, Damage} tapple.
store_result(OidTo, {ok, X}) ->
	CurrHp = u:db_getter(OidTo, "hp"),
	u:db_setter(OidTo, "hp", (CurrHp - X)),
	{ok, X};
store_result(_OidTo, {ng, 0}) ->
	{ng, 0};
store_result(OidTo, {critical, X}) ->
	CurrHp = u:db_getter(OidTo, "hp"),
	u:db_setter(OidTo, "hp", (CurrHp - X)),
	{critical, X};
store_result(_OidTo, {fumble, 0}) ->
	{fumble, 0}.

%%===============================

init([]) ->
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	{ok, 0}.

handle_call({report, CidFrom, CidTo, Result}, _From, N) ->
	{reply, do_report(CidFrom, CidTo, Result), N+1};

handle_call(stop, _From, N) ->
	{stop, normal, stopped, N}.

handle_cast(_Msg, N) -> {noreply, N}.
handle_info(_Info, N) -> {noreply, N}.

terminate(_Reason, _N) ->
	io:format("~p stoppig~n", [?MODULE]).

code_change(_OldVsn, N, _Extra) -> {ok, N}.


%%===== TEST CODES ==========================

-ifdef(TEST).
store_result_ok_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = u:db_getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {ok, 1}),
	V2 = u:db_getter(Npcid1, "hp"),

	?assert(V1 - V2 == 1),
	?assert(R1 == {ok, 1}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_ok_999_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = u:db_getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {ok, 999}),
	V2 = u:db_getter(Npcid1, "hp"),

	?assert(V1 - V2 == 999),
	?assert(R1 == {ok, 999}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_ng_0_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = u:db_getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {ng, 0}),
	V2 = u:db_getter(Npcid1, "hp"),

	?assert(V1 - V2 == 0),
	?assert(R1 == {ng, 0}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_fumble_0_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = u:db_getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {fumble, 0}),
	V2 = u:db_getter(Npcid1, "hp"),

	?assert(V1 - V2 == 0),
	?assert(R1 == {fumble, 0}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_critical_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = u:db_getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {critical, 1}),
	V2 = u:db_getter(Npcid1, "hp"),

	?assert(V1 - V2 == 1),
	?assert(R1 == {critical, 1}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_critical_999_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = u:db_getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {critical, 999}),
	V2 = u:db_getter(Npcid1, "hp"),

	?assert(V1 - V2 == 999),
	?assert(R1 == {critical, 999}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

store_result_twice_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = u:db_getter(Npcid1, "hp"),
	R1 = store_result(Npcid1, {critical, 999}),
	V2 = u:db_getter(Npcid1, "hp"),

	?assert(V1 - V2 == 999),
	?assert(R1 == {critical, 999}),


	R2 = store_result(Npcid1, {ok, 1}),
	V3 = u:db_getter(Npcid1, "hp"),

	?assert(V2 - V3 == 1),
	?assert(R2 == {ok, 1}),

	?assert(V1 - V3 == 1000),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


notice_results_nil_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	{actions_and_stats, _, _} = mmoasp:get_list_to_know(self(), Cid1),

	L2 = notice_results(Npcid1, Cid1, [], 30),
	?assert(L2 == []),

	receive
		after 200 -> ok
	end,

	{actions_and_stats, Actions1, Stats1}
		= mmoasp:get_list_to_know(self(), Cid1),
	io:format("list to know for ~p: ~p~n",
		[Cid1, {actions_and_stats, Actions1, Stats1}]),
	AList1 = lists:flatten(
		[[{K, V} || {K, V} <- ST, K == from_cid] || ST <- Actions1]),
	?assert(sets:from_list(AList1)
		== sets:from_list([])),
	?assert(
		sets:from_list(lists:flatten(
			[[{K, V} || {K, V} <- ST, K == type] || ST <- Actions1]))
		== sets:from_list([])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

notice_results_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	{actions_and_stats, _, _} = mmoasp:get_list_to_know(self(), Cid1),

	L2 = notice_results(Npcid1, Cid1, [{ok, 999}], 30),
	?assert(L2 == [{ok, 999}]),

	receive
		after 200 -> ok
	end,

	{actions_and_stats, Actions1, Stats1}
		= mmoasp:get_list_to_know(self(), Cid1),
	io:format("list to know for ~p: ~p~n",
		[Cid1, {actions_and_stats, Actions1, Stats1}]),
	AList1 = lists:flatten(
		[[{K, V} || {K, V} <- ST, K == from_cid] || ST <- Actions1]),
	?assert(sets:from_list(AList1)
		== sets:from_list([{from_cid, "npc0001"}])),
	?assert(
		sets:from_list(lists:flatten(
			[[{K, V} || {K, V} <- ST, K == type] || ST <- Actions1]))
		== sets:from_list([{type, "attack"}])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

notice_results_2_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	{actions_and_stats, _, _} = mmoasp:get_list_to_know(self(), Cid1),

	L2 = notice_results(Npcid1, Cid1, [{ok, 7}, {ok, 13}], 30),
	?assert(L2 == [{ok, 7}, {ok, 13}]),

	receive
		after 200 -> ok
	end,

	{actions_and_stats, Actions1, Stats1}
		= mmoasp:get_list_to_know(self(), Cid1),
	io:format("list to know for ~p: ~p~n",
		[Cid1, {actions_and_stats, Actions1, Stats1}]),
	AList1 = lists:flatten(
		[[{K, V} || {K, V} <- ST, K == from_cid] || ST <- Actions1]),
	?assert(sets:from_list(AList1)
		== sets:from_list([{from_cid, "npc0001"}, {from_cid, "npc0001"}])),
	?assert(
		sets:from_list(lists:flatten(
			[[{K, V} || {K, V} <- ST, K == type] || ST <- Actions1]))
		== sets:from_list([{type, "attack"}, {type, "attack"}])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.
