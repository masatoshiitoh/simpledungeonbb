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


-module(battle).

%% this module is battle interface.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("mmoasp.hrl").
-export([single/3, single/2]).

%% battle with method
%% (if not applicable (ex. too far to melee),
%% just only fails - get {ng,0}).
single(CidFrom, CidTo, Method) ->
	BattleResult = calc_single(CidFrom, CidTo, Method),
	io:format("single BattleResult ~p ~n", [BattleResult]),
	battle_observer:set_one(CidFrom, CidTo, BattleResult).



%% full automatic battle
single(CidFrom, CidTo) ->
	single(CidFrom, CidTo, get_default_battle_method(CidFrom)).

%%% not exported ------------------------------------------ %%%

calc_single(_CidFrom, _CidTo, Method) when Method == "hth" -> {ok, 0};
calc_single(_CidFrom, _CidTo, Method) when Method == "missile" -> {ok, 0};
calc_single(_CidFrom, _CidTo, Method) when Method == "magic" -> {ok, 0};
calc_single(CidFrom, CidTo, Method) ->
	%% any other method handler
	proc_battle(
		CidFrom, CidTo, Method,
		fun(X,Y) ->
			unarmed:calc(X,Y)
		end).


%% player choose attack method
%% hth: hand-to-hand. nuckle, katana...
%% missile: arrow, gun, rocket launcher...
%% magic: like tiltowait of wizardry,
%%        and any other special resource consuming.

proc_battle(CidFrom, CidTo, Method, CalcFunc) ->
	CalcFunc(
		get_battle_parameter(CidFrom, Method),
		get_battle_parameter(CidTo, Method)).

get_battle_parameter(Cid, Method) ->
	case Method of
		"unarmed" ->
			#battle_param{cid = Cid, range=1.0,
				hp = 10, mp = 0, ac = 3, str = 5}
	end.

get_default_battle_method(_Cid) ->
	%% check equipment.
	"unarmed".


%%% TEST CODE ------------------------------------------ %%%
-ifdef(TEST).

battle_01_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	%% check list head only.
	L1 = single(Cid1, Npcid1),
	[H1|_T1] = L1,
	{R1, _V1} = H1,
	?assert(R1 /= ng),

	L2 = single(Cid2, Npcid1),
	[H2|_T2] = L2,
	{R2, _V2} = H2,
	?assert(R2 == ng),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

battle_02_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	mmoasp:get_list_to_know(self(), Cid1),	%% reset list_to_know queue.

	%% 1: DO ATTACK
	L1 = single(Cid1, Npcid1),
	[H1|_T1] = L1,
	{R1, _V1} = H1,

	%% 2: Check results.
	?assert(R1 /= ng),
	
	%% 2-1: Get List to Know.
	{actions_and_stats, Actions1, _Stats1}
		= mmoasp:get_list_to_know(self(), Cid1),
	
	%% 2-1-1: Attacked Cid check: (check list head only.)
	AList1 = lists:flatten(
		[[{K, V} || {K, V} <- ST, K == to_cid] || ST <- Actions1]),
	io:format("battle_02_test:~p~n", [AList1]),
	[LH1 | _LT1] = AList1,
	?assert(sets:from_list([LH1]) == sets:from_list([{to_cid, Npcid1}])),

	%% 2-1-2: Type:
	AList2 = lists:flatten(
		[[{K, V} || {K, V} <- ST, K == type] || ST <- Actions1]),
	[LH2 | _LT2] = AList2,
	?assert(sets:from_list([LH2]) == sets:from_list([{type, "attack"}])),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.
