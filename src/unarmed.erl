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


-module(unarmed).

%% this module is for Unarmed fight.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("mmoasp.hrl").
-compile(export_all).

%% do not throw dice.
fixed_calc(
	#battle_param{cid=OidFrom, range=Range, str=Str1} = _From,
	#battle_param{cid=OidTo, ac=Ac2} = _To)
	->
		Distance = mmoasp:distance({cid,OidFrom}, {cid,OidTo}),
		if
			(Distance > Range) -> {ng, 0};
			true -> {ok, trim_negative(Str1 + (Ac2 - 10))}
		end.

%% throw dice to calc result.
calc(
	#battle_param{cid=OidFrom, range=Range, str=_Str1} = From,
	#battle_param{cid=OidTo, ac=_Ac2} = To)
	->
		Distance = mmoasp:distance({cid,OidFrom}, {cid,OidTo}),
		if
			(Distance > Range) ->
				{ng, 0};
			true ->
				case throw:result(throw:dice(20)) of
					{fumble, _X}
						-> {fumble, 0};
					{critical, X}
						-> {critical, damage_critical(From, To, X)};
					{ok, X}
						-> {ok, damage_normal(From, To, X)}
				end
		end.

%% critical damage!
%% you got cancel enemy's armor,
%% ATK: double of Strength, + 20D1
damage_critical(
	#battle_param{str=Str1} = _From,
	#battle_param{ac=_Ac2} = _To,
	_Dice)
	->
		{dice, _Max, V} = throw:dice(20),
		trim_negative(Str1 * 2 + V).

%% normal damage.
%% ATK: strength was decreased by armor
%% (AC 10 : decrease 0, AC 0 : decrease 10...).
%% every ATK get random bonus (max: strength of attacker).
damage_normal(
	#battle_param{str=Str1} = _From,
	#battle_param{ac=Ac2} = _To,
	_Dice)
	->
		{dice, _Max, V} = throw:dice(Str1),
		trim_negative(Str1 + (Ac2 - 10) + V).

trim_negative(N) when N < 0 -> 0;
trim_negative(N) -> N.


%%% TEST CODE ------------------------------------------ %%%
-ifdef(TEST).
fixed_000_test() ->
	mmoasp:change_schema().

fixed_calc_01_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),


	From = #battle_param{cid=Cid1, str = 7, range=1.0},
	To = #battle_param{cid=Npcid1, ac = 10},
	?assert({ok, 7} == fixed_calc(From, To)),

	From2 = #battle_param{cid=Cid2, str = 7, range=1.0},
	To = #battle_param{cid=Npcid1, ac = 10},
	?assert({ng, 0} == fixed_calc(From2, To)),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

fixed_calc_02_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	From = #battle_param{cid=Cid1, str = 3, range=1.0},
	To = #battle_param{cid=Npcid1, ac = 6},
	?assert({ok, 0} == fixed_calc(From, To)),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

fixed_calc_03_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	From = #battle_param{cid=Cid1, str = 12, range=1.0},
	To = #battle_param{cid=Npcid1, ac = -10},
	?assert({ok, 0} == fixed_calc(From, To)),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

calc_01_1000_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	test:repeat(
		fun() ->
			From = #battle_param{cid=Cid1, str = 7, range=1.0},
			To = #battle_param{cid=Npcid1, ac = 10},
			case (calc(From, To)) of
				{ok, A}
					-> ?assert(A >= 7 andalso A =< 14);
				{fumble, B}
					-> ?assert(B == 0);
				{critical, C}
					-> ?assert(C >= 15 andalso C =< 35)
			end
		end,
		1000),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

calc_02_1000_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	test:repeat(
		fun() ->
			From = #battle_param{cid=Cid1, str = 5, range=1.0},
			To = #battle_param{cid=Npcid1, ac = 0},
			case (calc(From, To)) of
				{ok, A}
					-> ?assert(A == 0);
				{fumble, B}
					-> ?assert(B == 0);
				{critical, C}
					-> ?assert(C >= 11 andalso C =< 30)
			end
		end,
		1000),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


-endif.


