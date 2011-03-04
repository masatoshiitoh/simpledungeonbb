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

-ifdef(TEST).
fixed_calc_01_test() ->
	From = #battle_param{str = 7},
	To = #battle_param{ac = 10},
	?assert({ok, 7} == fixed_calc(From, To)).

fixed_calc_02_test() ->
	From = #battle_param{str = 3},
	To = #battle_param{ac = 6},
	?assert({ok, 0} == fixed_calc(From, To)).

fixed_calc_03_test() ->
	From = #battle_param{str = 12},
	To = #battle_param{ac = -10},
	?assert({ok, 0} == fixed_calc(From, To)).


calc_01_test() ->
	From = #battle_param{str = 7},
	To = #battle_param{ac = 10},
	case (calc(From, To)) of
		{ok, A}
			-> ?assert(A >= 7 andalso A =< 14);
		{fumble, B}
			-> ?assert(B == 0);
		{critical, C}
			-> ?assert(C >= 15 andalso C =< 35)
	end.

calc_01_10000_test() ->
	calc_01_single(10000).

calc_01_single(0) -> {end_of_test};
calc_01_single(X) ->
	calc_01_test(),
	calc_01_single(X - 1).


-endif.

fixed_calc(
	#battle_param{str=Str1} = From,
	#battle_param{ac=Ac2} = To)
	when (Str1 + (Ac2 - 10)) < 0
	-> {ok, 0};

fixed_calc(
	#battle_param{str=Str1} = From,
	#battle_param{ac=Ac2} = To)
	-> {ok, (Str1 + (Ac2 - 10))}.

calc(
	#battle_param{str=Str1} = From,
	#battle_param{ac=Ac2} = To)
	->
	case throw:result(throw:dice(20)) of
		{fumble, X}
			-> {fumble, 0};
		{critical, X}
			-> {critical, damage_critical(From, To, X)};
		{ok, X}
			-> {ok, damage_normal(From, To, X)}
	end.

%% critical damage!
%% you got cancel enemys armor,
%% ATK: double of Strength, + 20D1
damage_critical(
	#battle_param{str=Str1} = From,
	#battle_param{ac=Ac2} = To,
	Dice)
	->
		{dice, _Max, V} = throw:dice(20),
		(Str1 *2 + V).

%% normal damage.
%% ATK: strength was decreased by armor
%% (AC 10 : decrease 0, AC 0 : decrease 10...).
%% every ATK get random bonus (max: strength of attacker).
damage_normal(
	#battle_param{str=Str1} = From,
	#battle_param{ac=Ac2} = To,
	Dice)
	->
		{dice, _Max, V} = throw:dice(Str1),
		(Str1 + (Ac2 - 10) + V).
