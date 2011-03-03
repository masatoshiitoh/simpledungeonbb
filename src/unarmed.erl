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
calc_01_test() ->
	From = #battle_param{str = 7},
	To = #battle_param{ac = 10},
	?assert({ok, 7} == fixed_calc(From, To)).

calc_02_test() ->
	From = #battle_param{str = 3},
	To = #battle_param{ac = 6},
	?assert({ok, 0} == fixed_calc(From, To)).

calc_03_test() ->
	From = #battle_param{str = 7},
	To = #battle_param{ac = 10},
	case (calc(From, To)) of
		{ok, A}
			-> ?assert(A >= 7 andalso A =< 10.5);
		{fumble, B}
			-> ?assert(B == 0);
		{critical, C}
			-> ?assert(C >= 7 andalso C =< 10.5);
		{missed, D}
			-> ?assert(D == 0)
	end.
-endif.

fixed_calc(
	#battle_param{str=Str1} = From,
	#battle_param{ac=Ac2} = To) when (Str1 + (Ac2 - 10)) < 0
	-> {ok, 0};

fixed_calc(
	#battle_param{str=Str1} = From,
	#battle_param{ac=Ac2} = To)
	-> {ok, (Str1 + (Ac2 - 10))}.

calc(
	#battle_param{str=Str1} = From,
	#battle_param{ac=Ac2} = To)
	-> {ok, Str1 + (Ac2 - 10)}.

