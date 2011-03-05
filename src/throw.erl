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


-module(throw).

%% this module is for throwing dice.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("mmoasp.hrl").
-compile(export_all).

-ifdef(TEST).

result_funble_01_test() ->	{funble, 1} == result({dice, 10, 1}).
result_funble_02_test() ->	{funble, 1} == result({dice, 2, 1}).
result_critical_01_test() -> {critical, 10} == result({dice, 10, 10}).
result_critical_02_test() -> {critical, 2} == result({dice, 2, 2}).
result_ok_01_test() ->	{ok, 3} == result({dice, 10, 3}).
result_ok_02_test() ->	{ok, 5} == result({dice, 10, 5}).

single_dice_01_10000_test() ->
	test:repeat(
		fun() ->
			{dice, 6, X} = dice(6),
			?assert(X >= 1 andalso X =< 6)
		end,
		10000).

multi_dice_01_10000_test() ->
	test:repeat(
		fun() ->
			{dice, 12, X} = dice(6,2),
			?assert(X >= 2 andalso X =< 12)
		end,
		10000).

multi_dice_02_10000_test() ->
	test:repeat(
		fun() ->
			{dice, 60, X} = dice(20,3),
			?assert(X >= 3 andalso X =< 60)
		end,
		10000).

-endif.

dice(N,M) -> {dice, N*M, raw_dice(N, M)}.
	raw_dice(N,0) -> 0;
	raw_dice(N,M) -> random:uniform(N) + raw_dice(N, M-1).

dice(N) -> {dice, N, random:uniform(N)}.

result({dice, _Max, X}) when X == 1 -> {fumble, X};
result({dice,  Max, X}) when Max == X -> {critical, X};
result({dice, _Max, X}) -> {ok, X}. 
