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

-endif.




dice(N,M) -> {dice, N*M, raw_dice(N, M)}.
	raw_dice(N,0) -> 0;
	raw_dice(N,M) -> random:uniform(N) + raw_dice(N, M-1).


dice(N) -> {dice, N, random:uniform(N)}.

result({dice, _Max, X}) when X == 1 -> {fumble, X};
result({dice,  Max, X}) when Max == X -> {critical, X};
result({dice, _Max, X}) -> {ok, X}. 



