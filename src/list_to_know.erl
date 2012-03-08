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


-module(list_to_know).
-include("mmoasp.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile(export_all).

%	online_character:send_message_by_cid(Cid, {self(), update_neighbor_status, default:distance()}),
%	{list_to_know, ListToKnow, NeighborStats, MovePaths} = list_to_know:request(Cid)

get_one(Cid) when is_record(Cid, cid) ->
	online_character:send_message_by_cid(Cid, {sensor, {self(), request_list_to_know}}),
	receive
		{list_to_know, Actions, Stats, MovePaths} ->
			#list_to_know{
				actions = Actions,
				stats = Stats,
				move_paths = MovePaths}
		after 1000 -> {timeout, [], [], []}
	end.


-ifdef(TEST).

get_one_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{list_to_know, AL, SL, ML} = get_one(Cid1),
	[A1 | AT] = AL,
	
	% io:format("get_list_to_know_test:~p~n", [A1]),
	A1Type = u:kv_get(A1, type),
	A1Cid = u:kv_get(A1, cid),
	A1Name = u:kv_get(A1, name),
	?assert(A1Type == "login"),
	?assert(A1Cid == "1"),
	?assert(A1Name == "alpha"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


-endif.









