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


-module(u).

-include_lib("mmoasp.hrl").
-export([make_new_id/0, distance/2, cid_pair/2, store_kvpairs/2, find_list_from_dict/2, add_new_member/2, list_to_hexstr/1]).

% use for Tid, Cid, ItemId...
make_new_id() ->
	list_to_hexstr(erlang:binary_to_list(erlang:term_to_binary(erlang:make_ref()))).

list_to_hexstr(A) -> list_to_hexstr(A, []).
list_to_hexstr([], Acc) -> lists:flatten(lists:reverse(Acc));
list_to_hexstr([H|T], Acc) -> list_to_hexstr(T, io_lib:format("~.16b", [H]) ++ Acc).

distance({session, S1}, {session, S2}) ->
	distance(
		{mapxy, S1#session.map, S1#session.x, S1#session.y},
		{mapxy, S2#session.map, S2#session.x, S2#session.y}
	);

distance({mapxy, Map1, X1, Y1}, {mapxy, Map2, X2, Y2}) when Map1 =:= Map2 ->
	distance({pos, X1,Y1}, {pos, X2, Y2});
distance({mapxy, _MapId1, _X1, _Y1}, {mapxy, _MapId2, _X2, _Y2}) ->
	infinity;
distance(_, offline) ->
	infinity;
distance(offline,_) ->
	infinity;
distance({pos, X1, Y1}, {pos, X2, Y2}) ->
	math:sqrt(math:pow((X1-X2),2) + math:pow((Y1-Y2),2)).


cid_pair(Cid1, Cid2) when Cid1 < Cid2 -> {cid_pair, Cid1, Cid2};
cid_pair(Cid1, Cid2) when Cid1 >= Cid2 -> {cid_pair, Cid2, Cid1}.


store_kvpairs([], Dict) -> Dict;

store_kvpairs(KVPairList, Dict) ->
	[{K,V}|T] = KVPairList,
	store_kvpairs(T, case V of
		[] -> dict:erase(K, V, Dict);
		V -> dict:store(K, V, Dict)
	end).

% First implementation: its too simple...
%store_kvpairs(KVPairList, Dict) ->
%	[{K,V}|T] = KVPairList,
%	store_kvpairs(T, dict:store(K,V, Dict)).


find_list_from_dict(Cid, Dict) ->
	case dict:find(Cid, Dict) of
		{ok, V} -> V;
		error -> []
		end.

add_new_member(NewMember, List) ->
	case lists:member(NewMember, List) of
		true -> List;
		false -> [NewMember] ++ List
		end.


