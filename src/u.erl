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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("mmoasp.hrl").
-export([db_setter/3, make_new_id/0, distance/2, cid_pair/2, store_kvpairs/2, find_list_from_dict/2, add_new_member/2, list_to_hexstr/1]).

-ifdef(TEST).
kv_get_1_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	Result = kv_get(L, "k1"),
	?assert(Result == "v1").

kv_get_0_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	Result = kv_get(L, "k3"),
	?assert(Result == undefined).

kv_set_1_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	NewL = kv_set(L, "k1", "vnew"),
	Result = kv_get(NewL, "k1"),
	?assert(Result == "vnew").

kv_set_0_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	NewL = kv_set(L, "k3", "v3"),
	Result = kv_get(NewL, "k3"),
	?assert(Result == "v3").

-endif.

kv_get(L, K) ->
	case lists:keysearch(K, 1, L) of
		{value, {K,V}} -> V;
		false -> undefined
	end.

kv_set(L, K, V) ->
	case lists:keymember(K, 1, L) of
		true -> lists:keyreplace(K,1,L, {K, V});
		false -> [{K,V}] ++ L
	end.

db_getter(Npcid, Key) ->
	F = fun(X) ->
		Attr = X#cdata.attr,
		kv_get(Attr, Key)
	end,
	world:apply_cdata(Npcid, F).

db_setter(Cid, Key, Value) ->
	F = fun(X) ->
		mnesia:write(
			X#cdata{attr = kv_set(X#cdata.attr, Key, Value)}
		)
	end,
	world:apply_cdata(Cid, F).

% use for Tid, Cid, ItemId...
make_new_id() ->
	list_to_hexstr(erlang:binary_to_list(erlang:term_to_binary(erlang:make_ref()))).

list_to_hexstr(A) -> list_to_hexstr(A, []).
list_to_hexstr([], Acc) -> lists:flatten(lists:reverse(Acc));
list_to_hexstr([H|T], Acc) -> list_to_hexstr(T, io_lib:format("~2.16.0b", [H]) ++ Acc).

%% TEST list_to_hexstr
-ifdef(TEST).
list_to_hexstr_two_test() -> "ff01" = list_to_hexstr([255,1]).
list_to_hexstr_one_test() -> "ff" = list_to_hexstr([255]).
list_to_hexstr_nil_test() -> "" = list_to_hexstr([]).
-endif.

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
	math:sqrt(math:pow((X1-X2),2) + math:pow((Y1-Y2),2));

distance({oid,O1}, {oid,O2}) ->
	distance(
		{session, world:get_session(O1)},
		{session, world:get_session(O2)}).


%% TEST distance
-ifdef(TEST).

distance_l_offline_test() -> infinity = distance(offline, {pos, 3, 3}).
distance_r_offline_test() -> infinity = distance({pos, 3,3}, offline).
distance_1_1_test() -> 1.0 = distance({pos, 3, 3}, {pos, 2, 3}).
distance_1_2_test() -> 1.0 = distance({pos, 3, 3}, {pos, 3, 2}).
distance_1_3_test() -> 1.0 = distance({mapxy, "edo", 3, 3}, {mapxy, "edo", 3, 2}).
distance_1_4_test() -> infinity = distance({mapxy, "edo", 3, 3}, {mapxy, "kyoto", 3, 2}).

distance_1_sess_test() ->
	S1 = #session{map = "edo", x = 3, y = 3, z = 1},
	S2 = #session{map = "edo", x = 3, y = 2, z = 1},
	?assert(1.0 == distance({session, S1}, {session, S2})).

distance_by_oid_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	?assert(1.0 == distance({oid, Cid1}, {oid, Npcid1})),
	?assert(3.0 == distance({oid, Cid2}, {oid, Npcid1})),
	?assert(4.0 == distance({oid, Cid1}, {oid, Cid2})),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}).
-endif.



cid_pair(Cid1, Cid2) when Cid1 < Cid2 -> {cid_pair, Cid1, Cid2};
cid_pair(Cid1, Cid2) when Cid1 >= Cid2 -> {cid_pair, Cid2, Cid1}.

%% TEST cid_pair
-ifdef(TEST).
cid_pair_lr_test() -> {cid_pair, "1", "2"} = cid_pair("1", "2").
cid_pair_rl_test() -> {cid_pair, "1", "2"} = cid_pair("2", "1").
cid_pair_eq_test() -> {cid_pair, "1", "1"} = cid_pair("1", "1").
cid_pair_nil_test() -> {cid_pair, nil, "2"} = cid_pair(nil, "2").
-endif.


store_kvpairs([], Dict) -> Dict;

store_kvpairs(KVPairList, Dict) ->
	[{K,V}|T] = KVPairList,
	store_kvpairs(T, case V of
		[] -> dict:erase(K, V, Dict);
		V -> dict:store(K, V, Dict)
	end).


-ifdef(TEST).
	store_kvpairs_nil_test() ->
		D = dict:new(),
		D = store_kvpairs([], D).
	store_kvpairs_one_test() ->
		D = store_kvpairs([{"k1", "v1"}], dict:new()),
		{ok, "v1"} = dict:find("k1", D).
	store_kvpairs_two_test() ->
		D = store_kvpairs([{"k1", "v1"}, {"k2", "v2"}], dict:new()),
		{ok, "v2"} = dict:find("k2", D).
	store_kvpairs_overwrite_test() ->
		D = store_kvpairs([{"k1", "v1"}, {"k1", "vnew"}], dict:new()),
		{ok, "vnew"} = dict:find("k1", D).
-endif.


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


-ifdef(TEST).

find_list_from_dict_empty_test() ->
	[] = find_list_from_dict("k1", dict:new()).
find_list_from_dict_notfound_test() ->
	D = store_kvpairs([{"k1", "v1"}], dict:new()),
	[] = find_list_from_dict("k2", D).
find_list_from_dict_found_test() ->
	D = store_kvpairs([{"k1", "v1"}], dict:new()),
	"v1" = find_list_from_dict("k1", D).

-endif.


