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

-include("mmoasp.hrl").
-import(lists, [foreach/2]).
%%-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile(export_all).


%-----------------------------------------------------------
%
% Utilities.
%
%-----------------------------------------------------------

%-----------------------------------------------------------
% use for Tid, Cid, ItemId...
%-----------------------------------------------------------
make_new_id() ->
	list_to_hexstr(erlang:binary_to_list(erlang:term_to_binary(erlang:make_ref()))).

% token: session credential for web i/f.
%-----------------------------------------------------------
gen_token() ->
	u:list_to_hexstr(erlang:binary_to_list(crypto:rand_bytes(16))).

gen_cid(Svid, Id) ->
	#cid{service_name = Svid, id = Id}.

local_cid(Cid) when is_record(Cid, cid) ->
	Cid#cid.id.

gen_location(Service, LocalMapId, X, Y) ->
	#location{
		map_id = gen_map_id(Service, LocalMapId),
		x = X,
		y = Y
	}.

gen_pos(Loc) when is_record(Loc, location) ->
	{pos, Loc#location.x, Loc#location.y}.

gen_map_id(Service, LocalMapId) ->
	#map_id{
		service_name = Service,
		id = LocalMapId
	}.

%-----------------------------------------------------------
% timer.
%-----------------------------------------------------------
wait(W) ->
	receive
		after W -> ok
	end.

%-----------------------------------------------------------
%% lists version of KV access function.
%-----------------------------------------------------------
kv_get(L, K) ->	%% this will cause error when K is not found in L.
 	case lists:keysearch(K, 1, L) of
		{value, {K,V}} -> V
	end.

kv_get(L, K, Default) ->
	case lists:keysearch(K, 1, L) of
		{value, {K,V}} -> V;
		false -> Default
	end.

kv_set(L, K, V) ->
	case lists:keymember(K, 1, L) of
		true -> lists:keyreplace(K,1,L, {K, V});
		false -> [{K,V}] ++ L
	end.

%-----------------------------------------------------------
%% dict version of KV access function.
%-----------------------------------------------------------
dkv_get(D, K) ->
	case dict:find(K, D) of
		{ok, V} -> V;
		error -> error({mmoasp_error,key_not_found})
	end.

dkv_set(D, K, V) ->
	dict:store(K, V, D).

%-----------------------------------------------------------
% make human readable hex notation.
%-----------------------------------------------------------

list_to_hexstr(A) -> list_to_hexstr(A, []).
list_to_hexstr([], Acc) -> lists:flatten(lists:reverse(Acc));
list_to_hexstr([H|T], Acc) -> list_to_hexstr(T, io_lib:format("~2.16.0b", [H]) ++ Acc).

%-----------------------------------------------------------
%% distance calcurator.
%-----------------------------------------------------------
distance({cid, C1}, {cid, C2}) ->
	distance(
		{online_character, online_character:get_one(C1)},
		{online_character, online_character:get_one(C2)});

distance({online_character, S1}, {online_character, S2})
	when S1#online_character.map_id == S2#online_character.map_id ->
	distance(S1#online_character.location, S2#online_character.location);  %% location contains {pos, X, Y}

distance({online_character, S1}, {online_character, S2}) ->
	infinity;

distance(L1, L2)
	when is_record(L1, location), is_record(L2, location),
		L1#location.map_id =:= L2#location.map_id ->
	distance({pos, L1#location.x, L1#location.y}, {pos, L2#location.x, L2#location.y});

distance({location, _MapId1, _X1, _Y1}, {location, _MapId2, _X2, _Y2}) ->
	infinity;

distance({mapxy, Map1, X1, Y1}, {mapxy, Map2, X2, Y2}) when Map1 =:= Map2 ->
	distance({pos, X1,Y1}, {pos, X2, Y2});

distance({mapxy, _MapId1, _X1, _Y1}, {mapxy, _MapId2, _X2, _Y2}) ->
	infinity;

%% test with offline character.
distance(_, offline) -> infinity;
distance(offline,_) -> infinity;

%% test with not initialized "online_character".
distance(_,undefined) -> infinity;
distance(undefined,_) -> infinity;


distance({pos, X1, Y1}, {pos, X2, Y2}) ->
	math:sqrt(math:pow((X1-X2),2) + math:pow((Y1-Y2),2)).


%-----------------------------------------------------------
%% cid_pair makes ordered cid_pair tuple.
%-----------------------------------------------------------

cid_pair(Cid1, Cid2) when Cid1 < Cid2 -> {cid_pair, Cid1, Cid2};
cid_pair(Cid1, Cid2) when Cid1 >= Cid2 -> {cid_pair, Cid2, Cid1}.

%-----------------------------------------------------------
%
% Tests.
%
%-----------------------------------------------------------

%% test kv and dkv.

-ifdef(TEST).

kv_get_1_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	Result = kv_get(L, "k1"),
	?assert(Result == "v1").

kv_get_0_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	?assertError({case_clause, false}, kv_get(L, "k3") == undefined).

kv_get_custom_default_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	Result = kv_get(L, "k3", customdefault),
	?assert(Result == customdefault).

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


%% TEST distance
-ifdef(TEST).

distance_l_offline_test() -> infinity = distance(offline, {pos, 3, 3}).
distance_r_offline_test() -> infinity = distance({pos, 3,3}, offline).
distance_1_1_test() -> 1.0 = distance({pos, 3, 3}, {pos, 2, 3}).
distance_1_2_test() -> 1.0 = distance({pos, 3, 3}, {pos, 3, 2}).
distance_1_3_test() -> 1.0 = distance({mapxy, "edo", 3, 3}, {mapxy, "edo", 3, 2}).
distance_1_4_test() -> infinity = distance({mapxy, "edo", 3, 3}, {mapxy, "kyoto", 3, 2}).

distance_1_online_character_test() ->
	S1 = #online_character{
		cid = #cid{service_name = hibari, id = 1},
		map_id = #map_id{service_name = hibari, id = 1},
		location= {pos, 3, 3}},
		
	S2 = #online_character{
		cid = #cid{service_name = hibari, id = 2},
		map_id = #map_id{service_name = hibari, id = 1},
		location= {pos, 3, 2}},
	
	?assert(1.0 == distance({online_character, S1}, {online_character, S2})).

distance_by_cid_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
%%	{connected, Cid1} = online_character:connect(hibari,1),
%%	{connected, Cid2} = online_character:connect(hibari,2),
	
	%% SET LOCATION!!!!!
	online_character:setpos(Cid1, #map_id{service_name=hibari, id=1}, {pos, 1, 1}),
	online_character:setpos(Cid2, #map_id{service_name=hibari, id=1}, {pos, 1, 2}),

	?assert(1.0 == distance(
		{cid, #cid{service_name = hibari, id = 1}},
		{cid, #cid{service_name = hibari, id = 2}}
		)),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}).
-endif.

%% TEST list_to_hexstr
-ifdef(TEST).
list_to_hexstr_two_test() -> "ff01" = list_to_hexstr([255,1]).
list_to_hexstr_one_test() -> "ff" = list_to_hexstr([255]).
list_to_hexstr_nil_test() -> "" = list_to_hexstr([]).
-endif.

%% TEST cid_pair
-ifdef(TEST).
cid_pair_lr_test() -> {cid_pair, "1", "2"} = cid_pair("1", "2").
cid_pair_rl_test() -> {cid_pair, "1", "2"} = cid_pair("2", "1").
cid_pair_eq_test() -> {cid_pair, "1", "1"} = cid_pair("1", "1").
cid_pair_nil_test() -> {cid_pair, nil, "2"} = cid_pair(nil, "2").
-endif.

-ifdef(TEST).
gen_cid_test() ->
	Cid = gen_cid(hibari, 1),
	?assert(Cid == {cid, hibari, 1}).
-endif.
