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


-module(map2d).

-export([setup_player_initial_location/1]).
-export([distance/2, default_distance/0]).
-export([get_neighbor_char_sessions/2, get_all_neighbor_sessions/2, get_neighbor_char_cdata/2]).
-export([setpos/2]).

-include("mmoasp.hrl").
-include("map2d.hrl").
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%-----------------------------------------------------------
% sessions by location.
%-----------------------------------------------------------

setup_player_initial_location(Cid) ->
	setpos(Cid, make_allpos(get_initial_location(Cid))).

get_initial_location(Cid) ->
	u:mn_strip_atomic(mmoasp:apply_initial_location(Cid, fun(X) -> X end)).

get_location(Cid) ->
	u:mn_strip_atomic(mmoasp:apply_location(Cid, fun(X) -> X end)).

make_allpos(A) when is_record(A, location) ->
	{allpos,
		A#location.initmap,
		A#location.initx,
		A#location.inity,
		A#location.initz}.

%% define notification range.
default_distance() -> 100.

get_all_neighbor_sessions(Cid, R) ->
	Me = mmoasp:get_session(Cid),
	F = fun() ->
		qlc:e(qlc:q([Sess || Sess <- mnesia:table(session),
			distance({session, Sess}, {session, Me}) =< R
			]))
	end,
	do_query_and_strip_result(F).

get_neighbor_char_sessions(Cid, R) ->
	Me = mmoasp:get_session(Cid),
	F = fun() ->
		qlc:e(qlc:q([Sess || Sess <- mnesia:table(session),
			distance({session, Sess}, {session,Me}) =< R,
			Sess#session.type == "pc"]))
	end,
	do_query_and_strip_result(F).

get_neighbor_char_cdata(Cid, R) ->
	Me = mmoasp:get_session(Cid),
	F = fun() ->
		qlc:e(qlc:q(
			[CData#cdata{ attr = CData#cdata.attr ++ [
					{"x", Sess#session.x},{"y", Sess#session.y},{"z", Sess#session.z},{"map", Sess#session.map}
				]}
				|| Sess <- mnesia:table(session),
				distance({session, Sess}, {session, Me}) =< R,
				CData <- mnesia:table(cdata),	
				CData#cdata.cid == Sess#session.cid]))
	end,
	do_query_and_strip_result(F).

do_query_and_strip_result(F) ->
	u:mn_strip_atomic( mnesia:transaction(F)).

%-----------------------------------------------------------
% character location updater
%-----------------------------------------------------------

setpos(Cid, {pos, PosX, PosY}) ->
	F = fun(X) ->
		mnesia:write(X#session{x = PosX, y = PosY})
	end,
	mmoasp:apply_session(Cid, F);

setpos(Cid, {allpos, Map, PosX, PosY, PosZ}) ->
	F = fun(X) ->
		mnesia:write(X#session{map = Map, x = PosX, y = PosY, z = PosZ})
	end,
	mmoasp:apply_session(Cid, F).


%% distance calcurator.
distance({cid,O1}, {cid,O2}) ->
	distance(
		{session, mmoasp:get_session(O1)},
		{session, mmoasp:get_session(O2)});

distance({session, {ng, _}}, _) ->
	infinity;

distance(_, {session, {ng, _}}) ->
	infinity;

distance({session, S1}, {session, S2}) when is_record(S1, session), is_record(S2, session) ->
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

distance_by_cid_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	?assert(1.0 == distance({cid, Cid1}, {cid, Npcid1})),
	?assert(3.0 == distance({cid, Cid2}, {cid, Npcid1})),
	?assert(4.0 == distance({cid, Cid1}, {cid, Cid2})),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}).
-endif.

