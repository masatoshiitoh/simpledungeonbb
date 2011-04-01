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


-module(db).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start/0, reset_tables/0]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-import(lists, [foreach/2]).
-include_lib("mmoasp.hrl").

%=======================
% KV access utilities
%=======================

access_cdata(Oid, Key) -> Value = 0.

%%% TEST CODE ------------------------------------------ %%%
-ifdef(TEST).

access_cdata_01_test() ->
	?assert(access_cdata("cid0001", "hp") == 12),
	{end_of_run_tests}.

-endif.


%=======================
% DB access layer.
%=======================

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

start() ->
	mnesia:start(),
	mnesia:wait_for_tables([service, auth_basic, id_next, cdata, trade, session, location, money, supplies, estate], 30000).

stop() ->
	%% mnesia:stop(),
	ok.

reset_tables() ->
	mnesia:clear_table(service),
	mnesia:clear_table(auth_basic),
	mnesia:clear_table(id_next),
	mnesia:clear_table(cdata),
	mnesia:clear_table(trade),
	mnesia:clear_table(money),
	mnesia:clear_table(supplies),
	mnesia:clear_table(estate),
	mnesia:clear_table(session),
	mnesia:clear_table(location),
	mnesia:transaction(fun() ->
			foreach(fun mnesia:write/1, example_tables())
		end).

drop_all() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]).

do_this_once() ->
	mnesia:create_schema([node()]),
	mnesia:start(),

	mnesia:create_table(service,	[{attributes, record_info(fields, service)}]),
	mnesia:create_table(admin_session,	[{attributes, record_info(fields, admin_session)}]),
	mnesia:create_table(auth_basic,	[{attributes, record_info(fields, auth_basic)}]),
	mnesia:create_table(id_next,	[{attributes, record_info(fields, id_next)}]),
	mnesia:create_table(cdata,		[{attributes, record_info(fields, cdata)}]),
	mnesia:create_table(session,	[{attributes, record_info(fields, session)}]),
	mnesia:create_table(money,		[{attributes, record_info(fields, money)}]),
	mnesia:create_table(supplies,	[{attributes, record_info(fields, supplies)}, {index, [cid, item_id]}]),
	mnesia:create_table(estate,		[{attributes, record_info(fields, estate)}, {index, [cid]}]),

	mnesia:create_table(location,	[{attributes, record_info(fields, location)}]),
	mnesia:create_table(trade,		[{attributes, record_info(fields, trade)}]),
	mnesia:create_table(u_trade,	[{attributes, record_info(fields, u_trade)}]),

	mnesia:stop().

%% *************************
%% following codes are only for developing use.
%% *************************

demo(session) ->
	do(qlc:q([X || X <- mnesia:table(session)]));

demo(select_auth) ->
	do(qlc:q([X || X <- mnesia:table(auth_basic)]));

demo(select_trade) ->
	do(qlc:q([X || X <- mnesia:table(trade)])).

demo(cdata, Cid) ->
	do(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid]));

demo(session, Cid) ->
	do(qlc:q([X || X <- mnesia:table(session), X#session.oid == Cid]));

demo(supplies, Cid) ->
	do(qlc:q([X || X <- mnesia:table(supplies), X#supplies.cid == Cid]));

demo(u_trade, Cid) ->
	do(qlc:q([X || X <- mnesia:table(u_trade), X#u_trade.cid == Cid]));

demo(location, Cid) ->
	do(qlc:q([X || X <- mnesia:table(location), X#location.cid == Cid]));

demo(inventory, Cid) ->
	F = fun() ->
		Money = qlc:e(qlc:q([X || X <- mnesia:table(money), X#money.cid == Cid])),
		Supplies = qlc:e(qlc:q([X || X <- mnesia:table(supplies), X#supplies.cid == Cid])),
		Estate = qlc:e(qlc:q([X || X <- mnesia:table(estate), X#estate.cid == Cid])),
	
		#inventory{cid = Cid, money = Money, supplies = Supplies, estate = Estate}
	end,
	mnesia:transaction(F).

add_single(Id, Pass) ->
	mnesia:transaction(fun() ->
			foreach(fun mnesia:write/1, get_single(Id, Pass))
		end).

get_single(Id, Pass) ->
	Cid = "c" ++ Id,
	Name = "name" ++ Id,
	[
		{auth_basic, Cid, Id, Pass},
		{cdata, Cid, Name, [{"align", "neutral"}]},
		{location, Cid, 1, {pos, 1,3}, offline, offline},
		{money, Cid, 2000, 0}
	].

example_tables() ->
	[
	%% service
	{service, "hibari", "s0001", "p0001", unlimited},

	%% npc data
	{auth_basic,"npc0001", void,void},
	{auth_basic,"npc0002", void,void},
	{auth_basic,"npc0003", void,void},
	{auth_basic,"npc0004", void,void},
	{cdata,"npc0001", "Slime", [{type, "npc"}, {"hp", 2}]},
	{cdata,"npc0002", "Slime", [{type, "npc"}, {"hp", 2}]},
	{cdata,"npc0003", "Slime", [{type, "npc"}, {"hp", 2}]},
	{cdata,"npc0004", "Slime", [{type, "npc"}, {"hp", 2}]},

	%% login
	{auth_basic,"cid0001","id0001",	"pw0001"},
	{auth_basic,"cid0002","id0002",	"pw0002"},
	{auth_basic,"cid0003","id0003",	"pw0003"},
	{auth_basic,"cid0004","id0004",	"pw0004"},
	%% cdata
	{cdata,"cid0001", "alpha",		[{type, "pc"}, {align, "good"}, {"hp", 12}]},
	{cdata,"cid0002", "bravo",		[{type, "pc"}, {align, "evil"}, {"hp", 16}]},
	{cdata,"cid0003", "charlie",	[{type, "pc"}, {align, "good"}, {"hp", 10}]},
	{cdata,"cid0004", "delta",		[{type, "pc"}, {align, "good"}, {"hp", 18}]},

	%% initial location (= re-spawn point)
	{location,"cid0001", 1, 1, 1, 0},
	{location,"cid0002", 1, 5, 1, 0},
	{location,"cid0003", 1, 6, 1, 0},
	{location,"cid0004", 1, 7, 1, 0},
	{location,"npc0001", 1, 2, 1, 0},
	
	%% inventory ( key colomn is cid.)
	{money, "cid0001", 1000, 15},
	{money, "cid0002", 2000, 0},

	{supplies, u:make_new_id(),"cid0001", item_herb, 10, 0},
	{supplies, u:make_new_id(), "cid0002", item_herb, 5, 0},
	{supplies, u:make_new_id(), "cid0002", item_portion, 15, 0},

	{estate, item_sword01, "cid0001", false},
	{estate, item_sword02, "cid0001", false},
	{estate, item_shield01, "cid0002", false}
	

	].

