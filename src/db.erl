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

-export([start/0, start/1, stop/0]).
-export([change_schema/0, reset_tables/0]).
-export([do/1]).
-export([demo/1, demo/2]).	%% for test code.

-include_lib("stdlib/include/qlc.hrl").
-import(lists, [foreach/2]).
-include_lib("mmoasp.hrl").

%=======================
% KV access utilities
%=======================

access_cdata(Cid, Key) ->
	{atomic, Cdata} = mmoasp:apply_cdata(Cid, fun(X) -> X end),
	u:kv_get(Cdata#cdata.attr, Key).




%%% TEST CODE ------------------------------------------ %%%
-ifdef(TEST).

access_cdata_01_test() ->

	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	?assert(access_cdata("cid0001", "hp") == 12),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.


%=======================
% DB access layer.
%=======================

change_schema() ->
	drop_all(),
	do_this_once(),
	start(reset_tables).

start() ->
	mnesia:start(),
	mnesia:wait_for_tables([service, auth_basic, id_next, cdata, session, location], 30000).

start(reset_tables) ->
	mnesia:start(),
	mnesia:wait_for_tables([service, auth_basic, id_next, cdata, session, location], 30000),
	reset_tables().

stop() ->
	%mnesia:stop(),
	ok.

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

strip_transaction_result({atomic, ok}) -> ok;
strip_transaction_result(A) -> A.

reset_tables() ->
	mnesia:clear_table(service),
	mnesia:clear_table(auth_basic),
	mnesia:clear_table(id_next),
	mnesia:clear_table(cdata),
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
	mnesia:create_table(auth_basic,	[{attributes, record_info(fields, auth_basic)}]),
	mnesia:create_table(id_next,	[{attributes, record_info(fields, id_next)}]),
	mnesia:create_table(cdata,		[{attributes, record_info(fields, cdata)}]),
	mnesia:create_table(session,	[{attributes, record_info(fields, session)}]),

	mnesia:create_table(location,	[{attributes, record_info(fields, location)}]),

	mnesia:stop().

%% *************************
%% following codes are only for developing use.
%% *************************

demo(session) ->
	do(qlc:q([X || X <- mnesia:table(session)]));

demo(select_auth) ->
	do(qlc:q([X || X <- mnesia:table(auth_basic)])).

demo(cdata, Cid) ->
	do(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid]));

demo(session, Cid) ->
	do(qlc:q([X || X <- mnesia:table(session), X#session.cid == Cid]));

demo(location, Cid) ->
	do(qlc:q([X || X <- mnesia:table(location), X#location.cid == Cid])).

example_tables() ->
	[
	%% service
	{service, "hibari", "s0001", "p0001", unlimited},

	%% npc data
	{auth_basic,"npc0001", void,void},
	{auth_basic,"npc0002", void,void},
	{auth_basic,"npc0003", void,void},
	{auth_basic,"npc0004", void,void},
	{cdata,"npc0001", "Slime1", [{"type", "npc"}, {"hp", 2}]},
	{cdata,"npc0002", "Slime2", [{"type", "npc"}, {"hp", 2}]},
	{cdata,"npc0003", "Slime3", [{"type", "npc"}, {"hp", 2}]},
	{cdata,"npc0004", "Slime4", [{"type", "npc"}, {"hp", 2}]},

	%% login
	{auth_basic,"cid0001","id0001",	"pw0001"},
	{auth_basic,"cid0002","id0002",	"pw0002"},
	{auth_basic,"cid0003","id0003",	"pw0003"},
	{auth_basic,"cid0004","id0004",	"pw0004"},
	%% cdata
	{cdata,"cid0001", "alpha",		[{"type", "pc"}, {"align", "good"}, {"hp", 12}]},
	{cdata,"cid0002", "bravo",		[{"type", "pc"}, {"align", "evil"}, {"hp", 16}]},
	{cdata,"cid0003", "charlie",	[{"type", "pc"}, {"align", "good"}, {"hp", 10}]},
	{cdata,"cid0004", "delta",		[{"type", "pc"}, {"align", "good"}, {"hp", 18}]},

	%% initial location (= re-spawn point)
	{location,"cid0001", 1, 1, 1, 0},
	{location,"cid0002", 1, 5, 1, 0},
	{location,"cid0003", 1, 6, 1, 0},
	{location,"cid0004", 1, 7, 1, 0},
	{location,"npc0001", 1, 2, 1, 0},
	{location,"npc0002", 1, 4, 1, 0},
	{location,"npc0003", 1, 4, 4, 0},
	{location,"npc0004", 1, 5, 5, 0}
	
	].

