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

-module(battle_observer).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("mmoasp.hrl").
-export([set_one/3, start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).


-ifdef(TEST).

battle_observer_01_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	V1 = u:db_getter(Npcid1, "hp"),
	battle_observer:set_one(Cid1, Npcid1, {ok, 2}),
	V2 = u:db_getter(Npcid1, "hp"),

	?assert(V1 - V2 == 2),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.

%%===============================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_one(CidFrom, CidTo, DamTupple) ->
	gen_server:call(?MODULE, {report, CidFrom, CidTo, DamTupple}, 20000).

stop() ->
	gen_server:call(?MODULE, stop).

%%===============================

do_report(CidFrom, CidTo, DamTupple) ->
	notice_result(
		CidFrom,
		CidTo,
		store_result(CidTo, DamTupple),
		_Radiuis = 30).

notice_result(OidFrom, OidTo, DamTupple, Radius) ->
	{Result, DamageVal} = DamTupple,
	Sessions = mmoasp:get_all_neighbor_sessions(OidTo, Radius),
	[X#session.pid ! {self(), attack, OidFrom, OidTo, Result, DamageVal}
		|| X <- Sessions],
	DamTupple.

%% store_result series returns {Result, Damage} tapple.
store_result(OidTo, {ok, X}) ->
	CurrHp = u:db_getter(OidTo, "hp"),
	u:db_setter(OidTo, "hp", (CurrHp - X)),
	{ok, X};
store_result(_OidTo, {ng, 0}) ->
	{ng, 0};
store_result(OidTo, {critical, X}) ->
	CurrHp = u:db_getter(OidTo, "hp"),
	u:db_setter(OidTo, "hp", (CurrHp - X)),
	{critical, X};
store_result(_OidTo, {fumble, 0}) ->
	{fumble, 0}.

%%===============================

init([]) ->
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	{ok, 0}.

handle_call({report, CidFrom, CidTo, Result}, _From, N) ->
	{reply, do_report(CidFrom, CidTo, Result), N+1};

handle_call(stop, _From, N) ->
	{stop, normal, stopped, N}.

handle_cast(_Msg, N) -> {noreply, N}.
handle_info(_Info, N) -> {noreply, N}.

terminate(_Reason, _N) ->
	io:format("~p stoppig~n", [?MODULE]).

code_change(_OldVsn, N, _Extra) -> {ok, N}.


