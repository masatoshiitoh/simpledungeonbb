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


-module(test).
-compile(export_all).

-import(lists, [foreach/2]).

-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").


run_tests() ->
	mmoasp:start(),
	
	_NpcPid1 = npc:start_npc("npc0001"),
	io:format("after start npc ~p~n", [db:demo(session)]),
	
	%% login
	{ok, Cid1, Token1} = mmoasp:login(self(), "id0001", "pw0001", {192,168,1,200}),
	{ok, Cid2, Token2} = mmoasp:login(self(), "id0002", "pw0002", {192,168,1,201}),
	
	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),
	

	%% trade check.
	io:format("before :~n 1: ~p~n 2: ~p~n", [db:demo(inventory, Cid1),db:demo(inventory, Cid2)]),
	mmoasp:start_trade(Cid1, Cid2),
	io:format("trade started... ~p~n1: ~p~n2: ~p~n", [
		db:demo(select_trade),
		db:demo(u_trade, Cid1),
		db:demo(u_trade, Cid2)]),
	mmoasp:set_offer(Cid1, 112, [],[]),
	mmoasp:set_offer(Cid2, 0, [{item_herb, 2}],[item_shield01]),
	mmoasp:confirm_trade(Cid1),
	mmoasp:confirm_trade(Cid2),
	io:format("after :~n 1: ~p~n 2: ~p~n", [db:demo(inventory, Cid1),db:demo(inventory, Cid2)]),

	%% setter check.
	mmoasp:setter(self(), Cid1, Token1, "WindowSize", "123,55"),
	mmoasp:setter(self(), Cid2, Token2, "WindowSize", "99,160"),
	io:format("setter :~n 1: ~p~n 2: ~p~n", [db:demo(cdata, Cid1),db:demo(cdata, Cid2)]),
	
	%% talk check
	mmoasp:talk(open,Cid2, "hello all from cid 1234!!! ", 5),
	mmoasp:talk(open,Cid2, "will not appear this message!!! ", 1),
	mmoasp:talk(whisper,Cid2, Cid1, "hello cid 1 from cid 1234, with love :-)"),
	mmoasp:talk(whisper,Cid2, Cid1, "talk, line 2"),

	%% update neighbor stat.
	X = world:get_session(Cid1),
	X#session.pid ! {self(), update_neighbor_status, 10},
	X2 = world:get_session(Cid2),
	X2#session.pid ! {self(), update_neighbor_status, 10},
	
	{A1, S1} = mmoasp:get_list_to_know(self(), Cid1, Token1),
	io:format("list_to_json with ~p: ~p~n", [Cid1, mout:list_to_json(A1 ++ S1)]),
	{A2, S2} = mmoasp:get_list_to_know(self(), Cid2, Token2),
	io:format("list_to_json with ~p: ~p~n", [Cid2, mout:list_to_json(A2 ++ S2)]),

	%% state check
	io:format("state check :~n 1: ~p~n 2: ~p~n", [world:get_session(Cid1),world:get_session(Cid2)]),

	%% state check
	io:format("distance check for Cid1 - Cid2:~p~n",
		[u:distance((world:get_location(Cid1))#location.pos, (world:get_location(Cid2))#location.pos)]),
		
	io:format("get neighbor Cid1 (1) :~p~n",
		[mmoasp:get_neighbor_char_sessions(Cid1, 1)]),
	io:format("get neighbor Cid1 (2) :~p~n",
		[mmoasp:get_neighbor_char_sessions(Cid1, 2)]),
	
	%% moving !
	io:format("order move 1,3 to 3,3 ~p~n", [mmoasp:move(Cid1, {pos, 3,3})]),
	receive
		after 2500 -> ok
	end,
	io:format("RE-order move to 1,2 ~p~n", [mmoasp:move(Cid1, {pos, 1,2})]),
	receive
		after 5000 -> ok
	end,
	io:format("moved... :~n 1: ~p~n", [world:get_location(Cid1)]),
	
	%% logging out.
	mmoasp:logout(self(), Cid1, Token1),
	mmoasp:logout(self(), Cid2, Token2),

	npc:stop_npc("npc0001"),
	io:format("after stop npc ~p~n", [db:demo(session)]),

	%% stop service
	path_finder:stop(),
	
	{end_of_run_tests}.
