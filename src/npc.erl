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


-module(npc).
-compile(export_all).

%-import(lists, [foreach/2]).

-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% for NPC implementation

start_npc(Npcid) ->
	case lookup_pid_by_npcid(Npcid) of
		void ->
			Pid = setup_npc(Npcid);
		FoundPid -> FoundPid
	end.

%% code set location in session is DUMMY. Update it later.
setup_npc(Npcid)->
	{cdata, Npcid, Name, Npcdata} = db_get_npcdata(Npcid),
	
	UTimer = morningcall:new(),
	EventQueue = queue:new(),
	StatDict = [],
	
	Child = spawn(fun() ->npc:loop(Npcid, Npcdata, EventQueue, StatDict, UTimer) end),
	
	%% store session
	mnesia:transaction(fun() -> mnesia:write(#session{oid=Npcid, pid=Child, type="npc", map=1, x=3,y=3}) end),

	Child.
	

loop(Npcid, Npcdata, EventQueue, StatDict, UTimer) ->
	receive
		{_From, talk, Talker, MessageBody, Mode} ->
			io:format("*** npc: get chat. ~p~n", [{talk, Talker, MessageBody, Mode}]),
			%% mmoasp:talk(open, Npcid, "konnichiwa--", 100),
			loop(Npcid, Npcdata, EventQueue, StatDict, UTimer);

		%% action him/herself.
		{_From, init_move, CurrPos, WayPoints} ->
			SelfPid = self(),
			SelfPid ! {_From, cancel_timer},
			SelfPid ! {_From, move, CurrPos, WayPoints},
			loop(Npcid, Npcdata, EventQueue, StatDict, UTimer);

		% interval-timer base character move:
		{_From, move, CurrPos, WayPoints} ->
			character:db_setpos(Npcid, CurrPos),
			case WayPoints of
				[] -> 
					io:format("npc: ~p arrived at: ~p~n", [Npcid, CurrPos]),
					{pos, X, Y} = CurrPos,
					mmoasp:setter(Npcid, "x", X),
					mmoasp:setter(Npcid, "y", Y),
					loop(Npcid, Npcdata, EventQueue, StatDict, UTimer);
				[H | T] -> 			
					io:format("npc: ~p start move: ~p to ~p ~n", [Npcid, CurrPos, H]),

					{pos, X, Y} = CurrPos,
					mmoasp:setter(Npcid, "x", X),
					mmoasp:setter(Npcid, "y", Y),

					Radius = 100,
					mmoasp:notice_move(Npcid, {transition, CurrPos, H, 1000}, Radius),
					SelfPid = self(),
					F = fun() ->
						SelfPid ! {SelfPid, move, H, T}
					end,
					loop(Npcid, Npcdata, EventQueue, StatDict, morningcall:add(1000,F, UTimer))
			end;

		{_From, notice_move, SenderCid, From, To, Duration} ->
			io:format("npc: get others move. ~p~n", [{notice_move, SenderCid, From, To, Duration} ]),
			{pos, FromX, FromY} = From,
			{pos, ToX, ToY} = To,
			loop(Npcid,Npcdata,
				character:add_element(
					[{type, "move"}, {cid, SenderCid},
						{from_x, FromX}, {from_y, FromY},
						{to_x, ToX}, {to_y, ToY},
						{duration, Duration}]
					, EventQueue),
				StatDict, UTimer);
			


		{From, stop_process} ->
			io:format("npcloop: child process terminated by stop_process message.~n"),
			morningcall:cancel_all(UTimer),
			From ! {ok, Npcid},
			bye;
		_ ->
			%% do nothing.
			loop(Npcid, Npcdata, EventQueue, StatDict, UTimer)
	end.

db_get_npcdata(Cid) ->
	case db:do(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid])) of
		[] -> void;
		[CData] -> CData
	end.

lookup_pid_by_npcid(Npcid) ->
	case db:do(qlc:q([X || X <- mnesia:table(session), X#session.oid == Npcid, X#session.type == "npc"])) of
		[] -> void;
		[X] -> X#session.pid
	end.

stop_npc(Npcid) ->
	Radius = 100,
	mmoasp:notice_remove(Npcid, {csummary, Npcid}, Radius),
	case lookup_pid_by_npcid(Npcid) of
		void -> void;
		FoundPid ->
			FoundPid ! {self(), stop_process},
			case mnesia:transaction(fun() ->
					mnesia:delete({session, Npcid})
					end) of
				{atomic,ok}->
					io:format("npcloop: stop_npc session entry clear succeeded.~n"),
					ok;
				AbortedWithReason ->
					io:format("npcloop: stop_npc session entry clear failed ~p.~n", [AbortedWithReason]),
					AbortedWithReason
			end
	end.



db_getter(Npcid, Key) ->
	F = fun(X) ->
		Attr = X#cdata.attr,
		case lists:keysearch(Key, 1, Attr) of
			{value, {Key,V}} -> V;
			false -> undefined
		end
	end,
	world:apply_cdata(Npcid, F).

