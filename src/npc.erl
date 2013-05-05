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
-export([start_npc/1, stop_npc/1, loop/2]).

-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% for NPC implementation

start_npc(Npcid) ->
	case lookup_pid_by_npcid(Npcid) of
		void ->
			_Pid = setup_npc(Npcid);
		FoundPid -> FoundPid
	end.

stop_npc(Npcid) ->
	case lookup_pid_by_npcid(Npcid) of
		void -> void;
		FoundPid ->
			remove_npc_from_db(Npcid),
			FoundPid ! {system, {self(), stop_process}}
	end.

remove_npc_from_db(Npcid) ->
	notice_mgr:send_remove(Npcid, Npcid, map2d:default_distance()),
	case mnesia:transaction(fun() ->
			mnesia:delete({session, Npcid})
			end) of
		{atomic,ok}->
			%%io:format("npcloop: stop_npc session entry clear succeeded.~n"),
			ok;
		AbortedWithReason ->
			io:format("npcloop: stop_npc session entry clear failed ~p.~n", [AbortedWithReason]),
			AbortedWithReason
	end.

setup_npc(Npcid)->
	R = #task_env{
		cid = Npcid,
		event_queue = queue:new(),
		stat_dict = [],
		utimer = morningcall:new()
	},
	Child = spawn(fun() -> npc:loop(R, task:mk_idle_reset()) end),
	%% store session
	mnesia:transaction(
		fun() ->
			[Loc1] = mnesia:read({location, Npcid}),
			mnesia:write(#session{cid=Npcid, pid=Child, type="npc", map=Loc1#location.initmap, x=Loc1#location.initx,y=Loc1#location.inity}
		) end), %% TEMPORARY IMPLEMENTATION!!

	Child.
	
loop(undefined, _) -> ok;	%% exit loop.

loop(R, I) ->
	{NewR, NewI} = receive
		{system, X} -> 
			%io:format("task:system_call ~p~n", [X]),
			task:system_call(X, R, I);
		{timer, X} ->
			%io:format("task:timer_call ~p~n", [X]),
			task:timer_call(X, R, I);
		{mapmove, X} ->
			%io:format("move_old:mapmove_call ~p~n", [X]),
			move_old:mapmove_call(X,R,I);
		{event, X} ->
			%io:format("check_killed ~p~n", [X]),
			check_killed(X, R, I);

		{_From, talk, Talker, MessageBody, Mode} ->
			%io:format("*** npc: get chat. ~p~n", [{talk, Talker, MessageBody, Mode}]),
			{R,I}
	end,
	loop(NewR, NewI).

%% NPC is killed !
check_killed({_From, event, _OidFrom, _OidTo, killed, KilledOid}, R, _I)
	when KilledOid == R#task_env.cid ->
	remove_npc_from_db(KilledOid),
	{undefined, undefined};

check_killed(_, R, I) -> 
	{R, I}.

%% dbtest() -> db:do(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == "npc0002"])).

db_get_npcdata(Cid) ->
	case db:do(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid])) of
		[] -> void;
		[CData] -> CData
	end.

lookup_pid_by_npcid(Npcid) ->
	case db:do(qlc:q([X || X <- mnesia:table(session), X#session.cid == Npcid, X#session.type == "npc"])) of
		[] -> void;
		[X] -> X#session.pid
	end.
