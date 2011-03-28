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


-module(character).
-export([loop/2]).
-compile(export_all).
-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").

% API: sends message to child
whoareyou(Cid) ->
	world:apply_session(Cid,
		fun(X) -> X#session.pid ! {test, {self(), whoareyou}} end).

setter(From, Cid, Token, Key, Value) ->
	world:apply_session(Cid,
		fun(X) -> X#session.pid ! {From, set, Token, Key, Value} end).

setter(Cid, Key, Value) ->
	world:apply_session(Cid,
		fun(X) -> X#session.pid ! {set, Key, Value} end).

stop_child(Cid) ->
	world:apply_session(Cid,
		fun(X) -> X#session.pid ! {system, {self(), stop_process}} end).

% core loop -----------------------------------------------

% process user operation.
% uauth:db_login spawns this loop.

% this loop will expire 1800 second (just hard coded) after last operation.

% UTimer holds timer request.
% You can clear it with cancel_timer(), whenever you need.

loop(undefined, _) -> ok;	%% exit loop.

loop(R, I)
	when I#idle.since_last_op > 300*1000*1000->
	
	io:format("character: time out.~n"),
	uauth:db_logout(self(), R#task_env.cid, R#task_env.token);

loop(R, I) ->
	{NewR, NewI} = receive
		{test, X} -> task:test_call(X, R, I);
		{system, X} -> task:system_call(X, R, I);
		{timer, X} -> task:timer_call(X, R, I);
		{mapmove, X} -> task:mapmove_call(X,R,I);
		{sensor, X} -> task:sensor_call(X,R,I);

		
		%% update neighbor characters' status.
		{_From, update_neighbor_status, Radius} ->
			NewStatDict =
				[gen_stat_from_cdata(X)
					|| X <- mmoasp:get_neighbor_char_cdata(R#task_env.cid, Radius)],
			{R#task_env{stat_dict = NewStatDict}, task:mk_idle_update(I)};
		
		{_From, talk, Talker, MessageBody, Mode} ->
			%%io:format("character: get chat. ~p~n",
			%%	[{talk, Talker, MessageBody, Mode}]),
			{task:add_event(R, 
					[{type, "talk"},
						{cid, Talker},
						{content, MessageBody},
						{mode, Mode}]),
				task:mk_idle_update(I)};

		{_From, attack, OidFrom, OidTo, Res, Dam} ->
			io:format("character: ~p hits ~p. (~p, ~p)~n",
				[OidFrom, OidTo, Res, Dam]),
			{task:add_event(R,
					[{type, "attack"},
						{from_cid, OidFrom},
						{to_cid, OidTo},
						{result, atom_to_list(Res)},
						{damage, Dam}]),
				task:mk_idle_update(I)};

		%% Attribute setter
		{_From, set, Token, Key, Value} when Token == R#task_env.token ->
			NewCData = u:db_setter(R#task_env.cid, Key, Value),
			{R#task_env{cdata = NewCData}, task:mk_idle_reset()};

		%% Attribute setter simple
		{set, Key, Value} ->
			NewCData = u:db_setter(R#task_env.cid, Key, Value),
			{R#task_env{cdata = NewCData}, task:mk_idle_reset()}

	after 1000 ->
		{R, task:mk_idle_update(I)}
	end,
	loop(NewR, NewI).

% internal use -----------------------------------------------


gen_stat_from_cdata(X) -> 
	[{cid, X#cdata.cid}, {name, X#cdata.name}] ++ X#cdata.attr.


db_setpos(Cid, {pos, PosX, PosY}) ->
	F = fun(X) ->
		mnesia:write(X#session{x = PosX, y = PosY})
	end,
	world:apply_session(Cid, F);

db_setpos(Cid, {allpos, Map, PosX, PosY, PosZ}) ->
	F = fun(X) ->
		mnesia:write(X#session{map = Map, x = PosX, y = PosY, z = PosZ})
	end,
	world:apply_session(Cid, F).

