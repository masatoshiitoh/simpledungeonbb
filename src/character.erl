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
	mmoasp:apply_session(Cid,
		fun(X) -> X#session.pid ! {test, {self(), whoareyou}} end).

stop_child(Cid) ->
	mmoasp:apply_session(Cid,
		fun(X) -> X#session.pid ! {system, {self(), stop_process}} end).

% core loop -----------------------------------------------

% process user operation.
% mmoasp:db_login spawns this loop.

% this loop will expire 1800 second (just hard coded) after last operation.

% UTimer holds timer request.
% You can clear it with cancel_timer(), whenever you need.

loop(undefined, _) -> ok;	%% exit loop.

loop(R, I)
	when I#idle.since_last_op > 300*1000*1000->
	
	io:format("character: time out.~n"),
	mmoasp:logout(self(), R#task_env.cid, R#task_env.token);

loop(R, I) ->
	{NewR, NewI} = receive
		{test, X} -> task:test_call(X, R, I);
		{system, X} -> task:system_call(X, R, I);
		{timer, X} -> task:timer_call(X, R, I);
		{mapmove, X} -> move:mapmove_call(X,R,I);
		{sensor, X} -> task:sensor_call(X,R,I);
		{event, X} -> task:event_call(X,R,I);
		
		%% update neighbor characters' status.
		{_From, update_neighbor_status, Radius} ->
			NewStatDict =
				[mmoasp:gen_stat_from_cdata(X)
					|| X <- mmoasp:get_neighbor_char_cdata(R#task_env.cid, Radius)],
			{R#task_env{stat_dict = NewStatDict}, task:mk_idle_update(I)};
		
		{_From, talk, Talker, MessageBody, Mode} ->
			{task:add_event(R, 
					[{type, "talk"},
						{cid, Talker},
						{content, MessageBody},
						{mode, Mode}]),
				task:mk_idle_update(I)};

		{_From, attack, OidFrom, OidTo, Res, Dam} ->
			{task:add_event(R,
					[{type, "attack"},
						{cid, OidTo},
						{attacker, OidFrom},
						{result, atom_to_list(Res)},
						{damage, Dam}]),
				task:mk_idle_update(I)};

		%% Attribute setter
		{_From, set, Token, Key, Value} when Token == R#task_env.token ->
			mmoasp:setter(R#task_env.cid, Key, Value),
			{R, task:mk_idle_reset()};

		%% Attribute setter simple
		{set, Key, Value} ->
			mmoasp:setter(R#task_env.cid, Key, Value),
			{R, task:mk_idle_reset()}

	after 1000 ->
		{R, task:mk_idle_update(I)}
	end,
	loop(NewR, NewI).

