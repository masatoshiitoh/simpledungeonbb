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
-export([loop/2, mk_idle_reset/0]).
-compile(export_all).
-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").

% API: sends message to child
whoareyou(Cid) ->
	world:apply_session(Cid,
		fun(X) -> X#session.pid ! {self(), whoareyou} end).

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

system_call({From, stop_process}, R, I) ->
	io:format("character: proc stop by stop_process message.~n"),
	morningcall:cancel_all(R#task_env.utimer),
	From ! {ok, R#task_env.cid}.

loop(R, I)
	when I#idle.since_last_op > 300*1000*1000->
	
	io:format("character: time out.~n"),
	uauth:db_logout(self(), R#task_env.cid, R#task_env.token);

loop(R, I) ->
	receive
		{system, X} -> system_call(X, R, I});

		{From, request_list_to_know} ->
			From ! {list_to_know,
				get_elements(R#task_env.event_queue),
				get_stats(R#task_env.stat_dict)},
			loop(R#task_env{event_queue = queue:new()},mk_idle_reset());
		
		%% update neighbor characters' status.
		{_From, update_neighbor_status, Radius} ->
			NewStatDict =
				[gen_stat_from_cdata(X)
					|| X <- mmoasp:get_neighbor_char_cdata(R#task_env.cid, Radius)],
			loop(R#task_env{stat_dict = NewStatDict}, mk_idle_update(LastOp));
		
		{_From, talk, Talker, MessageBody, Mode} ->
			%%io:format("character: get chat. ~p~n",
			%%	[{talk, Talker, MessageBody, Mode}]),
			loop(add_event(R, 
					[{type, "talk"},
						{cid, Talker},
						{content, MessageBody},
						{mode, Mode}]),
				mk_idle_update(LastOp));

		{_From, attack, OidFrom, OidTo, Res, Dam} ->
			io:format("character: ~p hits ~p. (~p, ~p)~n",
				[OidFrom, OidTo, Res, Dam]),
			loop(add_event(R,
					[{type, "attack"},
						{from_cid, OidFrom},
						{to_cid, OidTo},
						{result, atom_to_list(Res)},
						{damage, Dam}]),
				mk_idle_update(LastOp));

		{_From, notice_login, SenderCid, Name} ->
			%%io:format("character: get others login. ~p~n",
			%%	[{notice_login, Name} ]),
			loop(add_event(R,
					[{type, "login"},
						{cid, SenderCid},
						{name, Name}]),
				mk_idle_update(LastOp));
			
		{_From, notice_logout, SenderCid} ->
			%%io:format("character: get others logout. ~p~n",
			%%	[{notice_logout, Cid} ]),
			loop(add_event(R,
					[{type, "logout"}, {cid, SenderCid}]),
				mk_idle_update(LastOp));
			
		{_From, notice_remove, SenderCid} ->
			%%io:format("character: get others removed. ~p~n",
			%%	[{notice_remove, Cid} ]),
			loop(add_event(R,
					[{type, "remove"}, {cid, SenderCid}]),
				mk_idle_update(LastOp));
			
		%% action him/herself.
		{_From, init_move, CurrPos, WayPoints} ->
			SelfPid = self(),
			SelfPid ! {_From, cancel_timer},
			SelfPid ! {_From, move, CurrPos, WayPoints},
			loop(R, mk_idle_reset());

		% interval-timer base character move:
		{_From, move, CurrPos, WayPoints} ->
			case WayPoints of
				[] -> 
					io:format("character: ~p arrived at: ~p~n",
						[R#task_env.cid, CurrPos]),
					db_setpos(R#task_env.cid, CurrPos),
					loop(R, mk_idle_update(LastOp));
				[H | T] -> 			
					io:format("character: ~p start move: ~p to ~p ~n",
						[R#task_env.cid, CurrPos, H]),

					db_setpos(R#task_env.cid, CurrPos),

					Radius = 100,
					mmoasp:notice_move(R#task_env.cid,
						{transition, CurrPos, H, 1000},
						Radius),
					SelfPid = self(),
					loop(R#task_env{
						utimer = morningcall:add(1000, fun() ->
							SelfPid ! {SelfPid, move, H, T}
							end, R#task_env.utimer)},
						mk_idle_update(LastOp))
			end;

		{_From, notice_move, SenderCid, From, To, Duration} ->
			%%io:format("character: get others move. ~p~n",
			%%	[{notice_move, SenderCid, From, To, Duration} ]),
			{pos, FromX, FromY} = From,
			{pos, ToX, ToY} = To,
			loop(add_event(R,
					[{type, "move"}, {cid, SenderCid},
						{from_x, FromX}, {from_y, FromY},
						{to_x, ToX}, {to_y, ToY},
						{duration, Duration}]),
				mk_idle_update(LastOp));

		%% Attribute setter
		{_From, set, Token, Key, Value} when Token == R#task_env.token ->
			NewCData = db_setter(R#task_env.cid, Key, Value),
			loop(R#task_env{cdata = NewCData}, mk_idle_reset());

		%% Attribute setter simple
		{set, Key, Value} ->
			NewCData = db_setter(R#task_env.cid, Key, Value),
			loop(R#task_env{cdata = NewCData}, mk_idle_reset());

		%% system messages.
		%% TIMER
		{goodmorning, Id} ->
			{_FunResult, NewUTimer} = morningcall:dispatch(Id, R#task_env.utimer),
			loop(R#task_env{utimer = NewUTimer}, mk_idle_update(LastOp));

		{_From, cancel_timer} ->
			NewUTimer = morningcall:cancel_all(R#task_env.utimer),
			loop(R#task_env{utimer = NewUTimer}, mk_idle_update(LastOp));

		%% Just a test code.
		{From, whoareyou} ->
			From ! {iam, R#task_env.cid},
			loop(R, mk_idle_update(LastOp));

		Other ->
			io:format("character: invalid message ~p~n", [Other]),
			loop(R, mk_idle_update(LastOp))
			
	after 1000 ->
		loop(R, mk_idle_update(LastOp))
	end.


% internal use -----------------------------------------------

add_event(R, Event) ->
	R#task_env{
		event_queue = add_element(Event, R#task_env.event_queue)
	}.

mk_idle_reset() -> I#idle.
mk_idle_update(I) -> I#idle{since_last_op = timer:now_diff(erlang:now(), LastOp)}.

add_element(X, Q) -> queue:in([{id, u:make_new_id()}] ++ X,Q).
get_elements(Q) -> queue:to_list(Q).
get_stats(L) -> L.

gen_stat_from_cdata(X) -> 
	[{cid, X#cdata.cid}, {name, X#cdata.name}] ++ X#cdata.attr.

db_setter(Cid, Key, Value) ->
	F = fun(X) ->
		Attr = X#cdata.attr,
		NewAttr = case lists:keymember(Key, 1, Attr) of
			true -> lists:keyreplace(Key,1,Attr, {Key, Value});
			false -> [{Key,Value}] ++ Attr
		end,
		NewCData = X#cdata{attr = NewAttr},
		mnesia:write(NewCData)
	end,
	world:apply_cdata(Cid, F).


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

