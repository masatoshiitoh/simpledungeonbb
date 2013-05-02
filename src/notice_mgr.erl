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


-module(notice_mgr).
-behaviour(gen_server).

-include("mmoasp.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0, stop/0]).
-export([send_login/4, send_logout/3, send_remove/3, send_move/5, send_move_list/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% APIs
%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

send_login(SenderCid, Cid, Name, Radius) ->
	Msg = make_msg({login, Cid, Name}),
	send_to_neighbors(SenderCid, Msg, Radius).

send_logout(SenderCid, Cid, Radius) ->
	Msg = make_msg({logout, Cid}),
	send_to_neighbors(SenderCid, Msg, Radius).

send_remove(SenderCid, Cid, Radius) ->
	Msg = make_msg({remove, Cid}),
	send_to_neighbors(SenderCid, Msg, Radius).

send_move(SenderCid, From, To, Duration, Radius) ->
	Msg = make_msg({move, SenderCid, From, To, Duration}),
	send_to_neighbors(SenderCid, Msg, Radius).

send_move_list(SenderCid, TransitionList, Radius) ->
	Msg = make_msg({move_list, SenderCid, TransitionList}),
	send_to_neighbors(SenderCid, Msg, Radius).


%%
%% private functions for caller.
%%

make_msg({login, Cid, Name}) ->
	{sensor, {self(), notice_login, Cid, Name}};

make_msg({logout, Cid}) ->
	{sensor, {self(), notice_logout, Cid}};

make_msg({remove, Cid}) ->
	{sensor, {self(), notice_remove, Cid}};

make_msg({move, SenderCid, From, To, Duration}) -> 
	{mapmove, {self(), notice_move, SenderCid, From, To, Duration}};

make_msg({move_list, SenderCid, TransitionList}) ->
	{mapmove, {self(), notice_move_list, SenderCid, TransitionList}}.


%%
%% private functions for caller.
%%

send_to_neighbors(SenderCid, Msg, Radius) ->
	gen_server:call(?MODULE, {send_to_neighbors, SenderCid, Msg, Radius}).


%%
%% private functions for callee.
%%

send_message_to_neighbors(SenderCid, Message, Radius) ->
	[X#session.pid ! Message
		|| X <- map2d:get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.


%%
%% callbacks for gen_server behaviour.
%%

init([]) ->
	process_flag(trap_exit, true),
	{ok, 0}.

handle_call({send_to_neighbors, SenderCid, Msg, Radius}, _From, N) ->
	{reply, send_message_to_neighbors(SenderCid, Msg, Radius), N+1};

handle_call(stop, _From, N) ->
	{stop, normal, stopped, N}.

handle_cast(_Msg, N) ->
	{noreply, N}.

handle_info(_Info, N) ->
	{noreply, N}.

terminate(_Reason, _N) ->
	ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

