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


-module(mmoasp).

-export([start/0, start/1, stop/0]).
-export([do_login/3, setdown_player/1, auth_get_cid/1]).
-export([get_session/1, apply_initial_location/2, apply_session/2, apply_cdata/2]).
-export([send_message_by_cid/2]).
-export([gen_stat_from_cdata/1]).

-include("mmoasp.hrl").
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%-----------------------------------------------------------
%% implement simple api for mmoasp.erl
%-----------------------------------------------------------

start() ->
	battle_mgr:start_link(),
	db:start(reset_tables),
	notice_mgr:start_link(),
	path_finder:start().

start(reset_tables) ->
	battle_mgr:start_link(),
	db:start(reset_tables),
	notice_mgr:start_link(),
	path_finder:start().

stop() ->
	battle_mgr:stop(),
	path_finder:stop(),
	notice_mgr:stop(),
	db:stop().
	

-ifdef(TEST).
-endif.


get_player_character_template(Id, Pass) ->
	Cid = "c" ++ Id,
	Name = "name" ++ Id,
	[
		{auth_basic, Cid, Id, Pass},
		{cdata, Cid, Name, [{"align", "neutral"}]},
		{location, Cid, 1, {pos, 1,3}, offline, offline}
	].

do_change_password(Cid, From, Svid, Id, Pw, NewPw, Ipaddr) when Cid == void ->
	{ng, check_id_and_password};

do_change_password(Cid, From, Svid, Id, Pw, NewPw, Ipaddr) ->
	mnesia:transaction(fun() -> mn_rewrite_password(mnesia:read({auth_basic, Cid}), NewPw) end).

mn_rewrite_password(Accts, NewPw) when Accts == [] -> mnesia:abort(not_found);
mn_rewrite_password([One], NewPw) ->
	PasswordChanged = One#auth_basic{pass = NewPw},
	mnesia:write(PasswordChanged),
	ok.


do_login(Id, Pw, Cid) when Cid == void ->
	{ng, "authentication failed"};

do_login(Id, Pw, Cid) ->
	setup_player(Cid).

%-----------------------------------------------------------
% notice functions.
%-----------------------------------------------------------


send_message_by_cid(Cid, Message) ->
	apply_session(Cid, fun(X) -> X#session.pid ! Message end).

%-----------------------------------------------------------
% load and setup character for each login.
%-----------------------------------------------------------

setup_player(Cid) ->
	do_setup_player(Cid, get_session(Cid)).

do_setup_player(Cid, ExistingSession)
	when ExistingSession == {ng, "no such character"} ->

	R = setup_task_env(Cid),
	%% start player character process.
	Child = spawn(fun() -> character:loop(R, task:mk_idle_reset()) end),
	add_session(Cid, Child, "pc"),
	%% setup character states.
	map2d:setup_player_initial_location(Cid),
	%% notice login information to nearby.
	CData = lookup_cdata(Cid),
	notice_mgr:send_login(Cid, Cid, CData#cdata.name, map2d:default_distance()),
	{ok, Cid, R#task_env.token};

do_setup_player(Cid, ExistingSession) ->
	{ng, "account is in use"}.


setdown_player(Cid) ->
	do_setdown_player(Cid, get_session(Cid)).

do_setdown_player(Cid, ExistingSession)
	when ExistingSession == {ng, "no such character"} ->
		{ng, "no such character"};

do_setdown_player(Cid, ExistingSession) ->
	notice_mgr:send_logout(Cid, Cid, map2d:default_distance()),
	character:stop_child(Cid),
	stop_stream((get_session(Cid))#session.stream_pid),
	case delete_session(Cid) of
		{atomic, ok} -> {ok, Cid};
		Other -> Other
	end.


% *** charachter setup support functions. ***

add_session(Cid, Pid, Type) ->
	mnesia:transaction(
		fun() -> mnesia:write(#session{cid=Cid, pid=Pid, type=Type}) end).

delete_session(Cid) ->
	mnesia:transaction(
		fun()-> mnesia:delete({session, Cid}) end).

setup_task_env(Cid) ->
	#task_env{
		cid = Cid,
		event_queue = queue:new(),
		stat_dict = [],
		token = u:gen_token("nil", Cid),
		utimer =  morningcall:new()}.

%-----------------------------------------------------------
% Character Persistency
%-----------------------------------------------------------

lookup_cdata(Cid) ->
	case db:do(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid])) of
		[] -> void;
		[CData] -> CData
	end.

gen_stat_from_cdata(X) ->
	[{cid, X#cdata.cid}, {name, X#cdata.name}] ++ X#cdata.attr.


%-----------------------------------------------------------
% character data and session
%-----------------------------------------------------------
auth_get_cid({basic, Id, Pw}) ->
	case db:do(qlc:q([X#auth_basic.cid
		|| X <- mnesia:table(auth_basic),
			X#auth_basic.id =:= Id,
			X#auth_basic.pass =:= Pw])) of
		[] -> void;
		[X] -> X
	end.

get_session(Cid) ->
	u:mn_strip_atomic(apply_session(Cid, fun(X) -> X end)).

stop_stream(Pid) when is_pid(Pid) -> Pid ! {self(), stop};
stop_stream(_) -> void.

-ifdef(TEST).

get_session_online_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	S = get_session(Cid1),
	?assert(is_record(S, session)),
	?assert(S#session.cid == Cid1),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

get_session_offline_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	?assert({ng, "no such character"} == get_session("cid_not_exist")),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.


%-----------------------------------------------------------
% apply function to online characters
%-----------------------------------------------------------

%% F requires 1 arg (session record).
apply_session(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(session), X#session.cid == Cid]), F).

%% F requires 1 arg (cdata record).
apply_cdata(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid]), F).

%% F requires 1 arg (cdata record).
apply_location(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(session), X#session.cid == Cid]), F).

apply_initial_location(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(location), X#location.cid == Cid]), F).

apply_cid_indexed_table(Cond, F) ->
	L = fun() ->
		case qlc:e(Cond) of
			[] -> {ng, "no such character"};	% this style makes return value as {atomic, {ng,"no~"}}
			[Row] -> F(Row)
		end
	end,
	mnesia:transaction(L).




