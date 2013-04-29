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
-include("mmoasp.hrl").
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



-compile(export_all).

%-----------------------------------------------------------
%% implement simple api for mmoasp.erl
%-----------------------------------------------------------

start() ->
	battle_observer:start_link(),
	db:start(reset_tables),
	path_finder:start().

start(reset_tables) ->
	battle_observer:start_link(),
	db:start(reset_tables),
	path_finder:start().

stop() ->
	battle_observer:stop(),
	path_finder:stop(),
	db:stop().
	
change_schema() ->
	db:drop_all(),
	db:do_this_once(),
	db:start(reset_tables).


-ifdef(TEST).
-endif.

%-----------------------------------------------------------
%% account management.
%-----------------------------------------------------------

delete_account(From, Svid, Id, Pw, Ipaddr) -> not_implemented.

create_account(From, Svid, Id, Pw, Ipaddr) ->
	mnesia:transaction(fun() ->
			foreach(fun mnesia:write/1, get_player_character_template(Id, Pw))
		end).

get_player_character_template(Id, Pass) ->
	Cid = "c" ++ Id,
	Name = "name" ++ Id,
	[
		{auth_basic, Cid, Id, Pass},
		{cdata, Cid, Name, [{"align", "neutral"}]},
		{location, Cid, 1, {pos, 1,3}, offline, offline}
	].

change_password(From, Svid, Id, Pw, NewPw, Ipaddr) ->
	do_change_password(auth_get_cid({basic, Id, Pw}), From, Svid, Id, Pw, NewPw, Ipaddr).

do_change_password(Cid, From, Svid, Id, Pw, NewPw, Ipaddr) when Cid == void ->
	{ng, check_id_and_password};

do_change_password(Cid, From, Svid, Id, Pw, NewPw, Ipaddr) ->
	mnesia:transaction(fun() -> mn_rewrite_password(mnesia:read({auth_basic, Cid}), NewPw) end).

mn_rewrite_password(Accts, NewPw) when Accts == [] -> mnesia:abort(not_found);
mn_rewrite_password([One], NewPw) ->
	PasswordChanged = One#auth_basic{pass = NewPw},
	mnesia:write(PasswordChanged),
	ok.

%-----------------------------------------------------------
%% authorization.
%-----------------------------------------------------------

% caution!
% login/4 cannot avoid failure caused by caller side problem.
% such as...
% > a = 1.
% > a = mmoasp:login(.....).  <- a is already bound!
%
% in such case, requested character will be set to on-line, but no one can handle it.
% Timeout mechanism will clear this situation.
%
login(From, Id, Pw, Ipaddr) ->
	do_login(Id, Pw, auth_get_cid({basic, Id, Pw})).

do_login(Id, Pw, Cid) when Cid == void ->
	{ng, "authentication failed"};

do_login(Id, Pw, Cid) ->
	setup_player(Cid).

logout(From, Cid, _Token) ->
	setdown_player(Cid).


-ifdef(TEST).

login_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{ok, Cid3, Token3}
		= login(self(), "id0003", "pw0003", {192,168,1,200}),
	logout(self(), Cid3, Token1),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

login_duplicated_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{ng, Reason} = login(self(), "id0001", "pw0001", {192,168,1,200}),
	?assert(Reason == "account is in use"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

logout_missing_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	logout(self(), "cid_not_exist", 0),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.


%-----------------------------------------------------------
%% LIST to KNOW sender
%-----------------------------------------------------------
get_list_to_know(_From, Cid) ->
	send_message_by_cid(Cid, {sensor, {self(), request_list_to_know}}),
	receive
		{list_to_know, Actions, Stats, MovePaths} -> {list_to_know, Actions, Stats, MovePaths}
		after 2000 -> {timeout, [], [], []}
	end.

-ifdef(TEST).

get_list_to_know_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{list_to_know, AL, SL, ML} = get_list_to_know(self(), Cid1),
	[A1 | AT] = AL,
	
	% io:format("get_list_to_know_test:~p~n", [A1]),
	A1Type = kv_get(A1, type),
	A1Cid = kv_get(A1, cid),
	A1Name = kv_get(A1, name),
	?assert(A1Type == "login"),
	?assert(A1Cid == "cid0001"),
	?assert(A1Name == "alpha"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


get_list_to_know_none_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{timeout, [], [], []} = get_list_to_know(self(), "cid_not_exist"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.

%-----------------------------------------------------------
% Talk APIs.
% chat mode: open/whisper/group
%-----------------------------------------------------------
talk_to(Pid, Sender, MessageBody, Mode) ->
	Pid ! {self(), talk, Sender, MessageBody, Mode}.

talk(whisper, SenderCid, ToCid, MessageBody) ->
	F = fun(X) ->
		talk_to(X#session.pid, SenderCid, MessageBody, "whisper")
	end,
	apply_session(ToCid, F);

talk(open, SenderCid, MessageBody, Radius) ->
	[talk_to(X#session.pid, SenderCid, MessageBody, "open")
		|| X <- get_all_neighbor_sessions(SenderCid, Radius)],
	{result, "ok"}.

%talk(group, SenderCid, GroupId, MessageBody) ->
%	[talk_to(X#state.pid, SenderCid, MessageBody, open)
%		|| X <- get_group_char_states(GroupId)].

%-----------------------------------------------------------
% notice functions.
%-----------------------------------------------------------

notice_login(SenderCid, {csummary, Cid, Name}, Radius) ->
	send_message_to_neighbors(
		SenderCid,
		{sensor, {self(), notice_login, Cid, Name}},
		Radius).

notice_logout(SenderCid, {csummary, Cid}, Radius) ->
	send_message_to_neighbors(
		SenderCid,
		{sensor, {self(), notice_logout, Cid}},
		Radius).

notice_remove(SenderCid, {csummary, Cid}, Radius) ->
	send_message_to_neighbors(
		SenderCid,
		{sensor, {self(), notice_remove, Cid}},
		Radius).

notice_move(SenderCid, {transition, From, To, Duration}, Radius) ->
	send_message_to_neighbors(
		SenderCid,
		{mapmove, {self(), notice_move, SenderCid, From, To, Duration}},
		Radius).

notice_move_list(SenderCid, {transition_list, L}, Radius) ->
	send_message_to_neighbors(
		SenderCid,
		{mapmove, {self(), notice_move_list, SenderCid, L}},
		Radius).

send_message_to_neighbors(SenderCid, Message, Radius) ->
	[X#session.pid ! Message
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

send_message_by_cid(Cid, Message) ->
	apply_session(Cid, fun(X) -> X#session.pid ! Message end).

%-----------------------------------------------------------
% load and setup character for each login.
%-----------------------------------------------------------

%% define notification range.
default_distance() -> 100.

setup_player(Cid) ->
	do_setup_player(Cid, get_session(Cid)).

do_setup_player(Cid, ExistingSession)
	when ExistingSession == {ng, "no such character"} ->

	R = setup_task_env(Cid),
	%% start player character process.
	Child = spawn(fun() -> character:loop(R, task:mk_idle_reset()) end),
	add_session(Cid, Child, "pc"),
	%% setup character states.
	setup_player_initial_location(Cid),
	%% notice login information to nearby.
	CData = lookup_cdata(Cid),
	notice_login(Cid, {csummary, Cid, CData#cdata.name}, default_distance()),
	{ok, Cid, R#task_env.token};

do_setup_player(Cid, ExistingSession) ->
	{ng, "account is in use"}.


setdown_player(Cid) ->
	do_setdown_player(Cid, get_session(Cid)).

do_setdown_player(Cid, ExistingSession)
	when ExistingSession == {ng, "no such character"} ->
		{ng, "no such character"};

do_setdown_player(Cid, ExistingSession) ->
	notice_logout(Cid, {csummary, Cid}, default_distance()),
	character:stop_child(Cid),
	stop_stream((get_session(Cid))#session.stream_pid),
	case delete_session(Cid) of
		{atomic, ok} -> {ok, Cid};
		Other -> Other
	end.


% *** charachter setup support functions. ***

make_allpos(A) when is_record(A, location) ->
	{allpos,
		A#location.initmap,
		A#location.initx,
		A#location.inity,
		A#location.initz}.

setup_player_initial_location(Cid) ->
	setpos(Cid, make_allpos(get_initial_location(Cid))).

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
		token = gen_token("nil", Cid),
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

% caution !!
% Following getter/2 and setter/2 are dangerous to open to web interfaces.
% DO NOT OPEN them as web i/f to set gaming parameters
% (like hit point) from remote.
getter(Cid, Key) ->
	F = fun() ->
		case mnesia:read({cdata, Cid}) of
			[] -> undefined;	%% no match
			[D] -> kv_get(D#cdata.attr, Key)
		end
	end,
	mn_strip_atomic(mnesia:transaction(F)).

setter(Cid, Key, Value) ->
	F = fun(X) ->
		mnesia:write(X#cdata{attr = kv_set(X#cdata.attr, Key, Value)})
	end,
	apply_cdata(Cid, F).

-ifdef(TEST).

db_get_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	V1 = getter(Cid1, "hp"),
	?assert(V1 == 12),
	
	V2 = getter(Cid2, "hp"),
	?assert(V2 == 16),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.
	
db_set_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	setter(Cid1, "hp", 2),
	V1 = getter(Cid1, "hp"),
	?assert(V1 == 2),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.


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
	mn_strip_atomic(apply_session(Cid, fun(X) -> X end)).

get_location(Cid) ->
	mn_strip_atomic(apply_location(Cid, fun(X) -> X end)).

get_initial_location(Cid) ->
	mn_strip_atomic(apply_initial_location(Cid, fun(X) -> X end)).

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
% sessions by location.
%-----------------------------------------------------------

get_all_neighbor_sessions(Cid, R) ->
	Me = get_session(Cid),
	F = fun() ->
		qlc:e(qlc:q([Sess || Sess <- mnesia:table(session),
			distance({session, Sess}, {session, Me}) =< R
			]))
	end,
	do_query_and_strip_result(F).

get_neighbor_char_sessions(Cid, R) ->
	Me = get_session(Cid),
	F = fun() ->
		qlc:e(qlc:q([Sess || Sess <- mnesia:table(session),
			distance({session, Sess}, {session,Me}) =< R,
			Sess#session.type == "pc"]))
	end,
	do_query_and_strip_result(F).

get_neighbor_char_cdata(Cid, R) ->
	Me = get_session(Cid),
	F = fun() ->
		qlc:e(qlc:q(
			[CData#cdata{ attr = CData#cdata.attr ++ [
					{"x", Sess#session.x},{"y", Sess#session.y},{"z", Sess#session.z},{"map", Sess#session.map}
				]}
				|| Sess <- mnesia:table(session),
				distance({session, Sess}, {session, Me}) =< R,
				CData <- mnesia:table(cdata),	
				CData#cdata.cid == Sess#session.cid]))
	end,
	do_query_and_strip_result(F).

do_query_and_strip_result(F) ->
	mn_strip_atomic( mnesia:transaction(F)).

mn_strip_atomic({atomic, Result}) -> Result;
mn_strip_atomic(Other) -> Other.

%-----------------------------------------------------------
% character location updater
%-----------------------------------------------------------

setpos(Cid, {pos, PosX, PosY}) ->
	F = fun(X) ->
		mnesia:write(X#session{x = PosX, y = PosY})
	end,
	apply_session(Cid, F);

setpos(Cid, {allpos, Map, PosX, PosY, PosZ}) ->
	F = fun(X) ->
		mnesia:write(X#session{map = Map, x = PosX, y = PosY, z = PosZ})
	end,
	apply_session(Cid, F).

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



%-----------------------------------------------------------
%
% Utilities.
%
%-----------------------------------------------------------

% token: session credential for web i/f.
% TODO: record log Ipaddr and Cid.
gen_token(_Ipaddr, _Cid) -> make_new_id().

% use for Tid, Cid, ItemId...
make_new_id() ->
	list_to_hexstr(erlang:binary_to_list(erlang:term_to_binary(erlang:make_ref()))).

wait(W) ->
	receive
		after W -> ok
	end.

kv_get(L, K) ->
	case lists:keysearch(K, 1, L) of
		{value, {K,V}} -> V;
		false -> undefined
	end.

kv_set(L, K, V) ->
	case lists:keymember(K, 1, L) of
		true -> lists:keyreplace(K,1,L, {K, V});
		false -> [{K,V}] ++ L
	end.


-ifdef(TEST).

kv_get_1_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	Result = kv_get(L, "k1"),
	?assert(Result == "v1").

kv_get_0_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	Result = kv_get(L, "k3"),
	?assert(Result == undefined).

kv_set_1_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	NewL = kv_set(L, "k1", "vnew"),
	Result = kv_get(NewL, "k1"),
	?assert(Result == "vnew").

kv_set_0_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	NewL = kv_set(L, "k3", "v3"),
	Result = kv_get(NewL, "k3"),
	?assert(Result == "v3").

-endif.

list_to_hexstr(A) -> list_to_hexstr(A, []).
list_to_hexstr([], Acc) -> lists:flatten(lists:reverse(Acc));
list_to_hexstr([H|T], Acc) -> list_to_hexstr(T, io_lib:format("~2.16.0b", [H]) ++ Acc).

%% TEST list_to_hexstr
-ifdef(TEST).
list_to_hexstr_two_test() -> "ff01" = list_to_hexstr([255,1]).
list_to_hexstr_one_test() -> "ff" = list_to_hexstr([255]).
list_to_hexstr_nil_test() -> "" = list_to_hexstr([]).
-endif.

%% distance calcurator.
distance({cid,O1}, {cid,O2}) ->
	distance(
		{session, get_session(O1)},
		{session, get_session(O2)});

distance({session, {ng, _}}, _) ->
	infinity;

distance(_, {session, {ng, _}}) ->
	infinity;

distance({session, S1}, {session, S2}) when is_record(S1, session), is_record(S2, session) ->
	distance(
		{mapxy, S1#session.map, S1#session.x, S1#session.y},
		{mapxy, S2#session.map, S2#session.x, S2#session.y}
	);

distance({mapxy, Map1, X1, Y1}, {mapxy, Map2, X2, Y2}) when Map1 =:= Map2 ->
	distance({pos, X1,Y1}, {pos, X2, Y2});

distance({mapxy, _MapId1, _X1, _Y1}, {mapxy, _MapId2, _X2, _Y2}) ->
	infinity;

distance(_, offline) ->
	infinity;

distance(offline,_) ->
	infinity;

distance({pos, X1, Y1}, {pos, X2, Y2}) ->
	math:sqrt(math:pow((X1-X2),2) + math:pow((Y1-Y2),2)).

%% TEST distance
-ifdef(TEST).

distance_l_offline_test() -> infinity = distance(offline, {pos, 3, 3}).
distance_r_offline_test() -> infinity = distance({pos, 3,3}, offline).
distance_1_1_test() -> 1.0 = distance({pos, 3, 3}, {pos, 2, 3}).
distance_1_2_test() -> 1.0 = distance({pos, 3, 3}, {pos, 3, 2}).
distance_1_3_test() -> 1.0 = distance({mapxy, "edo", 3, 3}, {mapxy, "edo", 3, 2}).
distance_1_4_test() -> infinity = distance({mapxy, "edo", 3, 3}, {mapxy, "kyoto", 3, 2}).

distance_1_sess_test() ->
	S1 = #session{map = "edo", x = 3, y = 3, z = 1},
	S2 = #session{map = "edo", x = 3, y = 2, z = 1},
	?assert(1.0 == distance({session, S1}, {session, S2})).

distance_by_cid_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	?assert(1.0 == distance({cid, Cid1}, {cid, Npcid1})),
	?assert(3.0 == distance({cid, Cid2}, {cid, Npcid1})),
	?assert(4.0 == distance({cid, Cid1}, {cid, Cid2})),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}).
-endif.


%% cid_pair makes ordered cid_pair tuple.

cid_pair(Cid1, Cid2) when Cid1 < Cid2 -> {cid_pair, Cid1, Cid2};
cid_pair(Cid1, Cid2) when Cid1 >= Cid2 -> {cid_pair, Cid2, Cid1}.

%% TEST cid_pair
-ifdef(TEST).
cid_pair_lr_test() -> {cid_pair, "1", "2"} = cid_pair("1", "2").
cid_pair_rl_test() -> {cid_pair, "1", "2"} = cid_pair("2", "1").
cid_pair_eq_test() -> {cid_pair, "1", "1"} = cid_pair("1", "1").
cid_pair_nil_test() -> {cid_pair, nil, "2"} = cid_pair(nil, "2").
-endif.



