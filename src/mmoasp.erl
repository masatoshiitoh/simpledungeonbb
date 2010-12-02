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
-compile(export_all).

-import(lists, [foreach/2]).

-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").

%-----------------------------------------------------------
%% implement simple api for yaws_if.erl
%-----------------------------------------------------------

%% admnistration use.

get_service_entry(Svid, AdminId, AdminPw) ->
	case db:do(qlc:q([X
		|| X <- mnesia:table(service),
			X#service.svid =:= Svid,
			X#service.adm_id =:= AdminId,
			X#service.adm_pass =:= AdminPw])) of
		[] -> void;
		[X] -> X
	end.


admin_login(From, Svid, AdminId, AdminPw, Ipaddr) ->
	Loaded = get_service_entry(Svid, AdminId, AdminPw),
	case Loaded of 
		{service, Svid, AdminId, AdminPw, _Expire} ->
			Token = uauth:gen_token(Ipaddr, AdminId),
			Now = erlang:now(),
			mnesia:transaction(
				fun() ->
					mnesia:write(#admin_session{key=Token, svid=Svid, adm_id=AdminId, token=Token, last_op_time=Now})
					end),
			{ok, AdminId, Token};
		void ->
			{ng, "admin_login: authentication failed"}
		end.

is_ok_admin_session(AdminId, Token) ->
	Result = db:do(qlc:q([AdmSess || 
			AdmSess <- mnesia:table(admin_session),	
			AdmSess#admin_session.adm_id =:= AdminId,
			AdmSess#admin_session.token =:= Token
			])),
	case Result of
		[X] -> true;
		[] -> false
	end.


admin_delete_old_sessions(TimeoutSec) ->
	Now = erlang:now(),
	db:do(qlc:q([mnesia:delete({admin_session, AdmSess#admin_session.key}) || 
		AdmSess <- mnesia:table(admin_session),	
		timer:now_diff(Now, AdmSess#admin_session.last_op_time) > TimeoutSec * 1000000])).


admin_logout(From, Svid, AdminId, Token, Ipaddr) ->
	mnesia:transaction(fun() -> mnesia:delete({admin_session, Token}) end).


admin_list_users(From, Svid, AdminId, AdminToken, Ipaddr) ->
	case is_ok_admin_session(AdminId, AdminToken) of
		true -> 0;
		false -> {ng, session_timed_out}
	end.




start() ->
	db:start(),
	db:reset_tables(),
	path_finder:start().

run_tests() ->
	start(),
	
	%% login
	{ok, Cid1, Token1} = login(self(), "id0001", "pw0001", {192,168,1,200}),
	{ok, Cid2, Token2} = login(self(), "id0002", "pw0002", {192,168,1,201}),
	
	io:format("location of ~p: ~p~n", [Cid1, db:demo(location, Cid1)]),
	

	%% trade check.
	io:format("before :~n 1: ~p~n 2: ~p~n", [db:demo(inventory, Cid1),db:demo(inventory, Cid2)]),
	start_trade(Cid1, Cid2),
	io:format("trade started... ~p~n1: ~p~n2: ~p~n", [
		db:demo(select_trade),
		db:demo(u_trade, Cid1),
		db:demo(u_trade, Cid2)]),
	set_offer(Cid1, 112, [],[]),
	set_offer(Cid2, 0, [{item_herb, 2}],[item_shield01]),
	confirm_trade(Cid1),
	confirm_trade(Cid2),
	io:format("after :~n 1: ~p~n 2: ~p~n", [db:demo(inventory, Cid1),db:demo(inventory, Cid2)]),

	%% setter check.
	setter(self(), Cid1, Token1, "WindowSize", "123,55"),
	setter(self(), Cid2, Token2, "WindowSize", "99,160"),
	io:format("setter :~n 1: ~p~n 2: ~p~n", [db:demo(cdata, Cid1),db:demo(cdata, Cid2)]),
	
	%% talk check
	talk(open,Cid2, "hello all from cid 1234!!! ", 5),
	talk(open,Cid2, "will not appear this message!!! ", 1),
	talk(whisper,Cid2, Cid1, "hello cid 1 from cid 1234, with love :-)"),
	talk(whisper,Cid2, Cid1, "talk, line 2"),

	%% update neighbor stat.
	X = world:get_session(Cid1),
	X#session.pid ! {self(), update_neighbor_status, 10},
	X2 = world:get_session(Cid2),
	X2#session.pid ! {self(), update_neighbor_status, 10},
	
	{A1, S1} = get_list_to_know(self(), Cid1, Token1),
	io:format("list_to_json with ~p: ~p~n", [Cid1, mout:list_to_json(A1 ++ S1)]),
	{A2, S2} = get_list_to_know(self(), Cid2, Token2),
	io:format("list_to_json with ~p: ~p~n", [Cid2, mout:list_to_json(A2 ++ S2)]),

	%% state check
	io:format("state check :~n 1: ~p~n 2: ~p~n", [world:get_session(Cid1),world:get_session(Cid2)]),

	%% state check
	io:format("distance check for Cid1 - Cid2:~p~n",
		[u:distance((world:get_location(Cid1))#location.pos, (world:get_location(Cid2))#location.pos)]),
		
	io:format("get neighbor Cid1 (1) :~p~n",
		[get_neighbor_char_sessions(Cid1, 1)]),
	io:format("get neighbor Cid1 (2) :~p~n",
		[get_neighbor_char_sessions(Cid1, 2)]),
	
	%% moving !
	io:format("order move 1,3 to 3,3 ~p~n", [move(Cid1, {pos, 3,3})]),
	receive
		after 2500 -> ok
	end,
	io:format("RE-order move to 1,2 ~p~n", [move(Cid1, {pos, 1,2})]),
	receive
		after 5000 -> ok
	end,
	io:format("moved... :~n 1: ~p~n", [world:get_location(Cid1)]),
	
	%% logging out.
	logout(self(), Cid1, Token1),
	logout(self(), Cid2, Token2),
	path_finder:stop().
	
change_schema() ->
	db:drop_all(),
	db:do_this_once(),
	db:start(),
	db:reset_tables().



%% account registration.
subscribe(From, Svid, Id, Pw, Ipaddr) -> uauth:db_subscribe(From,Svid, Id, Pw, Ipaddr).


%% change password command for subscribers.

change_password(From, Svid, Id, Pw, NewPw, Ipaddr) ->
	case uauth:db_change_password(From,Svid, Id, Pw, NewPw, Ipaddr) of
		{atomic,ok} -> {ok};
		Other -> Other
	end.

% caution!
% mmoasp:login/4 cannot detect its failure before return value.
% for example,
% > a = 1,
% > a = login(.....)
%
% in such case, requested character will be set to on-line, but no one can handle it.
% Timeout mechanism will clear this situation.
%
login(From, Id, Pw, Ipaddr) ->	uauth:db_login(From, Id, Pw, Ipaddr).
logout(From, Cid, Token) ->
	case uauth:db_logout(From, Cid, Token) of
		{atomic, ok} -> {ok, Cid};
		Other -> Other
	end.

stop_all_characters(From) -> world:stop_all_characters(From).

knock_all_characters(From) ->
	world:knock_all_characters(From).	% knock_all is send 'whoareyou' to all character processes.


% caution !!
% General purpose setter.(use this for configuration store, or other misc operation.
% DO NOT USE for set gaming parameters(like hit point or money) from client.).

setter(From, Cid, Token, Key, Value) ->character:setter(From, Cid,Token, Key, Value).
setter(Cid, Key, Value) ->character:setter(Cid, Key, Value).

get_list_to_know(_From, Cid, Token) ->
	
	% send message.
	F = fun(X) ->
		X#session.pid ! {self(), request_list_to_know, Token}
	end,
	world:apply_session(Cid, F),
	
	% wait reply and receive.
	receive
		{list_to_know, Actions, Stats} -> {Actions, Stats}
		after 2000 -> {[], []}
	end.

	
%-----------------------------------------------------------
% Trade APIs.
%   Tid is cid_pair tuple.
% start_trade returns CidPair. use it for trade id (Tid).
%-----------------------------------------------------------
start_trade(Cid1,Cid2) -> trade:db_start_trade(Cid1, Cid2).
set_offer(Cid, Money, Supplies, Estates) -> trade:db_set_offer(Cid, Money, Supplies, Estates).
get_offer(Cid) -> trade:db_get_offer(Cid).
cancel_trade(Cid) -> trade:db_cancel_trade(Cid).
confirm_trade(Cid) -> trade:db_confirm_trade(Cid).

%-----------------------------------------------------------
% Talk APIs.
% chat mode: open/whisper/group
%-----------------------------------------------------------
talk_to(Pid, Sender, MessageBody, Mode) -> Pid ! {self(), talk, Sender, MessageBody, Mode}.

init_move(Pid, CurrentPos, Path) -> Pid ! {self(), init_move, CurrentPos, Path}.

get_neighbor_char_sessions(Cid, R) ->
	Me = world:get_location(Cid),
	F = fun() ->
		qlc:e(qlc:q([Sess || Loc <- mnesia:table(location),
		%%	Loc#location.cid =/= Cid,
			u:distance(Loc#location.pos, Me#location.pos) < R,
			Loc#location.map == Me#location.map,
			Sess <- mnesia:table(session),	
			Sess#session.cid =:= Loc#location.cid]))
	end,
	case mnesia:transaction(F) of
		{atomic, Result} -> Result;
		Other -> Other
	end.

get_neighbor_char_cdata(Cid, R) ->
	Me = world:get_location(Cid),
	F = fun() ->
		qlc:e(qlc:q([CData || Loc <- mnesia:table(location),
		%%	Loc#location.cid =/= Cid,
			u:distance(Loc#location.pos, Me#location.pos) < R,
			Loc#location.map == Me#location.map,
			CData <- mnesia:table(cdata),	
			CData#cdata.cid =:= Loc#location.cid]))
	end,
	case mnesia:transaction(F) of
		{atomic, Result} -> Result;
		Other -> Other
	end.


%select * from location, session where location.cid = session.cid


talk(whisper, SenderCid, ToCid, MessageBody) ->
	F = fun(X) ->
		talk_to(X#session.pid, SenderCid, MessageBody, "whisper")
	end,
	world:apply_session(ToCid, F);

talk(open, SenderCid, MessageBody, Radius) ->
	[talk_to(X#session.pid, SenderCid, MessageBody, "open")
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

notice_login(SenderCid, {csummary, Cid, Name}, Radius) ->
	[X#session.pid ! {self(), notice_login, SenderCid, Name}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

notice_logout(SenderCid, {csummary, Cid}, Radius) ->
	[X#session.pid ! {self(), notice_logout, SenderCid}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

notice_move(SenderCid, {transition, From, To, Duration}, Radius) ->
	[X#session.pid ! {self(), notice_move, SenderCid, From, To, Duration}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

move(Cid, DestPos) ->
	F = fun(X) ->
		{ok, Result} = path_finder:lookup_path(X#location.pos, DestPos),
		Result
	end,
	{atomic, WayPoints} = world:apply_location(Cid, F),
	case WayPoints of
		[] -> io:format("path unavailable.  no waypoints found.~n", []),
			[];
		[Start| Path] ->
			FS = fun(X) ->
				init_move(X#session.pid, Start, Path)
			end,
			world:apply_session(Cid, FS)
	end.

%% Following codes are under developing...

%talk(group, SenderCid, GroupId, MessageBody) ->
%	[talk_to(X#state.pid, SenderCid, MessageBody, open)
%		|| X <- get_group_char_states(GroupId)].

	
% 	lookup state table and get Pid.
% 	Send messagebody to Pid.


