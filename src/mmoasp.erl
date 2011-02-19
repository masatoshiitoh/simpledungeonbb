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


start() ->
	db:start(),
	db:reset_tables(),
	path_finder:start().

	
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
% > a = 1.
% > a = login(.....).  <- a is already bound!
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

%stop_all_characters(From) -> world:stop_all_characters(From).
%
%knock_all_characters(From) ->
%	world:knock_all_characters(From).	% knock_all is send 'whoareyou' to all character processes.


% caution !!
% General purpose setter.(use this for configuration store, or other misc operation.
% DO NOT USE for set gaming parameters(like hit point or money) from client.).

setter(From, Cid, Token, Key, Value) ->character:setter(From, Cid,Token, Key, Value).
setter(Cid, Key, Value) ->character:setter(Cid, Key, Value).

get_list_to_know(_From, Cid) ->
	
	% send message.
	F = fun(X) ->
		X#session.pid ! {self(), request_list_to_know}
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

get_all_neighbor_sessions(Oid, R) ->
	Me = world:get_session(Oid),
	
	F = fun() ->
		%%	Sess#session.oid =/= Oid,
		qlc:e(qlc:q([Sess || Sess <- mnesia:table(session),
			u:distance({session, Sess}, {session, Me}) < R
			]))
	end,
	case mnesia:transaction(F) of
		{atomic, Result} -> Result;
		Other -> Other
	end.

get_neighbor_char_sessions(Oid, R) ->
	Me = world:get_session(Oid),
	
	F = fun() ->
		%%	Sess#session.oid =/= Oid,
		qlc:e(qlc:q([Sess || Sess <- mnesia:table(session),
			u:distance({session, Sess}, {session,Me}) < R,
			Sess#session.type == "pc"]))
	end,
	case mnesia:transaction(F) of
		{atomic, Result} -> Result;
		Other -> Other
	end.

get_neighbor_char_cdata(Oid, R) ->
	Me = world:get_session(Oid),
	F = fun() ->
		qlc:e(qlc:q(
			[CData#cdata{ attr = CData#cdata.attr ++ [
					{"x", Sess#session.x},{"y", Sess#session.y},{"z", Sess#session.z},{"map", Sess#session.map}
				]}
				|| Sess <- mnesia:table(session),
				%%	Loc#location.cid =/= Cid,
				u:distance({session, Sess}, {session, Me}) < R,
				CData <- mnesia:table(cdata),	
				CData#cdata.cid == Sess#session.oid]))
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
		|| X <- get_all_neighbor_sessions(SenderCid, Radius)],
	{result, "ok"}.

notice_login(SenderCid, {csummary, Cid, Name}, Radius) ->
	[X#session.pid ! {self(), notice_login, SenderCid, Name}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

notice_logout(SenderCid, {csummary, Cid}, Radius) ->
	[X#session.pid ! {self(), notice_logout, SenderCid}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

notice_remove(SenderCid, {csummary, Cid}, Radius) ->
	[X#session.pid ! {self(), notice_remove, SenderCid}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

notice_move(SenderCid, {transition, From, To, Duration}, Radius) ->
	[X#session.pid ! {self(), notice_move, SenderCid, From, To, Duration}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

move(Cid, DestPos) ->
	F = fun(X) ->
		NowPos = {pos, X#session.x, X#session.y},
		{ok, Result} = path_finder:lookup_path(NowPos, DestPos),
		Result
	end,
	{atomic, WayPoints} = world:apply_session(Cid, F),
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


