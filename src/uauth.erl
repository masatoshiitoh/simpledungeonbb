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


-module(uauth).
-compile(export_all).
-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").



db_subscribe(_From, Svid, Id, Pw, Ipaddr)->
	create_account(Svid, Id, Pw).

create_account(Svid, Id, Pw) ->
	db:add_single(Id, Pw).


db_change_password(_From, Svid, Id, Pw, NewPw, Ipaddr)->
	case get_cid({basic, Id, Pw}) of
		void -> {ng, check_id_and_password};
		Cid -> basic_change_password(Cid,NewPw)
	end.

basic_change_password(Cid, NewPw) ->
	mnesia:transaction(fun() ->
		case mnesia:read({auth_basic, Cid}) of
			[] ->mnesia:abort(not_found);
			[Acct] ->
				PasswordChanged = Acct#auth_basic{pass = NewPw},
				mnesia:write(PasswordChanged),
				ok
			end
		end).



setup_player_character(Cid)->
	{character, Cid, CData} = db_get_cdata(Cid),
	Token = uauth:gen_token("nil", Cid),
	UTimer = morningcall:new(),
	EventQueue = queue:new(),
	StatDict = [],
	db_location_online(Cid),
	Child = spawn(fun() ->character:loop(Cid, CData, EventQueue, StatDict, Token, UTimer, character:mk_idle_reset()) end),
	mnesia:transaction(fun() -> mnesia:write(#session{oid=Cid, pid=Child, type="pc"}) end),
	mnesia:transaction(fun() -> mnesia:write(#u_trade{cid=Cid, tid=void}) end),
	
	%%setup_player_location(Cid),
	setup_player_initial_location(Cid),

	Radius = 100,
	mmoasp:notice_login(Cid, {csummary, Cid, CData#cdata.name}, Radius),
	{ok, Child, Token}.

setup_player_initial_location(Cid) ->
	Me = world:get_location(Cid),
	Map = Me#location.initmap,
	X = Me#location.initx,
	Y = Me#location.inity,
	Z = Me#location.initz,
	character:db_setpos(Cid, {allpos, Map, X, Y, Z}).

setup_player_location(Cid) ->
	%% copy location data from location table to cdata attribute.
	Me = world:get_location(Cid),
	Map = Me#location.initmap,
	X = Me#location.initx,
	Y = Me#location.inity,
	mmoasp:setter(Cid, "map", Map),
	mmoasp:setter(Cid, "x", X),
	mmoasp:setter(Cid, "y", Y),
	
	mnesia:transaction(fun() ->
		[Pc] = mnesia:read({session, Cid}),
		mnesia:write(Pc#session{map = Map, x = X, y = Y}) end),
	
	{pos, X, Y}.

db_login(_From, Id, Pw, Ipaddr)->
	Loaded = load_character(Id,Pw),
	case Loaded of 
		{character, Oid, CData} ->
			P = db:do(qlc:q([X#session.oid||X<-mnesia:table(session), X#session.oid == Oid])),
			case P of
				[] ->
					% Not found.. Instanciate requested character !
					{ok, Pid, Token} = setup_player_character(Oid),
					{ok, Oid, Token};
				[Oid] ->
					% found.
					{ng, "character: account is in use"}
			end;
		void ->
			% Load failed.
			{ng, "character: authentication failed"}
		end.

db_get_cid(Id, Pw) ->
	Loaded = load_character(Id,Pw),
	case Loaded of 
		{character, Cid, CData} -> {character, Cid};
		void -> {ng, "character: authentication failed"}
		end.

db_get_cdata(Cid) ->
	{character, Cid, lookup_cdata(Cid)}.

% db_logout : this will be called by character process.
db_logout(_From, Cid, _Token) ->
	io:format("character:logout(~p)~n", [Cid]),
	Radius = 100,
	mmoasp:notice_logout(Cid, {csummary, Cid}, Radius),
	character:stop_child(Cid),
	mmoasp:cancel_trade(Cid),
	db_location_offline(Cid),
	stop_stream((world:get_session(Cid))#session.stream_pid),
	mnesia:transaction(fun()-> mnesia:delete({session, Cid})end).

stop_stream(Pid) when is_pid(Pid) -> Pid ! {self(), stop};
stop_stream(_) -> void.

db_location_online(Cid) ->
	F = fun() ->
		[CLoc] = mnesia:read({location, Cid}),
		[CSess] = mnesia:read({session, Cid}),
		mnesia:write(CSess#session{map = CLoc#location.initmap, x = CLoc#location.initx, y = CLoc#location.inity})
	end,
	mnesia:transaction(F).

db_location_offline(Cid) -> nop.

db_location_offline(Cid, Map, {pos, X, Y}) -> nop.



%=======================
% Character Persistency

load_character(Id,Pw) ->
	case get_cid({basic, Id, Pw}) of
		void -> void;
		Cid -> {character, Cid, lookup_cdata(Cid)}
	end.

save_character(Cid, CData) ->
	store_cdata(Cid, CData).

%=======================
get_cid({basic, Id, Pw}) ->
	case db:do(qlc:q([X#auth_basic.cid
		|| X <- mnesia:table(auth_basic),
			X#auth_basic.id =:= Id,
			X#auth_basic.pass =:= Pw])) of
		[] -> void;
		[X] -> X
	end.

%% lookup_cdata -> #cdata | void
lookup_cdata(Cid) ->
	case db:do(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid])) of
		[] -> void;
		[CData] -> CData
	end.

store_cdata(_Cid, CData) ->
	F = fun() ->
		mnesia:write(CData)
	end,
	mnesia:transaction(F).



%=======================
% Making token.

gen_token(_Ipaddr, _Cid) -> u:make_new_id().

%=======================
