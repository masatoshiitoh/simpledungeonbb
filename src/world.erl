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


-module(world).
-compile(export_all).

-import(lists, [foreach/2]).
-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").

%stop_all_characters(_From) ->
%	db:do(qlc:q([mmoasp:logout(self(), Cid, Token)
%		|| {state, Cid, _Pid, _Ipaddr, Token} <- mnesia:table(state)])).

%knock_all_characters(From) ->
%	db:do(qlc:q([Pid ! {From, whoareyou}
%		|| {state, _Cid, Pid, _Ipaddr, _Token} <- mnesia:table(state)])).

get_session(Cid) ->
	case apply_session(Cid, fun(X) -> X end) of
		{ng, _} -> void;
		{atomic, Result} -> Result
	end.

get_location(Cid) ->
	case apply_location(Cid, fun(X) -> X end) of
		{ng, _} -> void;
		{atomic, Result} -> Result
	end.


%% F requires 1 arg (session record).
apply_session(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(session), X#session.oid == Cid]), F).

apply_pc(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(session), X#session.oid == Cid, X#session.type == "pc"]), F).

apply_npc(Oid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(session), X#session.oid == Oid, X#session.type == "npc"]), F).

%% F requires 1 arg (cdata record).
apply_cdata(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid]), F).
%% F requires 1 arg (cdata record).
apply_location(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(location), X#location.cid == Cid]), F).

apply_cid_indexed_table(Cond, F) ->
	L = fun() ->
		case qlc:e(Cond) of
			[] -> {ng, "no such character"};	% this style makes return value as {atomic, {ng,"no~"}}
			[R] -> F(R)
		end
	end,
	mnesia:transaction(L).


