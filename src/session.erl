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

-module(session).
-include("mmoasp.hrl").
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile(export_all).

%----------------------------------------------------------------------
% session module: session handlers.
% 
% session?  a set of active cid, token, and expiration time.
% session table holds active sessions.
% who call me? : this module is called in mmoasp.erl mainly.
%----------------------------------------------------------------------

%---------------------------------
% access to session
%---------------------------------

check_and_call(Cid, Token, F) when is_record(Cid, cid) ->
	case lookup(Cid) of
		failed -> {failed, "session error"};
		S -> update(S),
			F()
	end.

add_one(ValidatedCid) when is_record(ValidatedCid, cid) ->
	Token = u:gen_token(),
	case mnesia:activity(transaction, fun() ->
		mnesia:write(#session{cid = ValidatedCid, token = Token, expire_at = make_expire()})
		end) of
		ok -> {ok, ValidatedCid, Token}
	end.

lookup(Svid, LocalCid) ->
	lookup(u:gen_cid(Svid, LocalCid)).

lookup(Cid) when is_record(Cid, cid) ->
	mnesia:activity(transaction, fun() ->
		case mnesia:read({session, Cid}) of
			[] -> failed;
			[A] -> A
		end
	end).

delete(S) when is_record(S, session) ->
	mnesia:activity(transaction, fun() ->
		mnesia:delete({session, S#session.cid})
	end).

%---------------------------------
%% update expireation limit
%---------------------------------
update(Svid, LocalCid) ->
	case lookup(Svid, LocalCid) of
		failed -> failed;
		S -> update(S)
	end.

update(S) when is_record(S, session) ->
	mnesia:activity(transaction, fun() ->
		mnesia:write(S#session{expire_at = make_expire()})
	end).

%---------------------------------
%% calculate expiration time.
%---------------------------------

make_expire() ->
	calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + default:expire_seconds().


%---------------------------------
%% test.
%---------------------------------

-ifdef(TEST).

add_new_session_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	{ok, {cid, hibari, 1}, _} = add_one(u:gen_cid(hibari, 1)),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

-endif.

