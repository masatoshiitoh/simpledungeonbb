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


-module(char_kv).
-include("mmoasp.hrl").
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile(export_all).


% caution !!
% Following getter/2 and setter/2 are dangerous to open to web interfaces.
% DO NOT OPEN them as web i/f to set gaming parameters without protect mechanism.
% unless, it will cause cheat storm (rewrite hit point or money ..) from remote.

getter(Svid, Id, Key) ->
	getter(u:gen_cid(Svid, Id), Key).

getter(Cid, Key) ->
	F = fun() ->
		case mnesia:read({character, Cid}) of
			[] -> error({mmoasp_error, character_not_found});	%% no match
			[D] -> u:dkv_get(D#character.status, Key)
		end
	end,
	mnesia:activity(transaction, F).

setter(Svid, Id, Key, Value) ->
	setter(u:gen_cid(Svid, Id), Key, Value).

setter(Cid, Key, Value) ->
	F = fun(X) ->
		mnesia:write(X#character{status = u:dkv_set(X#character.status, Key, Value)})
	end,
	online_character:apply_character(Cid, F).

-ifdef(TEST).

db_get_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	
	V1 = getter(hibari, 1, "hp"),
	?assert(V1 == 12),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.
	

db_get_no_key_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	
	?assertExit({aborted, {mmoasp_error,key_not_found}}, getter(hibari, 1, "nosuchkey") == 12),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

db_set_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	
	setter(hibari, 1, "hp", 2),
	V1 = getter(hibari, 1, "hp"),
	?assert(V1 == 2),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

-endif.


