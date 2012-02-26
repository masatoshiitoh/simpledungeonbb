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


-module(initial_location).
-include("mmoasp.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile(export_all).


get_one(Cid) when is_record(Cid, cid) ->
	mnesia:activity(transaction, fun() ->
		case mnesia:read({initial_location, Cid}) of
			[] -> failed;
			[IL] -> IL#initial_location.location
			end
		end).

set_one(Cid, Loc) when is_record(Cid, cid), is_record(Loc, location) ->
	mnesia:activity(transaction, fun() ->
		case mnesia:read({initial_location, Cid}) of
			[] -> failed;
			[IL] -> mnesia:write(IL#initial_location{location = Loc})
			end
		end).





%---------------------------------
%% test.
%---------------------------------

-ifdef(TEST).

get_location_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	{location, {map_id, hibari, 1}, 1, 1} = get_one(u:gen_cid(hibari, 1)),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

get_location_fail_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	failed = get_one(u:gen_cid(notexist, 1)),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

set_location_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	ok = set_one(u:gen_cid(hibari, 1), {location, {map_id, hibari, 1}, 2, 2}),
	{location, {map_id, hibari, 1}, 2, 2} = get_one(u:gen_cid(hibari, 1)),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

set_location_fail_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	failed = set_one(u:gen_cid(notexist, 1), {location, {map_id, hibari, 1}, 2, 2}),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

-endif.


