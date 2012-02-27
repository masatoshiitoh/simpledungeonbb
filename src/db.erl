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


-module(db).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start/0, start/1, reset_tables/0]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-import(lists, [foreach/2]).
-include_lib("mmoasp.hrl").

%=======================
% KV access utilities
%=======================


get_new_id_for_service(Svid, IdType) ->
	mnesia:activity(transaction,
		fun() ->
			[S] = mnesia:read({service, Svid}),
			CurrId = u:kv_get(S#service.id_list, IdType),
			mnesia:write(S#service{id_list = u:kv_set(S#service.id_list, IdType, CurrId + 1)}),
			CurrId
			end
		).

%%% TEST CODE ------------------------------------------ %%%
-ifdef(TEST).

access_new_id_01_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	?assert(get_new_id_for_service(testservice, cid) == 100003),
	?assert(get_new_id_for_service(testservice, cid) == 100004),
	?assert(get_new_id_for_service(testservice, cid) == 100005),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

access_new_id_not_exist_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	?assertException(_,_, get_new_id_for_service(testservice, idname_not_exist) == 1),
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

-endif.


%=======================
% DB access layer.
%=======================

start(reset_tables) ->
	start(),
	reset_tables().

start() ->
	mnesia:start(),
	mnesia:wait_for_tables(
		[service,
		admin,
		character,
		online_character,
		session,
		id_password,
		initial_location], 30000).

stop() ->
	mnesia:stop(),
	ok.

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	mnesia:activity(transaction, F).

strip_transaction_result({atomic, ok}) -> ok.

reset_tables() ->
	mnesia:clear_table(service),
	mnesia:clear_table(admin),
	mnesia:clear_table(character),
	mnesia:clear_table(online_character),
	mnesia:clear_table(session),
	mnesia:clear_table(id_password),
	mnesia:clear_table(initial_location),
	mnesia:activity(transaction, fun() -> foreach(fun mnesia:write/1, example_tables()) end).

drop_database() -> mnesia:delete_schema([node()]).
create_database() -> mnesia:create_schema([node()]).
recreate_database() ->
	drop_database(),
	create_database().

create_all_tables() ->
	mnesia:start(),
	mnesia:create_table(service,	[{attributes, record_info(fields, service)}]),
	mnesia:create_table(admin,		[{attributes, record_info(fields, admin)}]),
	mnesia:create_table(character,	[{attributes, record_info(fields, character)}]),
	mnesia:create_table(online_character,	[{attributes, record_info(fields, online_character)}]),
	mnesia:create_table(session,	[{attributes, record_info(fields, session)}]),
	mnesia:create_table(id_password,	[{attributes, record_info(fields, id_password)}]),
	mnesia:create_table(initial_location,	[{attributes, record_info(fields, initial_location)}]),
	mnesia:stop().

recreate_db_and_tables() ->
	recreate_database(),
	create_all_tables().

%% *************************
%% following codes are only for developing use.
%% *************************

demo(service) ->
	do(qlc:q([X || X <- mnesia:table(service)]));

demo(admin) ->
	do(qlc:q([X || X <- mnesia:table(admin)])).

example_tables() ->
	Email = "masatoshi9953@gmail.com",
	Pass = "DefAULTpASSWord!!",
	AdminPass = "adM1Np@SS",
	%%EncEmail = encrypt_text("masatoshi9953@gmail.com"),
	%%EncPass = encrypt_text("DefAULTpASSWord!!"),
	%%EncAdminPass = encrypt_text("adM1Np@SS"),
	
	%%io:format("decrypt_text(EncEmail) = ~p~n", [decrypt_text(EncEmail)]),
	%%io:format("decrypt_text(EncPass) = ~p~n", [decrypt_text(EncPass)]),
	
	[
	{service, hibari, "defaultphrase",[{uid, 3},{cid, 3},{map_id, 2}], {{2013,1,1},{23,59,59}}},
	{admin, #uid{service_name= hibari, id=1}, AdminPass, dict:from_list([{"email", Email}])},

%	{user, {global_uid, hibari, 1}, hibari, "masatoshi", Pass, true, dict:new()},
%	{user, {global_uid, hibari, 2}, hibari, "ufoo", Pass, true, dict:new()},

	{character,
		#cid{service_name = hibari, id = 1},
		"GM Masa",
		dict:from_list([{"sword", 1}]),
		dict:from_list([{"hp", 12}]),
		dict:from_list([{"hidden", 0}])
	},
	{character,
		#cid{service_name = hibari, id = 2},
		"Foo",
		dict:from_list([{"sword", 1}]),
		dict:from_list([{"hp", 12}]),
		dict:from_list([{"hidden", 0}])
	},
	{character,
		#cid{service_name = hibari, id = 99990001},
		"Slime",
		dict:from_list([]),
		dict:from_list([{"hp", 1}]),
		dict:from_list([{"hidden", 0}])
	},


%	{online_character,
%		#cid{service_name = hibari, id = 1},
%		#map_id{service_name = hibari, id = 1},
%		{pos, 1, 1},
%		now(),
%		dummypid
%	},
%	{online_character,
%		#cid{service_name = hibari, id = 2},
%		#map_id{service_name = hibari, id = 1},
%		{pos, 1, 2},
%		now(),
%		dummypid
%	},

%	{session,
%		#cid{service_name = hibari, id = 1},
%		"ToKEN",
%		session:make_expire()
%	},
%	{session,
%		#cid{service_name = hibari, id = 2},
%		"ToKEN",
%		session:make_expire()
%	},

	{initial_location,
		#cid{service_name = hibari, id = 1},
		#location{map_id = #map_id{service_name = hibari, id = 1}, x = 1, y = 1}
	},
	{initial_location,
		#cid{service_name = hibari, id = 2},
		#location{map_id = #map_id{service_name = hibari, id = 1}, x = 1, y = 2}
	},
	{initial_location,
		#cid{service_name = hibari, id = 99990001},
		#location{map_id = #map_id{service_name = hibari, id = 1}, x = 3, y = 3}
	},

	{id_password,
		#login_id{service_name = hibari, id = "lid00001"},
		"password",
		#cid{service_name = hibari, id = 1}
	},
	{id_password,
		#login_id{service_name = hibari, id = "lid00002"},
		"password",
		#cid{service_name = hibari, id = 2}
	},

%	{user_session, {global_uid, hibari, 1}, now(), "ToKEN"},

%{user_character, {global_uid, hibari, 1}, {global_cid, hibari, 1}},
%{user_character, {global_uid, hibari, 2}, {global_uid, hibari, 2}},

	{service, testservice, "testservicephrase00", [{uid, 1003},{cid, 100003},{map_id, 2}], {{2011,12,31},{23,59,59}}},
	{admin, #uid{service_name= testservice, id=1}, AdminPass, dict:from_list([{"email", Email}])},
%	{user, {global_uid, testservice, 1001}, testservice, "testmasa", Pass, true, dict:new()},
%	{user, {global_uid, testservice, 1002}, testservice, "testfoo", Pass, true, dict:new()},
%	{character, {global_cid, testservice, 100001}, "GM TEST", dict:from_list([{"sword", 1}]), dict:from_list([{"hp", 12}])},
%	{character, {global_cid, testservice, 100002}, "TEST CHAR", dict:from_list([{"sword", 1}]), dict:from_list([{"hp", 12}])},
%	{session, {global_cid, testservice, 100001}, {global_map_id, testservice, 1}, {pos, 1, 1}, now(), "ToKEN", dummypid},
%	{session, {global_cid, testservice, 100002}, {global_map_id, testservice, 1}, {pos, 1, 2}, now(), "ToKEN", dummypid}
%	{user_session, {global_uid, testservice, 1001}, now(), "ToKEN"}

	{character,
		#cid{service_name = testservice, id = 100001},
		"GM Masa",
		dict:from_list([{"sword", 1}]),
		dict:from_list([{"hp", 12}]),
		dict:from_list([{"hidden", 0}])
	},
	{character,
		#cid{service_name = testservice, id = 100002},
		"Foo",
		dict:from_list([{"sword", 1}]),
		dict:from_list([{"hp", 12}]),
		dict:from_list([{"hidden", 0}])
	},

	{online_character,
		#cid{service_name = testservice, id = 100001},
		#map_id{service_name = testservice, id = 1},
		#location{ map_id = #map_id{service_name = testservice, id = 1},
			x = 1, y = 1},
		now(),
		dummypid,
		dummypid
	},
	{online_character,
		#cid{service_name = testservice, id = 100002},
		#map_id{service_name = testservice, id = 1},
		#location{ map_id = #map_id{service_name = testservice, id = 1},
			x = 1, y = 2},
		now(),
		dummypid,
		dummypid
	},

	{session,
		#cid{service_name = testservice, id = 100001},
		"ToKEN",
		session:make_expire()
	},
	{session,
		#cid{service_name = testservice, id = 100002},
		"ToKEN",
		session:make_expire()
	},

	{id_password,
		#login_id{service_name = testservice, id = "test00001"},
		"password",
		#cid{service_name = testservice, id = 100001}
	},
	{id_password,
		#login_id{service_name = testservice, id = "test00002"},
		"password",
		#cid{service_name = testservice, id = 100002}
	}

%{user_character, {global_uid, testservice, 1001}, {global_cid, testservice, 100001}},
%{user_character, {global_uid, testservice, 1002}, {global_cid, testservice, 100002}},
	
	
	%% inventory ( key colomn is cid.)
%	{money, "cid0001", 1000, 15},
%	{money, "cid0002", 2000, 0},

%	{supplies, mmoasp:make_new_id(),"cid0001", item_herb, 10, 0},
%	{supplies, mmoasp:make_new_id(), "cid0002", item_herb, 5, 0},
%	{supplies, mmoasp:make_new_id(), "cid0002", item_portion, 15, 0},

%	{estate, item_sword01, "cid0001", false},
%	{estate, item_sword02, "cid0001", false},
%	{estate, item_shield01, "cid0002", false}
	

	].

