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
	{service, hibari, "defaultphrase",[], {{2013,1,1},{23,59,59}}},
	{admin, #uid{service_name= hibari, id="1"}, AdminPass, dict:from_list([{"email", Email}])},

	{character,
		#cid{service_name = hibari, id = "1"},
		pc,
		"alpha",
		dict:from_list([{"sword", 1}]),
		dict:from_list([{"type", "pc"}, {"hp", 12}]),
		dict:from_list([{"hidden", 0}])
	},
	{character,
		#cid{service_name = hibari, id = "2"},
		pc,
		"bravo",
		dict:from_list([{"sword", 1}]),
		dict:from_list([{"type", "pc"}, {"hp", 12}]),
		dict:from_list([{"hidden", 0}])
	},
	{character,
		#cid{service_name = hibari, id = "3"},
		pc,
		"charlie",
		dict:from_list([{"sword", 1}]),
		dict:from_list([{"type", "pc"}, {"hp", 12}]),
		dict:from_list([{"hidden", 0}])
	},
	{character,
		#cid{service_name = hibari, id = "4"},
		pc,
		"delta",
		dict:from_list([{"sword", 1}]),
		dict:from_list([{"type", "pc"}, {"hp", 12}]),
		dict:from_list([{"hidden", 0}])
	},
	{character,
		#cid{service_name = hibari, id = "99990001"},
		npc,
		"Slime",
		dict:from_list([]),
		dict:from_list([{"type", "npc"}, {"hp", 1}]),
		dict:from_list([{"hidden", 0}])
	},

	{initial_location,
		#cid{service_name = hibari, id = "1"},
		#location{map_id = #map_id{service_name = hibari, id = "1"}, x = 1, y = 1}
	},
	{initial_location,
		#cid{service_name = hibari, id = "2"},
		#location{map_id = #map_id{service_name = hibari, id = "1"}, x = 5, y = 1}
	},
	{initial_location,
		#cid{service_name = hibari, id = "3"},
		#location{map_id = #map_id{service_name = hibari, id = "1"}, x = 6, y = 1}
	},
	{initial_location,
		#cid{service_name = hibari, id = "4"},
		#location{map_id = #map_id{service_name = hibari, id = "1"}, x = 7, y = 1}
	},
	{initial_location,
		#cid{service_name = hibari, id = "99990001"},
		#location{map_id = #map_id{service_name = hibari, id = "1"}, x = 2, y = 1}
	},

	{id_password,
		#login_id{service_name = hibari, id = "id0001"},
		"pw0001",
		#cid{service_name = hibari, id = "1"}
	},
	{id_password,
		#login_id{service_name = hibari, id = "id0002"},
		"pw0002",
		#cid{service_name = hibari, id = "2"}
	},
	{id_password,
		#login_id{service_name = hibari, id = "id0003"},
		"pw0003",
		#cid{service_name = hibari, id = "3"}
	},
	{id_password,
		#login_id{service_name = hibari, id = "id0004"},
		"pw0004",
		#cid{service_name = hibari, id = "4"}
	},

	{service, testservice, "testservicephrase00", [], {{2011,12,31},{23,59,59}}},
	{admin, #uid{service_name= testservice, id="1"}, AdminPass, dict:from_list([{"email", Email}])},

	{character,
		#cid{service_name = testservice, id = "100001"},
		pc,
		"alpha",
		dict:from_list([{"sword", 1}]),
		dict:from_list([{"hp", 12}]),
		dict:from_list([{"hidden", 0}])
	},
	{character,
		#cid{service_name = testservice, id = "100002"},
		pc,
		"Foo",
		dict:from_list([{"sword", 1}]),
		dict:from_list([{"hp", 12}]),
		dict:from_list([{"hidden", 0}])
	},

	{online_character,
		#cid{service_name = testservice, id = "100001"},
		pc,
		#map_id{service_name = testservice, id = "1"},
		#location{ map_id = #map_id{service_name = testservice, id = "1"},
			x = 1, y = 1},
		now(),
		dummypid,
		dummypid
	},
	{online_character,
		#cid{service_name = testservice, id = "100002"},
		pc,
		#map_id{service_name = testservice, id = "1"},
		#location{ map_id = #map_id{service_name = testservice, id = "1"},
			x = 1, y = 2},
		now(),
		dummypid,
		dummypid
	},

	{session,
		#cid{service_name = testservice, id = "100001"},
		"ToKEN",
		session:make_expire()
	},
	{session,
		#cid{service_name = testservice, id = "100002"},
		"ToKEN",
		session:make_expire()
	},

	{id_password,
		#login_id{service_name = testservice, id = "test00001"},
		"password",
		#cid{service_name = testservice, id = "100001"}
	},
	{id_password,
		#login_id{service_name = testservice, id = "test00002"},
		"password",
		#cid{service_name = testservice, id = "100002"}
	}

	].

