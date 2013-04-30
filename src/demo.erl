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


-module(demo).
-compile(export_all).

json_test() ->	
	L = test_data(),
	D2 = [{struct, X} || X <- L],
	json:encode({array, D2}).

json_test_liststyle() ->
	lists:map(fun(X) -> mout:object_list_to_json(X) end, test_data()).

test_data_2() ->
	[
		[
			{id, u:make_new_id()},
			{type,"talk"},
			{cid,"cid1234"},
			{content,"talk, line 2"},
			{mode,"whisper"},
			{array, 
				[
					{struct, [{field1, "val1"}]},
					{struct, [{field2, "val2"}]}
				]
			}

		]
	].
		
	

test_data_1() ->
	{array, 
		[
			{struct, [{field1, "val1"}]},
			{struct, [{field1, "val1"}]}
		]
	}.


test_data() ->
	[		
		[
			{id, u:make_new_id()},
			{type,"talk"},
			{cid,"cid1234"},
			{content,"talk, line 2"},
			{mode,"whisper"}
		],
		
		[
			{id, u:make_new_id()},
			{type,"talk"},
			{cid,"cid1234"},
			{content, "hello cid0001 from cid1234, with love"},
			{mode,"whisper"}
		],
		
		[
			{id, u:make_new_id()},
			{type,"talk"},
			{cid,"cid1234"},
			{content,"hello all from cid1234 "},
			{mode,"open"}
		],

		[
			{id, u:make_new_id()},
			{type, "status"},
			{"cid","cid0001"},
			{"name","alpha"},
			{"WindowSize","123,55"},
			{"align","good"}
		]
	].