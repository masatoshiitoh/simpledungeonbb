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


-module(task).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-compile(export_all).

-include_lib("mmoasp.hrl").

%% for task utilities for PC/NPC

system_call({From, stop_process}, R, _I) ->
	io:format("~p: proc stop by stop_process message.~n", [R#task_env.cid]),
	morningcall:cancel_all(R#task_env.utimer),
	From ! {ok, R#task_env.cid}.

%%% TEST CODE ------------------------------------------ %%%
-ifdef(TEST).

task_01_test() ->
	{end_of_run_tests}.

-endif.


