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


-module(morningcall).
-export([new/0, add/3, dispatch/2, cancel_all/1]).
%-compile(export_all).


%%% sample codes %%%
test() ->
	Pid = spawn_link(?MODULE, loop, [morningcall:new()]),
	Pid ! start,
	Pid.

loop(DJobs) ->
	receive
		start ->
			F = fun() ->
				io:format("hello!! now = ~p~n", [erlang:now()]) end,
			loop(morningcall:add(2000,F, DJobs));
		cancel_timer ->
			NewNewDJobs = cancel_all(DJobs),
			io:format("receive cancel_timer ~p~n", [NewNewDJobs]),
			loop(NewNewDJobs);
		{goodmorning, Id} ->
			io:format("receive wakeup call ~p~n", [Id]),
			{FunResult, NewNewDJobs} = morningcall:dispatch(Id, DJobs),
			io:format("dispatch result ~p~n", [FunResult]),
			F = fun() -> io:format("hello!! reload = ~p~n", [erlang:now()]) end,
			loop(morningcall:add(2000,F, NewNewDJobs))

	after 10000 ->
		%% io:format("morningcall: timed out~n", []),
		loop(DJobs)
	end.
%%% sample codes (end)%%%

% ------------------------------------------------------
% module core.

new() ->
	timer:start(),
	dict:new().

add(After, Fun, DJobs) ->
	Id = u:make_new_id(),
	case timer:send_after(After, {timer, {goodmorning, Id}}) of
		{ok, TRef} ->
			%% io:format("added with id ~p~n", [Id]),
			dict:store(Id, {morningcall, Fun, TRef}, DJobs);
		{error, Reason} ->
			%% io:format("morningcall:add error. Reason = ~p~n", [Reason]),
			DJobs
	end.

cancel_all(DJobs) ->
	dict:map(
		fun(_Key,Value) ->
			{morningcall, _Fun, TRef} = Value,
			timer:cancel(TRef)
		end, DJobs),
		morningcall:new().

dispatch(Id, DJobs) ->
	case dict:find(Id, DJobs) of
		{ok, {morningcall, F, _TRef}} -> {F(), dict:erase(Id, DJobs)};
		error -> {no_such_id, DJobs}
	end.

