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


-module(character_stream).
-include_lib("mmoasp.hrl").
-compile(export_all).


%% UNDER CONSTRUCTION !!!

start(Cid, YawsPid) when is_record(Cid, cid) ->
	O = online_character:get_one(Cid),
	% io:format("character_stream: start(~p) with pid ~p~n", [Sess#session.cid, Sess#session.pid]),
	StreamPid = spawn(fun() -> loop(O, YawsPid) end),
	F = fun() -> mnesia:write(O#online_character{stream_pid = StreamPid}) end,
	mnesia:activity(transaction, F).

% Sess: holds session type data that has character module process.
loop(O, YawsPid) when is_record(O, online_character), is_pid(YawsPid) ->
	link(YawsPid),		% this 'link' works "process living checker".
	
	
	{list_to_know, ListToKnow, NeighborStats, MovePaths} = mmoasp:get_list_to_know(self(), O#online_character.cid),
	send_list_to_stream(YawsPid, (mout:struct_list_to_json(
		[{struct, X} || X <- ListToKnow]
		 ++
		 [{struct, X} || X <- NeighborStats]
		 ++
		 MovePaths
		 ))),
	receive
		{_From, stop} ->
			io:format("character_stream: stop(~p)~n", [O]),
			yaws_api:stream_chunk_end(YawsPid)
		after 500 -> loop(O, YawsPid) % wait 500ms and back to loop top.
	end.

send_list_to_stream(YawsPid, L) ->
	yaws_api:stream_chunk_deliver(YawsPid, L).

