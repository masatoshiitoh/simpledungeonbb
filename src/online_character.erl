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

-module(online_character).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("mmoasp.hrl").
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile(export_all).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([start_new_char/1, move/3, talk/3, force_fail/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_new_char(Cid) ->
	gen_server:call(?MODULE, {start_pc, Cid}).

move(Cid, Token, {pos, X, Y}) ->
	gen_server:call(?MODULE, {move, Cid, Token, {pos, X, Y}}).

talk(Cid, Token, Talked) ->
	gen_server:call(?MODULE, {talk, Cid, Token, Talked}).

force_fail(Cid) ->
	gen_server:call(?MODULE, {force_fail, Cid}).

get_neighbors(Cid) ->
	gen_server:call(?MODULE, {get_neighbors, Cid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, dict:new()}.

handle_call({get_neighbors, Cid}, _From, State) ->
	ListOfNeighbors = [],
	{reply, ListOfNeighbors, State};

handle_call({start_pc, Cid}, _From, State) ->
	NewPcPid = start_pc_impl(Cid, dict:is_key(Cid, State), State),
	{reply, ok, dict:append(Cid, NewPcPid, State)};

handle_call({move, Cid, Token, {pos, X, Y}}, _From, State) ->
	io:format("move called. ~p(~p) {pos, ~p, ~p}~n", [Cid,lookup_ref_by_cid(Cid, State), X, Y]),
	
	player_char:move(lookup_ref_by_cid(Cid, State), {pos, X, Y}),
	{reply, ok, State};

handle_call({talk, Cid, Token, Talked}, _From, State) ->
	io:format("talk called. ~p ~p~n", [Cid, Talked]),
	player_char:talk(lookup_ref_by_cid(Cid, State), Talked),
	{reply, ok, State};

handle_call({force_fail, Cid}, _From, State) ->
	io:format("force_fail called. ~p ~n", [Cid]),
	player_char:force_fail(lookup_ref_by_cid(Cid, State), fail_it),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_pc_impl(Cid, IsCidExist, State) when IsCidExist =:= false ->
	io:format("start_pc_impl (do path) called. ~p~n", [Cid]),
    
	ID = Cid,
    StartFunc = {player_char, start_link, [Cid]},
    Restart = transient,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [player_char],
    ChildSpecChar = {ID, StartFunc, Restart, Shutdown, Type, Modules},
	
	NewPcPid = supervisor:start_child(charpool_sup, ChildSpecChar),
	
	NewPcPid.

lookup_ref_by_cid(Cid, State) ->
	[{ok, Ref}|_T] = dict:fetch(Cid, State),
	Ref.






%----------------------------------------------------------------------
% online_character module:
% 
% online_character?  a set of active cid, map id, location,
% last update, and pid of character.
%
% who call me? : mmoasp, talk, move,... (all game processing module will
% call this module.)
%----------------------------------------------------------------------


%---------------------------------
%% lookup character
%---------------------------------

lookup(Service, LocalCid) ->
	lookup(u:gen_cid(Service, LocalCid)).

lookup(Cid) ->
	mnesia:activity(transaction, fun() ->
		case mnesia:read({online_character, Cid}) of
			[] -> failed;
			[O] -> O
		end
	end).

is_active(Cid) when is_record(Cid, cid) ->
	case lookup(Cid) of
		failed -> false;
		_ -> true
	end.

%---------------------------------
% make character online.
%---------------------------------

connect(Cid) -> 
	case connect_impl(Cid) of
		{failed, Cid, Reason} -> {failed, Cid, Reason};
		{connected, Cid} ->
			%% notice login information to nearby.
			Name = character:get_name(Cid),
			mmoasp:notice_login(Cid, {csummary, Cid, Name}, default:distance()),
			{connected, Cid}
	end.


connect(Svid, Id) ->
	connect(#cid{service_name = Svid, id = Id}).

connect_impl(Cid) when is_record(Cid, cid)->
	case is_active(Cid) of
		true -> {failed, Cid, "character is in use"};
		false -> mnesia:activity(transaction,
			fun() ->
				case mnesia:read({character, Cid}) of
				[] -> {failed, Cid, "character not exist"};
				[C] ->
					O = start_character_impl(C),
					mnesia:write(O),
					{connected, Cid}
				end
			end)
	end.

start_character_impl(C) when is_record(C, character) ->
	O = make_record(C, initial_location),
	Cid = C#character.cid,
	Pid = character:start_child(Cid),

	set_pid(O, Pid).

make_record(C, initial_location)
	when is_record(C, character) ->
	L = initial_location:get_one(C#character.cid),
	make_record(C, L);

make_record(C, L)
	when is_record(C, character), is_record(L, location) ->
	O = make_record(C),
	O#online_character{map_id = L#location.map_id, location = L}.

make_record(C) when is_record(C, character) ->
	#online_character{
		cid = C#character.cid,
		type = C#character.type,
		%map_id,
		%location,
		last_update = erlang:now(),
		pid = undefined
		}.

set_pid(O, Pid) when is_record(O, online_character), is_pid(Pid) ->
	O#online_character{pid = Pid}.

add_record(O) when is_record(O, online_character) ->
	mnesia:activity(transaction,
		fun() ->
			mnesia:write(O),
			O
		end).


%---------------------------------
%% stop online character.
%---------------------------------

disconnect(Cid) when is_record(Cid, cid) ->
	case lookup(Cid) of
		failed -> {failed, Cid, "character not exist"};
		O -> stop_character_impl(O)
	end.

stop_character_impl(O) when is_record(O, online_character) ->
	stop_process(O),
	delete_record(O).

stop_process(O) when is_record(O, online_character) ->
	O#online_character.pid ! {system, {self(), stop_process}}.

stop_stream(Pid) when is_pid(Pid) ->
	Pid ! {self(), stop}.


delete_record(O) when is_record(O, online_character) ->
	mnesia:activity(transaction,
		fun() ->
			Oid = {online_character, O#online_character.cid},
			mnesia:delete(Oid)
			end).

%-----------------------------------------------------------
% character location updater
%-----------------------------------------------------------
setpos(Cid, MapId, {pos, PosX, PosY})
	when is_record(Cid, cid),
		is_record(MapId, map_id) ->
	setpos(Cid,u:gen_location(
		MapId#map_id.service_name,
		MapId#map_id.id,
		PosX,
		PosY)).

setpos(Cid, Location)
	when is_record(Cid, cid),
		is_record(Location, location) ->
	apply_online_character(Cid, fun(X) ->
		mnesia:write(X#online_character{
			location = Location,
			map_id = Location#location.map_id})
		end);

setpos(Cid, {pos, PosX, PosY}) ->
	apply_online_character(Cid, fun(X) -> %% X hold single online_character record.
		L = X#online_character.location,
		NewL = L#location{x = PosX, y = PosY},
		mnesia:write(X#online_character{location = NewL})
		end).

%-----------------------------------------------------------
% apply function to online characters
%-----------------------------------------------------------

apply_cid_indexed_table(Cond, F) ->
	L = fun() ->
		case qlc:e(Cond) of
			[] -> error({mmoasp_error, character_not_found});
			[Row] -> F(Row)
		end
	end,
	mnesia:activity(transaction, L).

%% F requires 1 arg (character record).
apply_character(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(character), X#character.cid == Cid]), F).

%% F requires 1 arg (session record).
apply_online_character(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(online_character), X#online_character.cid == Cid]), F).

send_message_by_cid(Cid, Message) ->
	apply_online_character(Cid, fun(X) -> X#online_character.pid ! Message end).

%-----------------------------------------------------------
% online_character lookup by location.
%-----------------------------------------------------------

get_one(Svid, Id) ->
	get_one(u:gen_cid(Svid, Id)).

get_one(Cid) when is_record(Cid, cid) ->
	apply_online_character(Cid, fun(X) -> X end).

get_players(Cid, R) when is_record(Cid, cid) ->
	Me = get_one(Cid),
	F = fun() ->
		qlc:e(qlc:q([O || O <- mnesia:table(online_character),
			O#online_character.type =:= pc,
			O#online_character.map_id =:= Me#online_character.map_id,
			u:distance(O#online_character.location, Me#online_character.location) =< R
			]))
	end,
	mnesia:activity(transaction, F).

get_all_neighbors(Svid, Id, R) ->
	get_all_neighbors(u:gen_cid(Svid, Id), R).

get_all_neighbors(Cid, R) ->
	Me = get_one(Cid),
	F = fun() ->
		qlc:e(qlc:q([O || O <- mnesia:table(online_character),
			O#online_character.map_id =:= Me#online_character.map_id,
			u:distance(O#online_character.location, Me#online_character.location) =< R
			]))
	end,
	mnesia:activity(transaction, F).


%%----------------------------------
%% tests
%%----------------------------------

-ifdef(TEST).

pc_setpos_normal_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	setpos(Cid1, {pos, 7, 9}),
	
	O = get_one(Cid1),
	
	Loc = O#online_character.location,
	
	?assert(7 == Loc#location.x),
	?assert(9 == Loc#location.y),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


connect_character_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
%%	{connected, Cid1} = connect(hibari,1),
	?assert(Cid1 == u:gen_cid(hibari, "1")),

	{failed, Cid1_2, Reason1_2} = connect(hibari,"1"),
	?assert(Cid1_2 == u:gen_cid(hibari, "1")),
	?assert(Reason1_2 == "character is in use"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

connect_character_2_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{failed, A, B} = connect(#cid{service_name = hibari, id = notexist}),
	?assert(A == #cid{service_name = hibari, id = notexist}),
	?assert(B == "character not exist"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

disconnect_character_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
%%	{connected, Cid} = connect(hibari,1),
	?assert(disconnect(#cid{service_name = hibari, id = "1"}) == ok),
	?assert(disconnect(#cid{service_name = hibari, id = "1"}) == {failed, u:gen_cid(hibari, "1"), "character not exist"}),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

disconnect_character_2_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	?assert(disconnect(#cid{service_name = hibari, id = "1"}) == ok),
	?assert(disconnect(#cid{service_name = hibari, id = "1"}) == {failed, u:gen_cid(hibari, "1"), "character not exist"}),
	?assert(disconnect(#cid{service_name = hibari, id = notexist}) == {failed, u:gen_cid(hibari, notexist), "character not exist"}),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.

