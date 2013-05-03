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


-module(sd_api).

-export([login/4, logout/3]).
-export([talk/4, get_list_to_know/2, getter/2, setter/3]).
-export([delete_account/5, create_account/5, change_password/6]).
-export([talk_to/4]).

-compile(export_all).


-include("sd_api.hrl").
-include("mmoasp.hrl").

-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%-----------------------------------------------------------
%% account management.
%-----------------------------------------------------------

delete_account(From, _Svid, Id, Pw, Ipaddr) -> not_implemented.

create_account(From, Svid, Id, Pw, Ipaddr) ->
	Rows =  mmoasp:get_player_character_template(Id, Pw),
	mnesia:transaction(fun() ->
			foreach(fun mnesia:write/1,Rows)
		end).

change_password(From, Svid, Id, Pw, NewPw, Ipaddr) ->
	mmoasp:do_change_password(mmoasp:auth_get_cid({basic, Id, Pw}), From, Svid, Id, Pw, NewPw, Ipaddr).

-ifdef(TEST).

create_account_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	create_account(self(), "hibari", "id9999", "pw9999", {192,168,1,200}),
	
	{ok, Cid3, Token3}
		= login(self(), "id9999", "pw9999", {192,168,1,200}),
	logout(self(), Cid3, Token1),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

change_password_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	create_account(self(), "hibari", "id9999", "pw9999", {192,168,1,200}),
	change_password(self(), "hibari", "id9999", "pw9999", "pwnew", {192,168,1,200}),
	
	{ok, Cid3, Token3}
		= login(self(), "id9999", "pwnew", {192,168,1,200}),
	logout(self(), Cid3, Token1),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


%% TODO: make failure path tests!!

-endif.

%-----------------------------------------------------------
%% authorization.
%-----------------------------------------------------------

% caution!
% login/4 cannot avoid failure caused by caller side problem.
% such as...
% > a = 1.
% > a = mmoasp:login(.....).  <- a is already bound!
%
% in such case, requested character will be set to on-line, but no one can handle it.
% Timeout mechanism will clear this situation.
%
login(From, Id, Pw, Ipaddr) ->
	mmoasp:do_login(Id, Pw, mmoasp:auth_get_cid({basic, Id, Pw})).

logout(From, Cid, _Token) ->
	mmoasp:setdown_player(Cid).

-ifdef(TEST).

login_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{ok, Cid3, Token3}
		= login(self(), "id0003", "pw0003", {192,168,1,200}),
	logout(self(), Cid3, Token1),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

login_duplicated_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{ng, Reason} = login(self(), "id0001", "pw0001", {192,168,1,200}),
	?assert(Reason == "account is in use"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

logout_missing_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	logout(self(), "cid_not_exist", 0),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.

%-----------------------------------------------------------
%% LIST to KNOW sender
%-----------------------------------------------------------
get_list_to_know(_From, Cid) ->
	mmoasp:send_message_by_cid(Cid, {sensor, {self(), request_list_to_know}}),
	receive
		{list_to_know, Actions, Stats, MovePaths} -> {list_to_know, Actions, Stats, MovePaths}
		after 2000 -> {timeout, [], [], []}
	end.

-ifdef(TEST).

get_list_to_know_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{list_to_know, AL, SL, ML} = get_list_to_know(self(), Cid1),
	[A1 | AT] = AL,
	
	% io:format("get_list_to_know_test:~p~n", [A1]),
	A1Type = u:kv_get(A1, type),
	A1Cid = u:kv_get(A1, cid),
	A1Name = u:kv_get(A1, name),
	?assert(A1Type == "login"),
	?assert(A1Cid == "cid0001"),
	?assert(A1Name == "alpha"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


get_list_to_know_none_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{timeout, [], [], []} = get_list_to_know(self(), "cid_not_exist"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.

%-----------------------------------------------------------
% Talk APIs.
% chat mode: open/whisper/group
%-----------------------------------------------------------

talk(whisper, SenderCid, ToCid, MessageBody) ->
	F = fun(X) ->
		talk_to(X#session.pid, SenderCid, MessageBody, "whisper")
	end,
	mmoasp:apply_session(ToCid, F);

talk(open, SenderCid, MessageBody, Radius) ->
	[talk_to(X#session.pid, SenderCid, MessageBody, "open")
		|| X <- map2d:get_all_neighbor_sessions(SenderCid, Radius)],
	{result, "ok"}.

%%
%% low level interface - talk to character identified by PID.
%%
talk_to(Pid, Sender, MessageBody, Mode) ->
	Pid ! {self(), talk, Sender, MessageBody, Mode}.

% caution !!
% Following getter/2 and setter/2 are dangerous to open to web interfaces.
% DO NOT OPEN them as web i/f to set gaming parameters
% (like hit point) from remote.
getter(Cid, Key) ->
	F = fun() ->
		case mnesia:read({cdata, Cid}) of
			[] -> undefined;	%% no match
			[D] -> u:kv_get(D#cdata.attr, Key)
		end
	end,
	u:mn_strip_atomic(mnesia:transaction(F)).

setter(Cid, Key, Value) ->
	F = fun(X) ->
		mnesia:write(X#cdata{attr = u:kv_set(X#cdata.attr, Key, Value)})
	end,
	mmoasp:apply_cdata(Cid, F).

-ifdef(TEST).

db_get_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	V1 = getter(Cid1, "hp"),
	?assert(V1 == 12),
	
	V2 = getter(Cid2, "hp"),
	?assert(V2 == 16),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.
	
db_set_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	setter(Cid1, "hp", 2),
	V1 = getter(Cid1, "hp"),
	?assert(V1 == 2),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.



