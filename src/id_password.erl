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

-module(id_password).
-include("mmoasp.hrl").
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile(export_all).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
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

make_login_id(Svid, Id) ->
	#login_id{service_name = Svid, id = Id}.

add_one(Svid, Id, Pw) ->
	add_one(make_login_id(Svid, Id), Pw).

add_one(Lid, Pw) ->
	add_one_impl(Lid, Pw, get_one(Lid)).

add_one_impl(Lid, Pw, false) when is_record(Lid, login_id) ->
	NewCid = character:add_one(Lid#login_id.service_name),
	mnesia:activity(transaction, fun() ->
		mnesia:write(#id_password{
			login_id = Lid,
			password = Pw,
			cid = NewCid}) end),
	ok;

add_one_impl(Lid, Pw, _) when is_record(Lid, login_id) ->
	{failed, "Try another Id"}.

get_one(Lid) when is_record(Lid, login_id) ->
	case mnesia:activity(transaction, fun() ->
		mnesia:read({id_password, Lid})
		end) of
		
		[] -> false;
		[A] -> A
	end.

get_one(Lid, Pw) when is_record(Lid, login_id) ->
	case mnesia:activity(transaction, fun() ->
		qlc:e(qlc:q([L || L <- mnesia:table(id_password),
			L#id_password.login_id == Lid,
			L#id_password.password == Pw
			])) end) of

		[] -> false;
		[A] -> A

	end.

check_id_and_password(Lid, Pw) when is_record(Lid, login_id) ->
	case get_one(Lid,Pw) of
		false -> failed;
		A -> A#id_password.cid
	end.

check_id_and_password(Service, Id, Pw) ->
	check_id_and_password(make_login_id(Service, Id), Pw).

update_password(Lid, OldPw, NewPw) ->
	case get_one(Lid,OldPw) of
		false -> failed;
		A -> mnesia:activity(transaction, fun() ->
			mnesia:write(A#id_password{password = NewPw})
			end)
	end.

%% login
%%
login(FromPid, Service, LocalId, Pw, Ipaddr) ->
	login(FromPid, make_login_id(Service, LocalId), Pw, Ipaddr).

login(FromPid, Lid, Pw, Ipaddr) when is_record(Lid, login_id) ->
	case check_id_and_password(Lid, Pw) of
		failed ->
			{ng, "Check your id and password"};
		Cid ->
			case online_character:connect(Cid) of
				{connected, Cid} ->
					{ok, Cid, Token} = session:add_one(Cid);
				{failed, Cid, Reason} ->
					{failed, "no character"}
			end
	end.

%% logout
%%
logout(FromPid, Cid, Ipaddr) ->
	case session:lookup(Cid) of
		failed -> {failed, "no session"};
		S ->
			session:delete(S),
			case online_character:disconnect(S#session.cid) of
				{failed, Cid, Reason} -> {failed, Reason};
				_ -> ok
			end
	end.

logout(FromPid, Service, LocalCid, Ipaddr) ->
	logout(FromPid, u:gen_cid(Service, LocalCid), Ipaddr).

%-----------------------------------------------------------
% test
%-----------------------------------------------------------

-ifdef(TEST).

make_login_id_test() ->
	LI = make_login_id(hibari, 1),
	?assert(LI == {login_id, hibari, 1}),
	{end_of_run_tests}.

check_id_and_password_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	A = check_id_and_password(hibari, "lid00001", "password"),
	?assert(A == {cid, hibari, 1}),
 	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

check_id_and_password_fail_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2} = test:up_scenarios(),
	A = check_id_and_password(hibari, "lid00001", "wrongpassword"),
	?assert(A == failed),
 	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2}),
	{end_of_run_tests}.

login_ok_test() ->
	{scenarios} = test:up_scenarios(without_character_setup),
	{ok, Cid, Token} = login(self(), hibari, "lid00001", "password", {127,0,0,1}),
	?assert(Cid == {cid, hibari, 1}),
 	test:down_scenarios({scenarios}),
	{end_of_run_tests}.

logout_ok_test() ->
	{scenarios} = test:up_scenarios(without_character_setup),
	{ok, Cid, Token} = login(self(), hibari, "lid00001", "password", {127,0,0,1}),
	?assert(Cid == {cid, hibari, 1}),

	Result = logout(self(), Cid, {127,0,0,1}),
	?assert(Result == ok),

 	test:down_scenarios({scenarios}),
	{end_of_run_tests}.

-endif.


