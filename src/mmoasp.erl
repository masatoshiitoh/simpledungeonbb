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


-module(mmoasp).
-include("yaws_api.hrl").
-include("mmoasp.hrl").
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



-compile(export_all).

%-----------------------------------------------------------
%% implement simple api for mmoasp.erl
%-----------------------------------------------------------

start() ->
	battle_observer:start_link(),
	db:start(reset_tables),
	path_finder:start().

start(reset_tables) ->
	battle_observer:start_link(),
	db:start(reset_tables),
	path_finder:start().

stop() ->
	battle_observer:stop(),
	path_finder:stop(),
	db:stop().
	
change_schema() ->
	db:drop_all(),
	db:do_this_once(),
	db:start(reset_tables).

%% GET version of login. This is only for test with web browser.
%% This will be disabled soon.
out(A, 'GET', ["service", _SVID, "login"]) ->
	Params = dict:from_list(yaws_api:parse_query(A)),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	%% io:format("yaws_if. login requested. ~p~n", [Id]),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = login(self(), Id, Pw, Ipaddr),
	case Result of
		{ok, Cid, Token} ->
			mout:return_html(mout:encode_json_array_with_result("ok", [{cid, Cid}, {token, Token}]));
		{ng} ->
			mout:return_html(mout:encode_json_array_with_result("failed", []))
	end;


%% [test] stream I/F "GET http://localhost:8001/service/hibari/stream/listtoknow/cid1234"
out(A, 'GET', ["service", _SVID, "stream", "listtoknow", CID]) ->
	spawn(character_stream, start, [CID,A#arg.pid]),
	{streamcontent, "text/html", ""};

% create account.
out(A, 'POST', ["service", SVID, "create_account"]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = create_account(self(), SVID, Id, Pw, Ipaddr),
	case Result of
		{atomic, ok} ->
			%% io:format("yaws_if. subscribe requested ok. ~p~n", [Id]),
			mout:return_json(mout:encode_json_array_with_result("ok", [{result, "ok"}]));
		{ng, Reason} ->
			%% io:format("yaws_if. subscribe requested failed. ~p~n", [Reason]),
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;

% create account.
out(A, 'POST', ["service", SVID, "delete_account"]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = delete_account(self(), SVID, Id, Pw, Ipaddr),
	case Result of
		{atomic, ok} ->
			%% io:format("yaws_if. subscribe requested ok. ~p~n", [Id]),
			mout:return_json(mout:encode_json_array_with_result("ok", [{result, "ok"}]));
		{ng, Reason} ->
			%% io:format("yaws_if. subscribe requested failed. ~p~n", [Reason]),
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;


% Add New Player Character.
out(A, 'POST', ["service", SVID, "subscribe"]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = create_account(self(), SVID, Id, Pw, Ipaddr),
	case Result of
		{atomic, ok} ->
			%% io:format("yaws_if. subscribe requested ok. ~p~n", [Id]),
			mout:return_json(mout:encode_json_array_with_result("ok", [{result, "ok"}]));
		{ng, Reason} ->
			%% io:format("yaws_if. subscribe requested failed. ~p~n", [Reason]),
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;

%% Change Password.
%% "POST http://localhost:8002/service/hibari/change_password/  id=id0001&password=pw0001&newpassword=pw9991"
out(A, 'POST', ["service", SVID, "change_password"]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	NewPw = param(Params, "newpassword"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = change_password(self(), SVID, Id, Pw, NewPw, Ipaddr),
	case Result of
		{ok} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{id, Id}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;

%% Login
%% Call "POST http://localhost:8002/service/hibari/login/  id=id0001&password=pw0001"
out(A, 'POST', ["service", _SVID, "login"]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = login(self(),Id, Pw, Ipaddr),
	case Result of
		{ok, Cid, Token} ->
			%% io:format("yaws_if. login requested ok. ~p~n", [Cid]),
			mout:return_json(mout:encode_json_array_with_result("ok", [{cid, Cid}, {token, Token}]));
		{ng, Reason} ->
			%% io:format("yaws_if. login requested failed. ~p~n", [Reason]),
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;

%% Logout
%% Call "POST http://localhost:8001/service/hibari/logout/cid0001  token=Token"
out(A, 'POST', ["service", _SVID, "logout", CidX]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	Token = param(Params, "token"),
	%% io:format("yaws_if. logout requested. ~p~n", [CidX]),
	Result = logout(self(), CidX, Token),
	case Result of
		{ok, CidX} ->
			mout:return_json(mout:encode_json_array_with_result("ok",[]));
		{ng} ->
			mout:return_json(mout:encode_json_array_with_result("failed",[]))
	end;

%% Get list to know (Your client calls this every xx sec.)
%% Call "POST http://localhost:8002/service/hibari/listtoknow/cid0001  token=Token"
out(A, 'POST', ["service", _SVID, "listtoknow", CidX]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	_Token = param(Params, "token"),
	%% io:format("yaws_if. listtoknow requested. ~p~n", [CidX]),
	X = get_session(CidX),
	X#session.pid ! {self(), update_neighbor_status, 10},

	{actions_and_stats, ListToKnow, NeighborStats} = get_list_to_know(self(), CidX),
	mout:return_json(mout:list_to_json(ListToKnow ++ NeighborStats));

%% Talk (open talk)
%% Call "POST http://localhost:8001/service/hibari/talk/cid1234  token=Token&talked=hello"
out(A, 'POST', ["service", _SVID, "talk", CidX]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	_Token = param(Params, "token"),
	Talked = param(Params, "talked"),
	%% io:format("yaws_if. talk requested. ~p~n", [CidX]),
	Result = talk(open, CidX, Talked, 100),
	mout:return_json(json:encode({struct, [Result]}));

%% Whisper (person to person talk)
%% Call "POST http://localhost:8001/service/hibari/talk/cid1234/hello"
out(_A, 'GET', ["service", SVID, "whisper", CidX, _TalkTo, Talked]) ->
	[{character, Cid, Pid}] = world_server:lookup(CidX),
	Clist = world_server:test_getdump(),	%% NOT IMPLEMENTED.
	lists:map(fun({character, _CidN, PidN}) ->
			PidN ! {self(), {talk, Cid, Talked}}
		end,
		Clist),
	{html,
		io_lib:format(
			"(~p) Talk by ~p (Pid = ~p) => ~p<br>",
			[SVID,Cid,Pid,Talked])};

%% Move
%% Callr "POST http://localhost:8001/service/hibari/move/cid1234  token=Token&x=3&y=3"
out(A, 'POST', ["service", _SVID, "move", CidX]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	_Token = param(Params, "token"),
	X = erlang:list_to_integer(param(Params, "x")),
	Y = erlang:list_to_integer(param(Params, "y")),
	%% io:format("yaws_if. move requested. cid = ~p, x = ~p, y = ~p~n", [CidX, X, Y]),
	_Result = move:move(CidX, {pos, X, Y}),
	mout:return_json(mout:encode_json_array_with_result("ok",[]));

%% Attack
out(A, 'POST', ["service", _SVID, "attack", CidX, CidTo]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	_Token = param(Params, "token"),
	_Result = battle:single(CidX, CidTo),
	mout:return_json(mout:encode_json_array_with_result("ok",[]));

%% Set attribute
%% Call "GET http://localhost:8002/service/hibari/set/cid0001/KEY?value=VALUE"
out(A, 'GET', ["service", _SVID, "set", Cid, Key]) ->
	Params = dict:from_list(yaws_api:parse_query(A)),
	Value = param(Params, "value"),
	Token = param(Params, "token"),
	Result = character:setter(Cid, Token, Key, Value),
	case Result of
		{ok, _K, _V} ->
			{html, io_lib:format("KV Storage Setter OK. owner=~p, key=~p, value=~p<br>",[Cid, Key, Value])};
		{ng} ->
			{html, "request failed<br>"}
	end;

%% Json sending test code.
%% sample for "GET http://localhost:8002/service/hibari/json"
%% Thanks to http://d.hatena.ne.jp/takkkun/20080626/1214468050
out(_A, 'GET', ["service", _SVID, "json"]) ->
	mout:return_json(json:encode({struct, [{"field1", "foo"}, {field2, "gova"}]}));


% Add New Non Player Character.
out(_A, 'POST', ["service", _SVID, "startnpc", NpcidX]) ->
	npc:start_npc(NpcidX),
	mout:return_json(mout:encode_json_array_with_result("ok",[{"npcid", NpcidX}]));


%% sample for "catch all" handler.
out(A, _Method, _Params) ->
	io:format("yaws_if. catchall. ~n", []),
	io:format("general handler: A#arg.appmoddata = ~p~n"
		"A#arg.appmod_prepath = ~p~n"
		"A#arg.querydata = ~p~n",
		[A#arg.appmoddata,
		A#arg.appmod_prepath,
		A#arg.querydata]),
	{status, 404}.

%% dispacher for RESTful service (caller out/3)
out(A) ->
	{http_request, Req, _params, _unknown} = A#arg.req,
	Uri = yaws_api:request_url(A),
	Path = string:tokens(Uri#url.path, "/"),
	out(A, Req, Path).


param(ParamsDict, Key) ->
	case dict:find(Key, ParamsDict) of
		{ok, Value} -> Value;
		error -> void
	end.


%-----------------------------------------------------------
%% account management.
%-----------------------------------------------------------

delete_account(From, Svid, Id, Pw, Ipaddr) -> {failed}.

create_account(From, Svid, Id, Pw, Ipaddr) ->
	mnesia:transaction(fun() ->
			foreach(fun mnesia:write/1, get_single(Id, Pw))
		end).

get_single(Id, Pass) ->
	Cid = "c" ++ Id,
	Name = "name" ++ Id,
	[
		{auth_basic, Cid, Id, Pass},
		{cdata, Cid, Name, [{"align", "neutral"}]},
		{location, Cid, 1, {pos, 1,3}, offline, offline},
		{money, Cid, 2000, 0}
	].

%% change password command for subscribers.
change_password(From, Svid, Id, Pw, NewPw, Ipaddr) ->
	case 
	
		(case get_cid({basic, Id, Pw}) of
		void -> {ng, check_id_and_password};
		Cid -> 
			mnesia:transaction(fun() ->
				case mnesia:read({auth_basic, Cid}) of
					[] ->mnesia:abort(not_found);
					[Acct] ->
						PasswordChanged = Acct#auth_basic{pass = NewPw},
						mnesia:write(PasswordChanged),
						ok
					end
				end)
		end)

	of
		{atomic,ok} -> {ok};
		Other -> Other
	end.


%-----------------------------------------------------------
%% authorization.
%-----------------------------------------------------------

% caution!
% login/4 cannot avoid failure caused by caller side problem.
% for example,
% > a = 1.
% > a = login(.....).  <- a is already bound!
%
% in such case, requested character will be set to on-line, but no one can handle it.
% Timeout mechanism will clear this situation.
%
login(From, Id, Pw, Ipaddr) ->
	Loaded = load_character(Id,Pw),
	case Loaded of 
		{character, Oid, _CData} ->
			P = db:do(qlc:q([X#session.oid||X<-mnesia:table(session), X#session.oid == Oid])),
			case P of
				[] ->
					% Not found.. Instanciate requested character !
					{ok, _Pid, Token} = setup_player_character(Oid),
					{ok, Oid, Token};
				[Oid] ->
					% found.
					{ng, "character: account is in use"}
			end;
		void ->
			% Load failed.
			{ng, "character: authentication failed"}
		end.

logout(From, Cid, Token) ->
	Radius = 100,
	notice_logout(Cid, {csummary, Cid}, Radius),
	character:stop_child(Cid),
	cancel_trade(Cid),
	db_location_offline(Cid),
	stop_stream((get_session(Cid))#session.stream_pid),
	case mnesia:transaction(fun()-> mnesia:delete({session, Cid})end) of
		{atomic, ok} -> {ok, Cid};
		Other -> Other
	end.



%-----------------------------------------------------------
%% LIST to KNOW sender
%-----------------------------------------------------------
get_list_to_know(_From, Cid) ->
	% send message.
	F = fun(X) ->
		X#session.pid ! {sensor, {self(), request_list_to_know}}
	end,
	apply_session(Cid, F),
	
	% wait reply and receive.
	receive
		{list_to_know, Actions, Stats} -> {actions_and_stats, Actions, Stats}
		after 2000 -> {[], []}
	end.

	
%-----------------------------------------------------------
% Trade APIs.
%   Tid is cid_pair tuple.
% start_trade returns CidPair. use it for trade id (Tid).
%-----------------------------------------------------------
start_trade(Cid1,Cid2) -> trade:db_start_trade(Cid1, Cid2).
set_offer(Cid, Money, Supplies, Estates) -> trade:db_set_offer(Cid, Money, Supplies, Estates).
get_offer(Cid) -> trade:db_get_offer(Cid).
cancel_trade(Cid) -> trade:db_cancel_trade(Cid).
confirm_trade(Cid) -> trade:db_confirm_trade(Cid).

%-----------------------------------------------------------
% Talk APIs.
% chat mode: open/whisper/group
%-----------------------------------------------------------
talk_to(Pid, Sender, MessageBody, Mode) -> Pid ! {self(), talk, Sender, MessageBody, Mode}.

talk(whisper, SenderCid, ToCid, MessageBody) ->
	F = fun(X) ->
		talk_to(X#session.pid, SenderCid, MessageBody, "whisper")
	end,
	apply_session(ToCid, F);

talk(open, SenderCid, MessageBody, Radius) ->
	[talk_to(X#session.pid, SenderCid, MessageBody, "open")
		|| X <- get_all_neighbor_sessions(SenderCid, Radius)],
	{result, "ok"}.

%talk(group, SenderCid, GroupId, MessageBody) ->
%	[talk_to(X#state.pid, SenderCid, MessageBody, open)
%		|| X <- get_group_char_states(GroupId)].

%-----------------------------------------------------------
% notice functions.
%-----------------------------------------------------------

notice_login(SenderCid, {csummary, _Cid, Name}, Radius) ->
	[X#session.pid ! {sensor, {self(), notice_login, SenderCid, Name}}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

notice_logout(SenderCid, {csummary, _Cid}, Radius) ->
	[X#session.pid ! {sensor, {self(), notice_logout, SenderCid}}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

notice_remove(SenderCid, {csummary, _Cid}, Radius) ->
	[X#session.pid ! {sensor, {self(), notice_remove, SenderCid}}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

notice_move(SenderCid, {transition, From, To, Duration}, Radius) ->
	[X#session.pid ! {mapmove, {self(), notice_move, SenderCid, From, To, Duration}}
		|| X <- get_neighbor_char_sessions(SenderCid, Radius)],
	{result, "ok"}.

%-----------------------------------------------------------
% load and setup character for each login.
%-----------------------------------------------------------

setup_player_character(Cid)->
	{character, Cid, CData} = db_get_cdata(Cid),
	db_location_online(Cid),
	R = #task_env{
		cid = Cid,
		cdata = CData,
		event_queue = queue:new(),
		stat_dict = [],
		token = gen_token("nil", Cid),
		utimer =  morningcall:new()},
	Child = spawn(fun() ->character:loop(R, task:mk_idle_reset()) end),
	mnesia:transaction(fun() -> mnesia:write(#session{oid=Cid, pid=Child, type="pc"}) end),
	mnesia:transaction(fun() -> mnesia:write(#u_trade{cid=Cid, tid=void}) end),
	
	%%setup_player_location(Cid),
	setup_player_initial_location(Cid),

	Radius = 100,
	notice_login(Cid, {csummary, Cid, CData#cdata.name}, Radius),
	{ok, Child, R#task_env.token}.

setup_player_initial_location(Cid) ->
	Me = get_initial_location(Cid),
	Map = Me#location.initmap,
	X = Me#location.initx,
	Y = Me#location.inity,
	Z = Me#location.initz,
	db_setpos(Cid, {allpos, Map, X, Y, Z}).

setup_player_location(Cid) ->
	%% copy location data from location table to cdata attribute.
	Me = get_location(Cid),
	Map = Me#location.initmap,
	X = Me#location.initx,
	Y = Me#location.inity,
	db_setter(Cid, "map", Map),
	db_setter(Cid, "x", X),
	db_setter(Cid, "y", Y),
	
	mnesia:transaction(fun() ->
		[Pc] = mnesia:read({session, Cid}),
		mnesia:write(Pc#session{map = Map, x = X, y = Y}) end),
	
	{pos, X, Y}.

%-----------------------------------------------------------
% Character Persistency
%-----------------------------------------------------------
load_character(Id,Pw) ->
	case get_cid({basic, Id, Pw}) of
		void -> void;
		Cid -> {character, Cid, lookup_cdata(Cid)}
	end.

save_character(Cid, CData) ->
	store_cdata(Cid, CData).

%% Load cdata
lookup_cdata(Cid) ->
	case db:do(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid])) of
		[] -> void;
		[CData] -> CData
	end.

%% Save cdata
store_cdata(_Cid, CData) ->
	F = fun() ->
		mnesia:write(CData)
	end,
	mnesia:transaction(F).

%-----------------------------------------------------------
% character data and session
%-----------------------------------------------------------
get_cid({basic, Id, Pw}) ->
	case db:do(qlc:q([X#auth_basic.cid
		|| X <- mnesia:table(auth_basic),
			X#auth_basic.id =:= Id,
			X#auth_basic.pass =:= Pw])) of
		[] -> void;
		[X] -> X
	end.

db_get_cid(Id, Pw) ->
	Loaded = load_character(Id,Pw),
	case Loaded of 
		{character, Cid, _CData} -> {character, Cid};
		void -> {ng, "character: authentication failed"}
		end.

db_get_cdata(Cid) ->
	{character, Cid, lookup_cdata(Cid)}.

stop_stream(Pid) when is_pid(Pid) -> Pid ! {self(), stop};
stop_stream(_) -> void.

db_location_online(Cid) ->
	F = fun() ->
		[CLoc] = mnesia:read({location, Cid}),
		[CSess] = mnesia:read({session, Cid}),
		mnesia:write(CSess#session{map = CLoc#location.initmap, x = CLoc#location.initx, y = CLoc#location.inity})
	end,
	mnesia:transaction(F).

db_location_offline(_Cid) -> nop.

db_location_offline(_Cid, _Map, {pos, _X, _Y}) -> nop.

get_session(Cid) ->
	case apply_session(Cid, fun(X) -> X end) of
		{ng, _} -> void;
		{atomic, Result} -> Result
	end.

get_location(Cid) ->
	case apply_location(Cid, fun(X) -> X end) of
		{ng, _} -> void;
		{atomic, Result} -> Result
	end.

get_initial_location(Cid) ->
	case apply_initial_location(Cid, fun(X) -> X end) of
		{ng, _} -> void;
		{atomic, Result} -> Result
	end.

%-----------------------------------------------------------
% sessions by location.
% (select * from location, session where location.cid = session.cid)
%-----------------------------------------------------------
get_all_neighbor_sessions(Oid, R) ->
	Me = get_session(Oid),
	
	F = fun() ->
		%%	Sess#session.oid =/= Oid,
		qlc:e(qlc:q([Sess || Sess <- mnesia:table(session),
			distance({session, Sess}, {session, Me}) =< R
			]))
	end,
	case mnesia:transaction(F) of
		{atomic, Result} -> Result;
		Other -> Other
	end.

get_neighbor_char_sessions(Oid, R) ->
	Me = get_session(Oid),
	
	F = fun() ->
		%%	Sess#session.oid =/= Oid,
		qlc:e(qlc:q([Sess || Sess <- mnesia:table(session),
			distance({session, Sess}, {session,Me}) =< R,
			Sess#session.type == "pc"]))
	end,
	case mnesia:transaction(F) of
		{atomic, Result} -> Result;
		Other -> Other
	end.

get_neighbor_char_cdata(Oid, R) ->
	Me = get_session(Oid),
	F = fun() ->
		qlc:e(qlc:q(
			[CData#cdata{ attr = CData#cdata.attr ++ [
					{"x", Sess#session.x},{"y", Sess#session.y},{"z", Sess#session.z},{"map", Sess#session.map}
				]}
				|| Sess <- mnesia:table(session),
				%%	Loc#location.cid =/= Cid,
				distance({session, Sess}, {session, Me}) =< R,
				CData <- mnesia:table(cdata),	
				CData#cdata.cid == Sess#session.oid]))
	end,
	case mnesia:transaction(F) of
		{atomic, Result} -> Result;
		Other -> Other
	end.

%-----------------------------------------------------------
% apply function to online characters
%-----------------------------------------------------------

%% F requires 1 arg (session record).
apply_session(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(session), X#session.oid == Cid]), F).

apply_pc(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(session), X#session.oid == Cid, X#session.type == "pc"]), F).

apply_npc(Oid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(session), X#session.oid == Oid, X#session.type == "npc"]), F).

%% F requires 1 arg (cdata record).
apply_cdata(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid]), F).
%% F requires 1 arg (cdata record).
apply_location(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(session), X#session.oid == Cid]), F).

apply_initial_location(Cid, F) ->
	apply_cid_indexed_table(qlc:q([X || X <- mnesia:table(location), X#location.cid == Cid]), F).

apply_cid_indexed_table(Cond, F) ->
	L = fun() ->
		case qlc:e(Cond) of
			[] -> {ng, "no such character"};	% this style makes return value as {atomic, {ng,"no~"}}
			[R] -> F(R)
		end
	end,
	mnesia:transaction(L).

%-----------------------------------------------------------
% moved from character
%-----------------------------------------------------------

gen_stat_from_cdata(X) -> 
	[{cid, X#cdata.cid}, {name, X#cdata.name}] ++ X#cdata.attr.

db_setpos(Cid, {pos, PosX, PosY}) ->
	F = fun(X) ->
		mnesia:write(X#session{x = PosX, y = PosY})
	end,
	apply_session(Cid, F);

db_setpos(Cid, {allpos, Map, PosX, PosY, PosZ}) ->
	F = fun(X) ->
		mnesia:write(X#session{map = Map, x = PosX, y = PosY, z = PosZ})
	end,
	apply_session(Cid, F).



%-----------------------------------------------------------
% Making token.
%-----------------------------------------------------------
gen_token(_Ipaddr, _Cid) -> make_new_id().


%-----------------------------------------------------------
% KV store
%-----------------------------------------------------------

-ifdef(TEST).
kv_get_1_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	Result = kv_get(L, "k1"),
	?assert(Result == "v1").

kv_get_0_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	Result = kv_get(L, "k3"),
	?assert(Result == undefined).

kv_set_1_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	NewL = kv_set(L, "k1", "vnew"),
	Result = kv_get(NewL, "k1"),
	?assert(Result == "vnew").

kv_set_0_test() ->
	L = [{"k1", "v1"}, {"k2", "v2"}],
	NewL = kv_set(L, "k3", "v3"),
	Result = kv_get(NewL, "k3"),
	?assert(Result == "v3").

db_get_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	V1 = db_getter(Cid1, "hp"),
	?assert(V1 == 12),
	
	V2 = db_getter(Cid2, "hp"),
	?assert(V2 == 16),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.
	
db_set_1_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	db_setter(Cid1, "hp", 2),
	V1 = db_getter(Cid1, "hp"),
	?assert(V1 == 2),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.
-endif.

wait(W) ->
	receive
		after W -> ok
	end.

%-----------------------------------------------------------
%% general purpose KV access function.
%-----------------------------------------------------------

% caution !!
% General purpose setter.(use this for configuration store, or other misc operation.
% DO NOT USE for set gaming parameters(like hit point or money) from client.).

kv_get(L, K) ->
	case lists:keysearch(K, 1, L) of
		{value, {K,V}} -> V;
		false -> undefined
	end.

kv_set(L, K, V) ->
	case lists:keymember(K, 1, L) of
		true -> lists:keyreplace(K,1,L, {K, V});
		false -> [{K,V}] ++ L
	end.

db_getter(Cid, Key) ->
	F = fun(X) ->
		kv_get(X#cdata.attr, Key)
	end,
	Result = apply_cdata(Cid, F),
	case Result of
		{atomic, undefined} -> undefined;
		{atomic, V} -> V
	end.

db_setter(Cid, Key, Value) ->
	F = fun(X) ->
		mnesia:write(
			X#cdata{attr = kv_set(X#cdata.attr, Key, Value)}
		)
	end,
	apply_cdata(Cid, F).



%%%
%%% -- utility functions --
%%%


% use for Tid, Cid, ItemId...
make_new_id() ->
	list_to_hexstr(erlang:binary_to_list(erlang:term_to_binary(erlang:make_ref()))).

list_to_hexstr(A) -> list_to_hexstr(A, []).
list_to_hexstr([], Acc) -> lists:flatten(lists:reverse(Acc));
list_to_hexstr([H|T], Acc) -> list_to_hexstr(T, io_lib:format("~2.16.0b", [H]) ++ Acc).

%% TEST list_to_hexstr
-ifdef(TEST).
list_to_hexstr_two_test() -> "ff01" = list_to_hexstr([255,1]).
list_to_hexstr_one_test() -> "ff" = list_to_hexstr([255]).
list_to_hexstr_nil_test() -> "" = list_to_hexstr([]).
-endif.

distance({session, S1}, {session, S2}) ->
	distance(
		{mapxy, S1#session.map, S1#session.x, S1#session.y},
		{mapxy, S2#session.map, S2#session.x, S2#session.y}
	);

distance({mapxy, Map1, X1, Y1}, {mapxy, Map2, X2, Y2}) when Map1 =:= Map2 ->
	distance({pos, X1,Y1}, {pos, X2, Y2});
distance({mapxy, _MapId1, _X1, _Y1}, {mapxy, _MapId2, _X2, _Y2}) ->
	infinity;
distance(_, offline) ->
	infinity;
distance(offline,_) ->
	infinity;
distance({pos, X1, Y1}, {pos, X2, Y2}) ->
	math:sqrt(math:pow((X1-X2),2) + math:pow((Y1-Y2),2));

distance({oid,O1}, {oid,O2}) ->
	distance(
		{session, get_session(O1)},
		{session, get_session(O2)}).


%% TEST distance
-ifdef(TEST).

distance_l_offline_test() -> infinity = distance(offline, {pos, 3, 3}).
distance_r_offline_test() -> infinity = distance({pos, 3,3}, offline).
distance_1_1_test() -> 1.0 = distance({pos, 3, 3}, {pos, 2, 3}).
distance_1_2_test() -> 1.0 = distance({pos, 3, 3}, {pos, 3, 2}).
distance_1_3_test() -> 1.0 = distance({mapxy, "edo", 3, 3}, {mapxy, "edo", 3, 2}).
distance_1_4_test() -> infinity = distance({mapxy, "edo", 3, 3}, {mapxy, "kyoto", 3, 2}).

distance_1_sess_test() ->
	S1 = #session{map = "edo", x = 3, y = 3, z = 1},
	S2 = #session{map = "edo", x = 3, y = 2, z = 1},
	?assert(1.0 == distance({session, S1}, {session, S2})).

distance_by_oid_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),

	?assert(1.0 == distance({oid, Cid1}, {oid, Npcid1})),
	?assert(3.0 == distance({oid, Cid2}, {oid, Npcid1})),
	?assert(4.0 == distance({oid, Cid1}, {oid, Cid2})),

	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}).
-endif.



cid_pair(Cid1, Cid2) when Cid1 < Cid2 -> {cid_pair, Cid1, Cid2};
cid_pair(Cid1, Cid2) when Cid1 >= Cid2 -> {cid_pair, Cid2, Cid1}.

%% TEST cid_pair
-ifdef(TEST).
cid_pair_lr_test() -> {cid_pair, "1", "2"} = cid_pair("1", "2").
cid_pair_rl_test() -> {cid_pair, "1", "2"} = cid_pair("2", "1").
cid_pair_eq_test() -> {cid_pair, "1", "1"} = cid_pair("1", "1").
cid_pair_nil_test() -> {cid_pair, nil, "2"} = cid_pair(nil, "2").
-endif.


store_kvpairs([], Dict) -> Dict;

store_kvpairs(KVPairList, Dict) ->
	[{K,V}|T] = KVPairList,
	store_kvpairs(T, case V of
		[] -> dict:erase(K, V, Dict);
		V -> dict:store(K, V, Dict)
	end).


-ifdef(TEST).
	store_kvpairs_nil_test() ->
		D = dict:new(),
		D = store_kvpairs([], D).
	store_kvpairs_one_test() ->
		D = store_kvpairs([{"k1", "v1"}], dict:new()),
		{ok, "v1"} = dict:find("k1", D).
	store_kvpairs_two_test() ->
		D = store_kvpairs([{"k1", "v1"}, {"k2", "v2"}], dict:new()),
		{ok, "v2"} = dict:find("k2", D).
	store_kvpairs_overwrite_test() ->
		D = store_kvpairs([{"k1", "v1"}, {"k1", "vnew"}], dict:new()),
		{ok, "vnew"} = dict:find("k1", D).
-endif.


% First implementation: its too simple...
%store_kvpairs(KVPairList, Dict) ->
%	[{K,V}|T] = KVPairList,
%	store_kvpairs(T, dict:store(K,V, Dict)).


find_list_from_dict(Cid, Dict) ->
	case dict:find(Cid, Dict) of
		{ok, V} -> V;
		error -> []
		end.

add_new_member(NewMember, List) ->
	case lists:member(NewMember, List) of
		true -> List;
		false -> [NewMember] ++ List
		end.


-ifdef(TEST).

find_list_from_dict_empty_test() ->
	[] = find_list_from_dict("k1", dict:new()).
find_list_from_dict_notfound_test() ->
	D = store_kvpairs([{"k1", "v1"}], dict:new()),
	[] = find_list_from_dict("k2", D).
find_list_from_dict_found_test() ->
	D = store_kvpairs([{"k1", "v1"}], dict:new()),
	"v1" = find_list_from_dict("k1", D).

-endif.






