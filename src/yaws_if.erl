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


-module(yaws_if).
-include("yaws_api.hrl").
-include("mmoasp.hrl").
-compile(export_all).

% Module for Yaws.


%% GET version of login. This is only for test with web browser.
%% This will be disabled soon.
out(A, 'GET', ["service", _SVID, "login"]) ->
	Params = dict:from_list(yaws_api:parse_query(A)),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	%% io:format("yaws_if. login requested. ~p~n", [Id]),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = mmoasp:login(self(), Id, Pw, Ipaddr),
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



% Add New Player Character.
out(A, 'POST', ["service", SVID, "subscribe"]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = mmoasp:subscribe(self(), SVID, Id, Pw, Ipaddr),
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
	Result = mmoasp:change_password(self(), SVID, Id, Pw, NewPw, Ipaddr),
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
	Result = mmoasp:login(self(),Id, Pw, Ipaddr),
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
	Result = mmoasp:logout(self(), CidX, Token),
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
	X = world:get_session(CidX),
	X#session.pid ! {self(), update_neighbor_status, 10},

	{actions_and_stats, ListToKnow, NeighborStats} = mmoasp:get_list_to_know(self(), CidX),
	mout:return_json(mout:list_to_json(ListToKnow ++ NeighborStats));

%% Talk (open talk)
%% Call "POST http://localhost:8001/service/hibari/talk/cid1234  token=Token&talked=hello"
out(A, 'POST', ["service", _SVID, "talk", CidX]) ->
	Params = dict:from_list(yaws_api:parse_post(A)),
	_Token = param(Params, "token"),
	Talked = param(Params, "talked"),
	%% io:format("yaws_if. talk requested. ~p~n", [CidX]),
	Result = mmoasp:talk(open, CidX, Talked, 100),
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
	_Result = mmoasp:move(CidX, {pos, X, Y}),
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

%% sample for "GET http://localhost:8001/service/hibari/set/cid0001/KEY?value=VALUE"
out(A, 'GET', ["service", _SVID, "settest", Cid, Key]) ->
	Params = dict:from_list(yaws_api:parse_query(A)),
	Value = param(Params, "value"),
	Token = param(Params, "token"),
	Result = character:setter(Cid, Token, Key, Value),
	case Result of
		{ok, _K, _V} ->
			{html, io_lib:format("KV Storage Setter OK. owner=~p, key=~p, value=~p<br>",[Cid, Key, Value])};
		{ng} ->
			{html, io_lib:format("KV Storage Setter failed. owner=~p, key=~p, value=~p<br>",[Cid, Key, Value])}
	end;


%% sample for "GET http://localhost:8001/service/hibari/setevil/cid0001"
out(A, 'GET', ["service", _SVID, "setevil", CidX]) ->
	Params = dict:from_list(yaws_api:parse_query(A)),
	Token = param(Params, "token"),
	[{character, _Cid, Pid}] = world_server:lookup(CidX),
	Pid ! {self(), Token, change_evil},
	{html, io_lib:format("setevil called.<br>", [])};


%% sample for "GET http://localhost:8001/service/hibari/setgood/cid0001"
out(A, 'GET', ["service", _SVID, "setgood", CidX]) ->
	Params = dict:from_list(yaws_api:parse_query(A)),
	Token = param(Params, "token"),
	[{character, _Cid, Pid}] = world_server:lookup(CidX),
	Pid ! {self(), Token, change_good},
	{html, io_lib:format("setgood called.<br>", [])};


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

