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


-module(con_yaws).

-export([out/1]).
-export([start_yaws/1]).

-include("yaws_api.hrl").
-include("mmoasp.hrl").

%%
%% initialize yaws with con_yaws.
%%
start_yaws(Sup) ->
	Id = "simpledungeon",
	GconfList = [
		{logdir, "./test/log"},
		{ebin_dir, [".","../ebin"]},
		{id, Id}],
	Docroot = "../docroot",
	SconfList = [
		{port, 8002},
		{listen, {0,0,0,0}},
		{docroot, Docroot},
		{appmods, [{"/service", con_yaws}]}
	],

	{ok, SCList, GC, ChildSpecs} =
	    yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

	[supervisor:start_child(Sup, Ch) || Ch <- ChildSpecs],
	%% now configure Yaws
	yaws_api:setconf(GC, SCList).

%%
%% dispacher for RESTful service (caller out/3)
%%
out(A) ->
	{http_request, Req, _params, _unknown} = A#arg.req,
	Uri = yaws_api:request_url(A),
	Path = string:tokens(Uri#url.path, "/"),
	out(A, Req, Path).

%%
%% read parameters into dict.
%%
make_params({get, A}) ->
	dict:from_list(yaws_api:parse_query(A));

make_params({post, A}) ->
	dict:from_list(yaws_api:parse_post(A)).

%%
%% read one parameter from dict.
%%
param(ParamsDict, Key) ->
	case dict:find(Key, ParamsDict) of
		{ok, Value} -> Value;
		error -> void
	end.

%%
%% read parameters into tuple.
%%
prepare_id_password({Method, A}) ->
	Params = make_params({Method, A}),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	{Id, Pw, Ipaddr}.

prepare_change_password({Method, A}) ->
	Params = make_params({post, A}),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	NewPw = param(Params, "newpassword"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	{Id, Pw, NewPw, Ipaddr}.

prepare_token({Method, A}) ->
	Params = make_params({Method, A}),
	Token = param(Params, "token").

%% GET version of login. This is only for test with web browser.
%% This will be disabled soon.
out(A, 'GET', ["service", _SVID, "login"]) ->
	{Id, Pw, Ipaddr} = prepare_id_password({get, A}),

	Result = sd_api:login(self(), Id, Pw, Ipaddr),

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
	{Id, Pw, Ipaddr} = prepare_id_password({post, A}),

	Result = sd_api:create_account(self(), SVID, Id, Pw, Ipaddr),

	case Result of
		{atomic, ok} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{result, "ok"}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;


% create account.
out(A, 'POST', ["service", SVID, "delete_account"]) ->
	{Id, Pw, Ipaddr} = prepare_id_password({post, A}),

	Result = sd_api:delete_account(self(), SVID, Id, Pw, Ipaddr),

	case Result of
		{atomic, ok} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{result, "ok"}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;


% Add New Player Character.
out(A, 'POST', ["service", SVID, "subscribe"]) ->
	{Id, Pw, Ipaddr} = prepare_id_password({post, A}),

	Result = sd_api:create_account(self(), SVID, Id, Pw, Ipaddr),

	case Result of
		{atomic, ok} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{result, "ok"}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;

%% Change Password.
%% "POST http://localhost:8002/service/hibari/change_password/  id=id0001&password=pw0001&newpassword=pw9991"
out(A, 'POST', ["service", SVID, "change_password"]) ->
	{Id, Pw, NewPw, Ipaddr} = prepare_change_password({post, A}),

	Result = sd_api:change_password(self(), SVID, Id, Pw, NewPw, Ipaddr),

	case Result of
		{atomic, ok} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{id, Id}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;

%% Login
%% Call "POST http://localhost:8002/service/hibari/login/  id=id0001&password=pw0001"
out(A, 'POST', ["service", _SVID, "login"]) ->
	{Id, Pw, Ipaddr} = prepare_id_password({post, A}),

	Result = sd_api:login(self(),Id, Pw, Ipaddr),

	case Result of
		{ok, Cid, Token} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{cid, Cid}, {token, Token}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;

%% Logout
%% Call "POST http://localhost:8001/service/hibari/logout/cid0001  token=Token"
out(A, 'POST', ["service", _SVID, "logout", CidX]) ->
	Token = prepare_token({post, A}),

	Result = sd_api:logout(self(), CidX, Token),

	case Result of
		{ok, CidX} ->
			mout:return_json(mout:encode_json_array_with_result("ok",[]));
		{ng} ->
			mout:return_json(mout:encode_json_array_with_result("failed",[]))
	end;

%% Get list to know (Your client calls this every xx sec.)
%% Call "POST http://localhost:8002/service/hibari/listtoknow/cid0001  token=Token"
out(A, 'POST', ["service", _SVID, "listtoknow", CidX]) ->
	_Token = prepare_token({post, A}),

	mmoasp:send_message_by_cid(CidX, {self(), update_neighbor_status, map2d:default_distance()}),
	{list_to_know, ListToKnow, NeighborStats, MovePaths} = sd_api:get_list_to_know(self(), CidX),

	mout:return_json(mout:struct_list_to_json(
		[{struct, X} || X <- ListToKnow]
		 ++
		 [{struct, X} || X <- NeighborStats]
		 ++
		 MovePaths
		 ));

%% Talk (open talk)
%% Call "POST http://localhost:8001/service/hibari/talk/cid1234  token=Token&talked=hello"
out(A, 'POST', ["service", _SVID, "talk", CidX]) ->
	Params = make_params({post, A}),
	_Token = param(Params, "token"),
	Talked = param(Params, "talked"),

	Result = sd_api:talk(open, CidX, Talked, map2d:default_distance()),

	mout:return_json(json:encode({struct, [Result]}));

%% Whisper (person to person talk)
%% Call "POST http://localhost:8001/service/hibari/talk/cid1234/hello"
out(_A, 'POST', ["service", SVID, "whisper", CidX, _TalkTo, Talked]) ->
	not_implemented;

%% Move
%% Callr "POST http://localhost:8001/service/hibari/move/cid1234  token=Token&x=3&y=3"
out(A, 'POST', ["service", SVID, "move", CidX]) ->
	Params = make_params({post, A}),
	_Token = param(Params, "token"),
	X = erlang:list_to_integer(param(Params, "x")),
	Y = erlang:list_to_integer(param(Params, "y")),

	_Result = move:move({map_id, SVID, 1}, CidX, {pos, X, Y}),%%% TODO Write {map_id,SV,MAPID} appropriately !!

	mout:return_json(mout:encode_json_array_with_result("ok",[]));

%% Attack
out(A, 'POST', ["service", _SVID, "attack", CidX, CidTo]) ->
	_Token = prepare_token({post, A}),

	_Result = battle:single(CidX, CidTo),

	mout:return_json(mout:encode_json_array_with_result("ok",[]));

%% Set attribute
%% Call "GET http://localhost:8002/service/hibari/set/cid0001/KEY?value=VALUE"
out(A, 'GET', ["service", _SVID, "set", Cid, Key]) ->
	Params = make_params({get, A}),
	Value = param(Params, "value"),
	Token = param(Params, "token"),

	Result = character:setter(Cid, Token, Key, Value),

	case Result of
		{ok, _K, _V} ->
			{html, io_lib:format("KV Storage Setter OK. owner=~p, key=~p, value=~p<br>",[Cid, Key, Value])};
		{ng} ->
			{html, "request failed<br>"}
	end;

% Add New Non Player Character.
out(_A, 'POST', ["service", _SVID, "startnpc", NpcidX]) ->
	npc:start_npc(NpcidX),
	mout:return_json(mout:encode_json_array_with_result("ok",[{"npcid", NpcidX}]));


% Add New Non Player Character.
out(A, 'POST', ["service", _SVID, "ping"]) ->
	Params = make_params({post, A}),
	AdminId = param(Params, "admin_id"),
	ConnectPhrase = param(Params, "connect_phrase"),
	mout:return_json(
		mout:encode_json_array_with_result(
			"ok",
			[{"status", ok},
			{"admin_id", AdminId},
			{"connect_phrase", ConnectPhrase}]));


%% sample for "catch all" handler.
out(A, _Method, _Params) ->
	io:format("out/3 general handler: A#arg.appmoddata = ~p~n"
		"A#arg.appmod_prepath = ~p~n"
		"A#arg.querydata = ~p~n",
		[A#arg.appmoddata,
		A#arg.appmod_prepath,
		A#arg.querydata]),
	{status, 404}.

