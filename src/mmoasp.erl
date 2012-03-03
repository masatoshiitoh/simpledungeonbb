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
	crypto:start(),
	battle_observer:start_link(),
	path_finder:start(),
	db:start().

start(reset_tables) ->
	crypto:start(),
	battle_observer:start_link(),
	path_finder:start(),
	db:start(reset_tables).

stop() ->
	db:stop(),
	battle_observer:stop(),
	crypto:stop().
	
change_schema() ->
	db:stop(),
	db:recreate_db_and_tables(),
	db:start(reset_tables).

gen_cid(Req) when is_record(Req, request) ->
	u:gen_cid(Req#request.service_name, Req#request.id1).

make_boolean_json_result(A) ->
	case A of
		{ok, Cid, Token} when is_record(Cid, cid) ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{cid, u:local_cid(Cid)}, {token, Token}]));
		ok ->
			mout:return_json(mout:encode_json_array_with_result("ok",[]));
		{failed, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed",[{reason, Reason}]))
		end.


%% Login
%% Call "POST http://localhost:8002/service/hibari/login/  id=id0001&password=pw0001"
%%
%% ---updated: remodeling. ---
%%
action(login, Req, Param) ->
	Ipaddr = Req#request.client_ip,
	Svid = Req#request.service_name,
	Id = param(Param, "id"),
	Pw = param(Param, "password"),
	make_boolean_json_result(id_password:login(self(), Svid, Id, Pw, Ipaddr));

%% Logout
%% Call "POST http://localhost:8001/service/hibari/logout/1234  token=Token"
%%
%% ---updated: remodeling. ---
%%
action(logout, Req, Param) ->
	Cid = gen_cid(Req),
	Token = param(Param, "token"),
	Ipaddr = Req#request.client_ip,
	make_boolean_json_result(
		session:check_and_call(Cid, Token,
			fun() -> id_password:logout(self(), Cid, Ipaddr) end));

%% Get list to know (Your client calls this every xx sec.)
%% Call "POST http://localhost:8002/service/hibari/listtoknow/1234  token=Token"
action(listtoknow, Req, Param) ->
	Cid = gen_cid(Req),
	Token = param(Param, "token"),
	session:check_and_call(Cid, Token, fun() -> 
		online_character:send_message_by_cid(Cid, {self(), update_neighbor_status, default:distance()}),
		{list_to_know, ListToKnow, NeighborStats, MovePaths} = list_to_know:request(Cid),
		mout:return_json(mout:struct_list_to_json(
			[{struct, X} || X <- ListToKnow]
			++
			[{struct, X} || X <- NeighborStats]
			++
			MovePaths))
	end);

%% Talk (open talk)
%% Call "POST http://localhost:8001/service/hibari/talk/1234  token=Token&talked=hello"
action(talk, Req, Param) ->
	Cid = gen_cid(Req),
	Token = param(Param, "token"),
	session:check_and_call(Cid, Token, fun() -> 
		Result = talk:to_neighbors(Cid, param(Param, "talked"), default:distance()),
		mout:return_json(json:encode({struct, [Result]}))
	end);


%% Move
%% Callr "POST http://localhost:8001/service/hibari/move/1234  token=Token&x=3&y=3"
action(move, Req, Param) ->
	Cid = gen_cid(Req),
	Token = param(Param, "token"),
	
	X = get_param_int(Param, "x"),
	Y = get_param_int(Param, "y"),
	
	session:check_and_call(Cid, Token, fun() ->
		_Result = move:move(
			Cid,
			{pos, X, Y}
			),%%% TODO Write {map_id,SV,MAPID} appropriately !!
		mout:return_json(mout:encode_json_array_with_result("ok",[]))
	end);

%% Set attribute
%% Call "GET http://localhost:8002/service/hibari/set/1234  token=Token&key=KEY&value=VALUE"
action(set, Req, Param) ->
	Cid = gen_cid(Req),

	Token = param(Param, "token"),
	Key   = param(Param, "key"),
	Value = param(Param, "value"),

	session:check_and_call(Cid, Token, fun() ->
		Result = char_kv:setter(Cid, Key, Value),
		case Result of
			{ok, _K, _V} ->
				{html, io_lib:format("KV Storage Setter OK. owner=~p, key=~p, value=~p<br>",[Cid#cid.id, Key, Value])};
			ng ->
				{html, "request failed<br>"}
		end
	end);

%% Get attribute
%% Call "GET http://localhost:8002/service/hibari/set/cid0001/KEY?value=VALUE"
action(get, Req, Param) ->
	Cid = gen_cid(Req),
	Token = param(Param, "token"),
	Key   = param(Param, "key"),

	session:check_and_call(Cid, Token, fun() ->
		Result = char_kv:getter(Cid, Key),
		case Result of
			{ok, V} ->
				{html, io_lib:format("KV Storage Getter OK. owner=~p, key=~p, value=~p<br>",[Cid#cid.id, Key, V])};
			ng ->
				{html, "request failed<br>"}
		end
	end).

%% dispacher for RESTful service (caller out/3)
out(A) ->
	{http_request, Req, _params, _unknown} = A#arg.req,
	Uri = yaws_api:request_url(A),
	Path = string:tokens(Uri#url.path, "/"),
	out(A, Req, Path).
	
out(A, 'GET', Path) ->
	Param = make_param({get, A}),
	Req = map_path_to_request(Path, A),
	action(Req#request.action, Req, Param);

out(A, 'POST', Path) ->
	Param = make_param({post, A}),
	Req = map_path_to_request(Path, A),
	action(Req#request.action, Req, Param).


map_path_to_request(Path,A) ->
	{Ipaddr, Port} = A#arg.client_ip_port,
	X = #request{
		client_ip = Ipaddr,
		port = Port},

	case Path of
		["service", ServiceIdStr, Action] ->
			X#request{
				service_name = erlang:list_to_atom(ServiceIdStr),
				action = erlang:list_to_atom(Action),
				version = development};

		["service", ServiceIdStr, Action, Id1] ->
			X#request{
				service_name = erlang:list_to_atom(ServiceIdStr),
				action = erlang:list_to_atom(Action),
				version = development,
				id1 = erlang:list_to_integer(Id1)};

		["service", ServiceIdStr, Action, Id1, Id2] ->
			X#request{
				service_name = erlang:list_to_atom(ServiceIdStr),
				action = erlang:list_to_atom(Action),
				version = development,
				id1 = erlang:list_to_integer(Id1),
				id2 = erlang:list_to_integer(Id2)}	;

		["v", VerIdStr, "service", ServiceIdStr, Action] ->
			X#request{
				service_name = erlang:list_to_atom(ServiceIdStr),
				action = erlang:list_to_atom(Action),
				version = erlang:list_to_atom(VerIdStr)};

		["v", VerIdStr, "service", ServiceIdStr, Action, Id1] ->
			X#request{
				service_name = erlang:list_to_atom(ServiceIdStr),
				action = erlang:list_to_atom(Action),
				version = erlang:list_to_atom(VerIdStr),
				id1 = erlang:list_to_integer(Id1)};

		["v", VerIdStr, "service", ServiceIdStr, Action, Id1, Id2] ->
			X#request{
				service_name = erlang:list_to_atom(ServiceIdStr),
				action = erlang:list_to_atom(Action),
				version = erlang:list_to_atom(VerIdStr),
				id1 = erlang:list_to_integer(Id1),
				id2 = erlang:list_to_integer(Id2)}
	end.
	

%% parameter handler for Yaws' A value.

make_param({get, A}) ->
	dict:from_list(yaws_api:parse_query(A));

make_param({post, A}) ->
	dict:from_list(yaws_api:parse_post(A)).

get_param_int(Param, Key) ->
	erlang:list_to_integer(param(Param, Key)).

param(ParamDict, Key, Default) -> %% this param/3 will return Default value when key not found.
	case dict:find(Key, ParamDict) of
		{ok, Value} -> Value;
		error -> Default
	end.

param(ParamDict, Key) -> %% this param/2 will exit when key not found.
	case dict:find(Key, ParamDict) of
		{ok, Value} -> Value;
		error -> error({mmoasp_error, key_not_found, Key})
	end.

%-----------------------------------------------------------
% notice functions.
%-----------------------------------------------------------

notice_login(SenderCid, {csummary, Cid, Name}, Radius) ->
	send_message_to_neighbors(
		SenderCid,
		{sensor, {self(), notice_login, Cid, Name}},
		Radius).

notice_logout(SenderCid, {csummary, Cid}, Radius) ->
	send_message_to_neighbors(
		SenderCid,
		{sensor, {self(), notice_logout, Cid}},
		Radius).

notice_remove(SenderCid, {csummary, Cid}, Radius) ->
	send_message_to_neighbors(
		SenderCid,
		{sensor, {self(), notice_remove, Cid}},
		Radius).

notice_move(SenderCid, {transition, From, To, Duration}, Radius) ->
	send_message_to_neighbors(
		SenderCid,
		{mapmove, {self(), notice_move, SenderCid, From, To, Duration}},
		Radius).

notice_move_list(SenderCid, {transition_list, L}, Radius) ->
	send_message_to_neighbors(
		SenderCid,
		{mapmove, {self(), notice_move_list, SenderCid, L}},
		Radius).

send_message_to_neighbors(SenderCid, Message, Radius) ->
	[X#online_character.pid ! Message
		|| X <- online_character:get_all_neighbors(SenderCid, Radius)],
	{result, "ok"}.

send_message_by_cid(Cid, Message) ->
	online_character:apply_online_character(Cid, fun(X) -> X#online_character.pid ! Message end).


%-----------------------------------------------------------
% Talk APIs.
% chat mode: open/whisper/group
%-----------------------------------------------------------
talk_to(Pid, Sender, MessageBody, Mode) when is_pid(Pid) ->
	Pid ! {self(), talk, Sender, MessageBody, Mode}.

talk(whisper, SenderCid, ToCid, MessageBody) when is_record(SenderCid, cid), is_record(ToCid, cid) ->
	F = fun(X) ->
		talk_to(X#online_character.pid, SenderCid, MessageBody, "whisper")
	end,
	online_character:apply_online_character(ToCid, F);

talk(open, SenderCid, MessageBody, Radius) when is_record(SenderCid, cid) ->
	[talk_to(X#online_character.pid, SenderCid, MessageBody, "open")
		|| X <- online_character:get_all_neighbors(SenderCid, Radius)],
	{result, "ok"}.

%talk(group, SenderCid, GroupId, MessageBody) ->
%	[talk_to(X#state.pid, SenderCid, MessageBody, open)
%		|| X <- get_group_char_states(GroupId)].

-ifdef(TEST).

param_1_test() ->
	?assert(param(dict:from_list([{"a", "128"}, {"b", "256"}]), "a") == "128"),
	{end_of_run_tests}.

param_2_test() ->
	?assert(param(dict:from_list([{"a", "128"}, {"b", "256"}]), "c", "defaultvalue") == "defaultvalue"),
	{end_of_run_tests}.

param_3_test() ->
	?assertException(error,{mmoasp_error, key_not_found, "c"} , param(dict:from_list([{"a", "128"}, {"b", "256"}]), "c") == "will_cause_error_exit"),
	{end_of_run_tests}.

-endif.



%% -------Salvaged code ------------

%% Change Password.
%% "POST http://localhost:8002/service/hibari/change_password/  id=id0001&password=pw0001&newpassword=pw9991"
updated_out(A, 'POST', ["service", SVID, "change_password"]) ->
	Params = make_param({post, A}),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	NewPw = param(Params, "newpassword"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = id_password:update_password(id_password:make_login_id(SVID, Id), Pw, NewPw),
	case Result of
		{atomic, ok} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{id, Id}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end.


setup_task_env(Cid) ->
	#task_env{
		cid = Cid,
		event_queue = queue:new(),
		stat_dict = [],
		token = u:gen_token("nil", Cid),
		utimer =  morningcall:new()}.


login(FromPid, Lid, Pw, Ipaddr) when is_record(Lid, login_id) ->
	id_password:login(FromPid, Lid, Pw, Ipaddr).


do_change_password(Cid, From, Svid, Id, OldPw, NewPw, Ipaddr) ->
	Lid = id_password:make_login_id(Svid, Id),
	id_password:update_password(Lid, OldPw, NewPw).


get_list_to_know(_From, Cid) ->
	send_message_by_cid(Cid, {sensor, {self(), request_list_to_know}}),
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
	?assert(A1Cid == u:gen_cid(hibari,1)),
	?assert(A1Name == "alpha"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.


get_list_to_know_none_test() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{timeout, [], [], []} = get_list_to_know(self(), u:gen_cid(hibari,"cid_not_exist")),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.




%%----------------- Following are OLD CODE ---------------%%





%-----------------------------------------------------------
%% implement simple api for mmoasp.erl
%-----------------------------------------------------------

old_start() ->
	battle_observer:start_link(),
	db:start(reset_tables),
	path_finder:start().

old_start(reset_tables) ->
	battle_observer:start_link(),
	db:start(reset_tables),
	path_finder:start().

old_stop() ->
	battle_observer:stop(),
	path_finder:stop(),
	db:stop().
	
old_change_schema() ->
	db:drop_all(),
	db:do_this_once(),
	db:start(reset_tables).

old_make_params({get, A}) ->
	dict:from_list(yaws_api:parse_query(A));

old_make_params({post, A}) ->
	dict:from_list(yaws_api:parse_post(A)).

%% GET version of login. This is only for test with web browser.
%% This will be disabled soon.
old_out(A, 'GET', ["service", SVID, "login"]) ->
	Params = make_param({get, A}),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = login(self(),id_password:make_login_id(SVID, Id), Pw, Ipaddr),
	case Result of
		{ok, Cid, Token} ->
			mout:return_html(mout:encode_json_array_with_result("ok", [{cid, Cid}, {token, Token}]));
		{ng} ->
			mout:return_html(mout:encode_json_array_with_result("failed", []))
	end;


%% [test] stream I/F "GET http://localhost:8001/service/hibari/stream/listtoknow/cid1234"
old_out(A, 'GET', ["service", _SVID, "stream", "listtoknow", CID]) ->
	spawn(character_stream, start, [CID,A#arg.pid]),
	{streamcontent, "text/html", ""};

% create account.
old_out(A, 'POST', ["service", SVID, "create_account"]) ->
	Params = make_param({post, A}),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = old_create_account(self(), SVID, Id, Pw, Ipaddr),
	case Result of
		{atomic, ok} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{result, "ok"}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;


% create account.
old_out(A, 'POST', ["service", SVID, "delete_account"]) ->
	Params = make_param({post, A}),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = old_delete_account(self(), SVID, Id, Pw, Ipaddr),
	case Result of
		{atomic, ok} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{result, "ok"}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;


% Add New Player Character.
old_out(A, 'POST', ["service", SVID, "subscribe"]) ->
	Params = make_param({post, A}),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = old_create_account(self(), SVID, Id, Pw, Ipaddr),
	case Result of
		{atomic, ok} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{result, "ok"}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;
%% Login
%% Call "POST http://localhost:8002/service/hibari/login/  id=id0001&password=pw0001"
old_out(A, 'POST', ["service", SVID, "login"]) ->
	Params = make_param({post, A}),
	Id = param(Params, "id"),
	Pw = param(Params, "password"),
	{Ipaddr, _Port} = A#arg.client_ip_port,
	Result = login(self(),id_password:make_login_id(SVID, Id), Pw, Ipaddr),
	case Result of
		{ok, Cid, Token} ->
			mout:return_json(mout:encode_json_array_with_result("ok", [{cid, Cid}, {token, Token}]));
		{ng, Reason} ->
			mout:return_json(mout:encode_json_array_with_result("failed", [{reason, Reason}]))
	end;

%% Logout
%% Call "POST http://localhost:8001/service/hibari/logout/cid0001  token=Token"
old_out(A, 'POST', ["service", _SVID, "logout", CidX]) ->
	Params = make_param({post, A}),
	Token = param(Params, "token"),
	Result = old_logout(self(), CidX, Token),
	case Result of
		{ok, CidX} ->
			mout:return_json(mout:encode_json_array_with_result("ok",[]));
		{ng} ->
			mout:return_json(mout:encode_json_array_with_result("failed",[]))
	end;

%% Get list to know (Your client calls this every xx sec.)
%% Call "POST http://localhost:8002/service/hibari/listtoknow/cid0001  token=Token"
old_out(A, 'POST', ["service", _SVID, "listtoknow", CidX]) ->
	Params = make_param({post, A}),
	_Token = param(Params, "token"),
	send_message_by_cid(CidX, {self(), update_neighbor_status, default:distance()}),
	{list_to_know, ListToKnow, NeighborStats, MovePaths} = get_list_to_know(self(), CidX),
	mout:return_json(mout:struct_list_to_json(
		[{struct, X} || X <- ListToKnow]
		 ++
		 [{struct, X} || X <- NeighborStats]
		 ++
		 MovePaths
		 ));

%% Talk (open talk)
%% Call "POST http://localhost:8001/service/hibari/talk/cid1234  token=Token&talked=hello"
old_out(A, 'POST', ["service", _SVID, "talk", CidX]) ->
	Params = make_param({post, A}),
	_Token = param(Params, "token"),
	Talked = param(Params, "talked"),
	Result = talk(open, CidX, Talked, default:distance()),
	mout:return_json(json:encode({struct, [Result]}));

%% Whisper (person to person talk)
%% Call "POST http://localhost:8001/service/hibari/talk/cid1234/hello"
old_out(_A, 'POST', ["service", SVID, "whisper", CidX, _TalkTo, Talked]) ->
	not_implemented;

%% Move
%% Callr "POST http://localhost:8001/service/hibari/move/cid1234  token=Token&x=3&y=3"
old_out(A, 'POST', ["service", SVID, "move", CidX]) ->
	Params = make_param({post, A}),
	_Token = param(Params, "token"),
	X = erlang:list_to_integer(param(Params, "x")),
	Y = erlang:list_to_integer(param(Params, "y")),
	_Result = move:move({map_id, SVID, 1}, CidX, {pos, X, Y}),%%% TODO Write {map_id,SV,MAPID} appropriately !!
	mout:return_json(mout:encode_json_array_with_result("ok",[]));

%% Attack
old_out(A, 'POST', ["service", _SVID, "attack", CidX, CidTo]) ->
	Params = make_param({post, A}),
	_Token = param(Params, "token"),
	_Result = battle:single(CidX, CidTo),
	mout:return_json(mout:encode_json_array_with_result("ok",[]));

%% Set attribute
%% Call "GET http://localhost:8002/service/hibari/set/cid0001/KEY?value=VALUE"
old_out(A, 'GET', ["service", _SVID, "set", Cid, Key]) ->
	Params = make_param({get, A}),
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
old_out(_A, 'POST', ["service", _SVID, "startnpc", NpcidX]) ->
	npc:start_npc(NpcidX),
	mout:return_json(mout:encode_json_array_with_result("ok",[{"npcid", NpcidX}]));


% Add New Non Player Character.
old_out(A, 'POST', ["service", _SVID, "ping"]) ->
	Params = make_param({post, A}),
	AdminId = param(Params, "admin_id"),
	ConnectPhrase = param(Params, "connect_phrase"),
	mout:return_json(
		mout:encode_json_array_with_result(
			"ok",
			[{"status", ok},
			{"admin_id", AdminId},
			{"connect_phrase", ConnectPhrase}]));


%% sample for "catch all" handler.
old_out(A, _Method, _Params) ->
	io:format("out/3 general handler: A#arg.appmoddata = ~p~n"
		"A#arg.appmod_prepath = ~p~n"
		"A#arg.querydata = ~p~n",
		[A#arg.appmoddata,
		A#arg.appmod_prepath,
		A#arg.querydata]),
	{status, 404}.

%% dispacher for RESTful service (caller out/3)
old_out(A) ->
	{http_request, Req, _params, _unknown} = A#arg.req,
	Uri = yaws_api:request_url(A),
	Path = string:tokens(Uri#url.path, "/"),
	out(A, Req, Path).


old_param(ParamsDict, Key) ->
	case dict:find(Key, ParamsDict) of
		{ok, Value} -> Value;
		error -> void
	end.

-ifdef(TEST).
-endif.

%-----------------------------------------------------------
%% account management.
%-----------------------------------------------------------

old_delete_account(From, Svid, Id, Pw, Ipaddr) -> not_implemented.

old_create_account(From, Svid, Id, Pw, Ipaddr) ->
	mnesia:transaction(fun() ->
			foreach(fun mnesia:write/1, old_get_player_character_template(Id, Pw))
		end).

old_get_player_character_template(Id, Pass) ->
	Cid = "c" ++ Id,
	Name = "name" ++ Id,
	[
		{auth_basic, Cid, Id, Pass},
		{cdata, Cid, Name, [{"align", "neutral"}]},
		{location, Cid, 1, {pos, 1,3}, offline, offline},
		{money, Cid, 2000, 0}
	].

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
old_logout(From, Cid, _Token) ->
	old_setdown_player(Cid).


-ifdef(TEST).

old_login_test_notexec() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{ok, Cid3, Token3}
		= login(self(), "id0003", "pw0003", {192,168,1,200}),
	old_logout(self(), Cid3, Token1),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

old_login_duplicated_test_notexec() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	{ng, Reason} = login(self(), "id0001", "pw0001", {192,168,1,200}),
	?assert(Reason == "account is in use"),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

old_logout_missing_test_notexec() ->
	{scenarios, Cid1, Token1, Cid2, Token2, Npcid1} = test:up_scenarios(),
	
	old_logout(self(), "cid_not_exist", 0),
	
	test:down_scenarios({scenarios, Cid1, Token1, Cid2, Token2, Npcid1}),
	{end_of_run_tests}.

-endif.


%-----------------------------------------------------------
%% LIST to KNOW sender
%-----------------------------------------------------------

%-----------------------------------------------------------
% load and setup character for each login.
%-----------------------------------------------------------

old_setup_player(Cid) when is_record(Cid, cid)->
	online_character:connect(Cid).

old_do_setup_player(Cid, ExistingSession)
	when ExistingSession == {ng, "no such character"} ->

	R = setup_task_env(Cid),
	%% start player character process.
	Child = spawn(fun() -> character:loop(R, task:mk_idle_reset()) end),
	add_online_character(Cid, Child, "pc"),
	%% setup character states.
	setup_player_initial_location(Cid),
	%% notice login information to nearby.
	Name = character:get_name(Cid),
	notice_login(Cid, {csummary, Cid, Name}, default:distance()),
	{ok, Cid, R#task_env.token};

old_do_setup_player(Cid, ExistingSession) ->
	{ng, "account is in use"}.


old_setdown_player(Cid) ->
	old_do_setdown_player(Cid, online_character:get_one(Cid)).

old_do_setdown_player(Cid, ExistingSession)
	when ExistingSession == {ng, "no such character"} ->
		{ng, "no such character"};

old_do_setdown_player(Cid, ExistingSession) ->
	notice_logout(Cid, {csummary, Cid}, default:distance()),
	character:stop_child(Cid),
	online_character:stop_stream((online_character:get_one(Cid))#online_character.stream_pid),
	case delete_online_character(Cid) of
		{atomic, ok} -> {ok, Cid};
		Other -> Other
	end.


% *** charachter setup support functions. ***

setup_player_initial_location(Cid) when is_record(Cid, cid) ->
	L = initial_location:get_one(Cid),
	online_character:setpos(Cid, L).

add_online_character(Cid, Pid, _Type) when is_record(Cid, cid) ->
	O = online_character:set_pid(online_character:make_record(Cid),Pid),
	mnesia:activity(
		transaction,
		fun() -> mnesia:write(O) end).

delete_online_character(Cid) when is_record(Cid, cid) ->
	mnesia:transaction(
		fun()-> mnesia:delete({online_character, Cid}) end).

%-----------------------------------------------------------
% Character Persistency
%-----------------------------------------------------------

%old_lookup_cdata(Cid) ->
%	case db:do(qlc:q([X || X <- mnesia:table(cdata), X#cdata.cid == Cid])) of
%		[] -> void;
%		[CData] -> CData
%	end.
%old_gen_stat_from_cdata(X) ->
%	[{cid, X#cdata.cid}, {name, X#cdata.name}] ++ X#cdata.attr.

lookup_cdata(Cid) ->
	[].
gen_stat_from_cdata(AttrList) ->
	[{cid, 0}, {name, "dummy"}] ++ AttrList.




