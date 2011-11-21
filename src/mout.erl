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


-module(mout).
-include("yaws_api.hrl").
-export([encode_json_array_with_result/2, return_html/1, return_json/1, object_list_to_json/1]).
-compile(export_all).

%% mout.erl :
%% MMOASP Module - JSON output support.

return_json(Json) -> {content, "application/json; charset=utf-8", Json}.

return_html(Json) -> {content,  "text/html; charset=utf-8", Json}.

encode_json_array_with_result(Result, L) ->
	json:encode(json_result_object(Result, L)).

object_list_to_json(L) when is_list(L) ->
	json:encode(json_array([json_object(X) || X <- L])).

struct_list_to_json(L) when is_list(L) ->
	json:encode(json_array(L)).

json_result_object(Result, L) when is_list(L) ->
	{struct, [{result, Result}] ++ L}.

json_object(PropList) when is_list(PropList) ->
	{struct, PropList}.

json_array(Array) when is_list(Array) ->
	{array, Array}.

%%list_to_xml(DATA, RootName)
%%	->xmerl:export_simple(prepare_xmerl(DATA, RootName), xmerl_xml).

%data_to_json(DATA)
%	->json:encode(prepare_json(DATA)).

%%
%%
%%

%%prepare_json(DATA) ->{struct, DATA}.
%%prepare_xmerl(DATA, NAME) ->[{to_atom(NAME), [ {to_atom(K),[V]} || {K,V} <- DATA]}].

to_atom(K) when is_atom(K) -> K;
to_atom(K) -> list_to_atom(K).




%% mout:wrap_jsontype([[ {myoji, "yamada"}, {name, "taro"} ], [ {myoji, "yamada"}, {name, "taro"} ]]).

%%[ {myoji, "yamada"}, {name, "taro"} ] -> {struct , [ {myoji, "yamada"}, {name, "taro"} ]}

%% wrap_jsontye aims to replace 'prepare_json/1'.

%wrap_jsontype([]) ->
%	[];
%
%wrap_jsontype([H|T]) when is_number(H) -> %% given list must be a STRING.
%	[H | T];	%% return STRING itself.
%
%wrap_jsontype([{struct, A} | T]) ->
%	{array, [{struct, A}, wrap_jsontype(T)]};
%
%wrap_jsontype(T) ->
%	{struct, T}.

%add_element_to_hash({struct, ElementList}, {K,V}) ->
%	{strucct, [{K,V} | ElementList]}.
%
%add_hash_to_array({array, Array}, {struct, H}) ->
%	{array, [{struct, E} | Array]}.



