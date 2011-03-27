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
-export([encode_json_array_with_result/2, return_html/1, return_json/1, list_to_json/1, list_to_xml/2]).
-compile(export_all).

%% MMOASP Module - XML/JSON output support.

return_json(Json) -> {content, "application/json; charset=utf-8", Json}.

return_html(Json) -> {content,  "text/html; charset=utf-8", Json}.

encode_json_array_with_result(Result, L) ->
	json:encode({struct, [{result, Result}] ++ L}).


list_to_json(L) ->
	L2 = [{struct, X} || X <- L],
	json:encode({array, L2}).


list_to_xml(DATA, RootName)
	->xmerl:export_simple(prepare_xmerl(DATA, RootName), xmerl_xml).

data_to_json(DATA)
	->json:encode(prepare_json(DATA)).

%%
%%
%%

prepare_json(DATA) ->{struct, DATA}.
prepare_xmerl(DATA, NAME) ->[{to_atom(NAME), [ {to_atom(K),[V]} || {K,V} <- DATA]}].

to_atom(K) when is_atom(K) -> K;
to_atom(K) -> list_to_atom(K).


