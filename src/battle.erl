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


-module(battle).

%% this module is battle interface.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("mmoasp.hrl").
-compile(export_all).

-ifdef(TEST).
-endif.

%% full automatic battle
once(OidFrom, OidTo) ->
	Method = get_default_battle_method(OidFrom),
	case Method of
		"hth" -> {ok, 0};
		"missile" -> {ok, 0};
		"magic" -> {ok, 0};
		_ ->
			unarmed:calc(
				get_battle_parameter(OidFrom, Method),
				get_battle_parameter(OidTo, Method))
	end.

%% player choose attack method
%% hth: hand-to-hand. nuckle, katana...
%% missile: arrow, gun, rocket launcher...
%% magic: like tiltowait of wizardry,
%%        and any other special resource consuming.
once(OidFrom, OidTo, Method) ->
	case Method of
		"hth" -> {ok, 0};
		"missile" -> {ok, 0};
		"magic" -> {ok, 0};
		_ ->
			unarmed:calc(
				get_battle_parameter(OidFrom, Method),
				get_battle_parameter(OidTo, Method))
	end.

get_battle_parameter(Oid, Method) ->
	case Method of
		"unarmed" ->
			#battle_param{oid = Oid, range=1.0, hp = 10, mp = 0, ac = 3, str = 5}
	end.

get_default_battle_method(Oid) ->
	%% check equipment.
	%% 
	"unarmed".

