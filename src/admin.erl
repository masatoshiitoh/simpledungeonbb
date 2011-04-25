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


-module(admin).
-compile(export_all).

-import(lists, [foreach/2]).

-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").




%% admnistration use.

get_service_entry(Svid, AdminId, AdminPw) ->
	case db:do(qlc:q([X
		|| X <- mnesia:table(service),
			X#service.svid =:= Svid,
			X#service.adm_id =:= AdminId,
			X#service.adm_pass =:= AdminPw])) of
		[] -> void;
		[X] -> X
	end.


admin_login(_From, Svid, AdminId, AdminPw, Ipaddr) ->
	Loaded = get_service_entry(Svid, AdminId, AdminPw),
	case Loaded of 
		{service, Svid, AdminId, AdminPw, _Expire} ->
			Token = uauth:gen_token(Ipaddr, AdminId),
			Now = erlang:now(),
			mnesia:transaction(
				fun() ->
					mnesia:write(#admin_session{key=Token, svid=Svid, adm_id=AdminId, token=Token, last_op_time=Now})
					end),
			{ok, AdminId, Token};
		void ->
			{ng, "admin_login: authentication failed"}
		end.

is_ok_admin_session(AdminId, Token) ->
	Result = db:do(qlc:q([AdmSess || 
			AdmSess <- mnesia:table(admin_session),	
			AdmSess#admin_session.adm_id =:= AdminId,
			AdmSess#admin_session.token =:= Token
			])),
	case Result of
		[_X] -> true;
		[] -> false
	end.


admin_delete_old_sessions(TimeoutSec) ->
	Now = erlang:now(),
	db:do(qlc:q([mnesia:delete({admin_session, AdmSess#admin_session.key}) || 
		AdmSess <- mnesia:table(admin_session),	
		timer:now_diff(Now, AdmSess#admin_session.last_op_time) > TimeoutSec * 1000000])).


admin_logout(_From, _Svid, _AdminId, Token, _Ipaddr) ->
	mnesia:transaction(fun() -> mnesia:delete({admin_session, Token}) end).


admin_list_users(_From, _Svid, AdminId, AdminToken, _Ipaddr) ->
	case is_ok_admin_session(AdminId, AdminToken) of
		true -> 0;
		false -> {ng, session_timed_out}
	end.


