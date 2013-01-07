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


%% Mnesia table structure

%% under development:
%-record(task_env, {cid, cdata, event_queue, stat_dict, token, utimer, waypoints = [], currpos = undefined}).
%-record(task_env, {cid,        event_queue, stat_dict, token, utimer, waypoints = [], currpos = undefined}).
-record( task_env, {cid,        event_queue, stat_dict, token, utimer, waypoints = [], currpos = undefined, move_path_dict = dict:new()}).
-record(idle,  {since_last_op = 0, last_op = erlang:now()}).

-record(cdata,	{cid, name, attr}).
-record(session, {cid, pid, type, name = "", map = 0, x = 0, y = 0, z = 0, stream_pid}). 
-record(location, {cid, initmap,initx, inity, initz}).

-record(battle_param, {cid, hp, mp, ac, str, range}).

% ** Admin **
-record(service, {svid, adm_id, adm_pass, expire}).
-record(admin_session, {key, svid, adm_id, token, last_op_time}).

-record(id_next, {svid, next}).  %% Holds next CID. Increment when getter called.

% ** Authentication **
-record(auth_basic,	{cid, id, pass}). 

% ** Base **
-record(private_kv,	{cid, attr}).	%% private (hidden from other player) information(last window position, shortcut...

-record(neighbors, {cid, list, updated}).

