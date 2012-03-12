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

-record(battle_param, {cid, hp, mp, ac, str, range}).

%% Common record.

-record(uid,      {service_name, id}).
-record(cid,      {service_name, id}).
-record(map_id,   {service_name, id}).
-record(login_id, {service_name, id}).
-record(location, {map_id, x, y}).

-record(notice,   {cid, type, value}).
-record(talk,     {cid, speaker_name, message}).

-record(request,  {client_ip, port, version, service_name, action, id1, id2, id3}).
-record(list_to_know, {actions, stats, move_paths}).



-record(task_env, {cid,
	event_queue,
	stat_dict,
	token,
	utimer,
	waypoints = [],
	currpos = undefined,
	move_path_dict = dict:new()}).

-record(idle,  {since_last_op = 0, last_op = erlang:now()}).

%% Mnesia table structure

-record(service,
	{service_name, conn_phrase, id_list, expire_date}).
	%% conn_phrase is just for a fail-safe (typo service name, just so on.).
	%% because conn_phrase is embedded in client program, and must be insecure.
	%% id_list will be removed.

-record(admin,
	{uid, crypted_password, attributes}).
	%% attributes holds a dictionary.  developers can store parameters into it.
	%% (ex. email, phone, facebook id,....)

-record(character,
	{cid, type, name, inventory, status, hidden_parameters}).
	%% inventory and status : you can store parameters.
	%% hidden_parameters : storage for erlang code.

-record(online_character,
	{cid, type, map_id, location, last_update, pid, stream_pid}).
	%% session holds online characters.
	%% map_id is included also in location. why? Help to select same map players.
	%% expired sessions will be removed by remover service (we must make it).

-record(initial_location,
	{cid, location}).

-record(session,
	{cid, token, expire_at}).
	%% session holds session token.
	%% expired sessions will be removed by remover service (we must make it).

-record(id_password,
	{login_id, password, cid}).

