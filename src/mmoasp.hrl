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
-record(cdata,	{cid, name, attr}).
%% oid is onigiri id. what's onigiri? onigiri is element. (object or sprite, character...)
-record(session, {oid, pid, type, name, map, x, y, z, stream_pid}). 
-record(location, {cid, initmap,initx, inity, initz}).

% ** Admin **
-record(service, {svid, adm_id, adm_pass, expire}).
-record(admin_session, {key, svid, adm_id, token, last_op_time}).

-record(id_next, {svid, next}).  %% Holds next CID. Increment when getter called.

% ** Authentication **
-record(auth_basic,	{cid, id, pass}). 

% ** Base **
-record(private_kv,	{cid, attr}).	%% private (hidden from other player) information(last window position, shortcut...

%-record(level, {cid, lv, exp}).
%-record(skill, {cid, list, rest_exp, used_exp}).
%-record(friends, {cid, list}).

% ** MMO style inventory: you can lookup them by character id **
-record(money,	{cid,  amount, offer}).
-record(supplies,	{id, cid, item_id, amount, offer}).
-record(estate,	{item_id, cid, is_offer}).
-record(trade,	{id, confirm_l, confirm_r}).
-record(u_trade, {cid, tid}).
-record(neighbors, {cid, list, updated}).

%% item master table.
%-record(m_item,		{id, data}).

%% Just records. Not for Mnesia
-record(inventory, {cid, money, supplies, estate}).


