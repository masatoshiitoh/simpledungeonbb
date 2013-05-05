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


-module(actor_pool_mgr).
-behaviour(gen_server).

-export([start/4, start_link/4, stop/1]).
-export([run/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SPEC(MFA),
	{actor_sup,
		{actor_sup, start_link, [MFA]},
		permanent,
		10000,
		supervisor,
		[actor_sup]}).

-record(state, {limit = 0, actor_sup, refs}).

%%
%% APIs
%%

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
	gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
	gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

stop(Name) ->
	gen_server:call(Name, stop).

run(Name, Args) ->
	gen_server:call(Name, {run, Args}).

%%
%% Callback functions
%%

init({Limit, MFA, Sup}) ->
	self() ! {start_actor_supervisor, Sup, MFA},
	{ok, #state{limit = Limit, refs = gb_sets:empty()}}.

handle_info({'DOWN', Ref, process, Pid, _}, S = #state{limit = L, actor_sup = Sup, refs = Refs}) ->
	case gb_sets:is_element(Ref, Refs) of
		true ->
			{noreply, S#state{limit = L + 1, refs = gb_sets:delete(Ref, Refs)}};
		false ->
			{noreply, S}
		end;

handle_info({start_actor_supervisor, Sup, MFA}, S = #state{}) ->
	{ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
	{noreply, S#state{actor_sup=Pid}};

handle_info(Msg, State) ->
	io:format("Unknown msg: ~p~n", [Msg]),
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call({run, Args}, _From, S = #state{limit=N, actor_sup=Sup, refs=R}) when N > 0 ->
	{ok, Pid} = supervisor:start_child(Sup, Args),
	Ref = erlang:monitor(process, Pid),
	{reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};

handle_call({run, Args}, _From, S = #state{limit=N}) when N =< 0 ->
	{reply, noalloc, S};

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

