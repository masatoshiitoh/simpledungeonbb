-module(battle_observer).
-behaviour(gen_server).

-export([set_one/3, start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%%===============================


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_one(CidFrom, CidTo, Result) ->
	gen_server:call(?MODULE, {store, CidFrom, CidTo, Result}, 20000).



%%===============================

init([]) ->
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	{ok, 0}.

handle_call({store, CidFrom, CidTo, Result}, _From, N) ->
	{reply, handle_battle(CidFrom, CidTo, Result), N+1}.

handle_cast(_Msg, N) -> {noreply, N}.
handle_info(_Info, N) -> {noreply, N}.

terminate(_Reason, _N) ->
	io:format("~p stoppig~n", [?MODULE]).

code_change(_OldVsn, N, _Extra) -> {ok, N}.

handle_battle(CidFrom, CidTo, Result) ->
	0.


