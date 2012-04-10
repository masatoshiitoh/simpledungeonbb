-module(player_char).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

-export([stop/1,
	you_killed/1,
	respawn_confirmation/1,
	force_fail/2,
	move/2,
	talk/2,
	logout/1,
	back_to_online/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1,
	
	idle/2,
	killed/2,
	logged_out/2,
	
	handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3,
    code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% start_link returns Pid. you must record it, because this start_link doesnt register its name.
start_link(Cid) ->
    gen_fsm:start_link(?MODULE, [{cid, Cid}], []).

stop(Ref) when is_pid(Ref) -> gen_fsm:send_all_state_event(Ref, stop).

%% --------------------
%%  user operation
%% --------------------

force_fail(Ref, fail_it) when is_pid(Ref) ->
    gen_fsm:send_event(Ref, force_fail).

move(Ref, {pos, X, Y}) when is_pid(Ref) ->
    gen_fsm:send_event(Ref, {move, {pos, X, Y}}).

talk(Ref, Talked) when is_pid(Ref) ->
    gen_fsm:send_event(Ref, {talk, Talked}).

logout(Ref) when is_pid(Ref) ->
    gen_fsm:send_event(Ref, {logout}).

respawn_confirmation(Ref) when is_pid(Ref) ->
    gen_fsm:send_event(Ref, {respawn_confirmed}).

back_to_online(Ref) when is_pid(Ref) ->
    gen_fsm:send_event(Ref, {back_to_online}).

%% --------------------
%%  result of others action
%% --------------------

you_killed(Ref) when is_pid(Ref) ->
    gen_fsm:send_event(Ref, {you_killed}).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([{cid, Cid}]) ->
    {ok, idle, make_init_state({cid, Cid})}.

idle(timeout, State) ->
	io:format("idle timed out ~p~n",[State]),
	{next_state, logged_out, State};

idle(Event, State) ->
	case Event of
		{you_killed} ->
			io:format("killed ~p~n",[State]),
			%% notify
			{next_state, killed, State};
		{logout} ->
			io:format("logged_out ~p~n",[State]),
			%% request to parent : logout
			{next_state, logged_out, State};
		{move, Pos} ->
			io:format("move ~p (State = ~p)~n",[Pos, State]),
			%% reset timeout timer.
			{next_state, idle, State, 30000};
		{talk, Talked} ->
			io:format("talk ~p (State = ~p)~n",[Talked, State]),
			%% reset timeout timer.
			{next_state, idle, State, 30000};
		{force_fail, fail_it} ->
			io:format("force_fail (State = ~p)~n",[State]),
			x = not_x,
			%% reset timeout timer.
			{next_state, idle, State, 30000};
		{do_proc} ->
			io:format("do_proc ~p~n",[State]),
			%% reset timeout timer.
			{next_state, idle, State, 30000}
	end.




killed(Event, State) ->
	case Event of
		{respawn_confirmed} ->
			io:format("respawn confirmed ~p~n",[State]),
			%% request to parent : move to default location
			{next_state, idle, State}
	end.

logged_out(Event, State) ->
	case Event of
		{back_to_online} ->
			io:format("back to online ~p~n",[State]),
			%% request to parent : move to default location
			{next_state, idle, State}
	end.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

make_init_state({cid, Cid}) ->
	 [{cid, Cid}, {list_to_know, []}, {move_path, []}].

