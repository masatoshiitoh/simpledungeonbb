-module(ybed).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).

-compile(export_all).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([run/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
	run(simpledungeon_sup).

run([]) ->
	run(simpledungeon_sup);

run(SupRef) ->
	Id = "simpledungeon",
	GconfList = [
%%		{logdir, "./test/log"},
%%		{ebin_dir, [
%%			".", "../ebin"
%%			]},
		{id, Id}],

	Docroot = "../docroot",

	SconfList = [
		{port, 8002},
		{servername, "mmoasp"},
		{listen, {0,0,0,0}},
		{docroot, Docroot},
		{appmods, [{"/service", mmoasp}]}
	],
io:format("ybed: run: embedded_start_conf ~p~n", [
	{ok, SCList, GC, ChildSpecs} =
		yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id)
]),

io:format("ybed: run: start_child ok ~p~n", [
	[supervisor:start_child(SupRef, Ch) || Ch <- ChildSpecs]
]),

	%% now configure Yaws
	
io:format("ybed: run: setconf ok ~p~n", [
	yaws_api:setconf(GC, SCList)
]),

	{ok, self()}.

