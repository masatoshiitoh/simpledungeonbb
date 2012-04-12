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
    {ok, spawn_link(?MODULE, run, [])}.


run() ->
	Id = "simpledungeon",

	GconfList = [
		{logdir, "./test/log"},
		{ebin_dir, [
			".",
			"../ebin",
			"c:/opt/yaws/ebin"
			]},
		{id, Id}],

	Docroot = "../docroot",

	SconfList = [
		{port, 8002},
		{listen, {0,0,0,0}},
		{docroot, Docroot},
		{appmods, [
			{"/service", mmoasp}]}
	],
io:format("ybed: run: initialize~n", []),
	{ok, SCList, GC, ChildSpecs} =
		%% yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
	    yaws_api:embedded_start_conf(Docroot),

io:format("ybed: run: embedded_start_conf ok~n", []),

	[supervisor:start_child(?MODULE, Ch) || Ch <- ChildSpecs],
io:format("ybed: run: start_child ok~n", []),

	%% now configure Yaws
	yaws_api:setconf(GC, SCList),
io:format("ybed: run: setconf ok~n", []),

	{ok, self()}.

