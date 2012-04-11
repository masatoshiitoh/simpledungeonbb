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


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    {ok, spawn(?MODULE, run, [])}.


run() ->
	Id = "simpledungeon",

	GconfList = [
		{logdir, "./test/log"},
		{ebin_dir, [".","../ebin"]},
		{id, Id}],

	Docroot = "../docroot",

	SconfList = [
		{port, 8002},
		{listen, {0,0,0,0}},
		{docroot, Docroot},
		{appmods, [{"/service", mmoasp}]}
	],

	{ok, SCList, GC, ChildSpecs} =
	    yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

	[supervisor:start_child(?MODULE, Ch) || Ch <- ChildSpecs],

	%% now configure Yaws
	yaws_api:setconf(GC, SCList),
	{ok, self()}.

