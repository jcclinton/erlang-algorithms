-module(genetic_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	Procs = [{genetic_server,
					{genetic_server, start_link, [self()]},
					permanent, 5000, worker, [genetic_server]}],
	{ok, {{one_for_all, 0, 1}, Procs}}.
