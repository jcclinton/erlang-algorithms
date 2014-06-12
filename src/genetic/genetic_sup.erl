-module(genetic_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	Procs = [{genetic_server,
						{genetic_server, start_link, []},
						transient, 5000, worker, [genetic_server]},
					{genetic_worker_sup,
						{genetic_worker_sup, start_link, []},
						transient, 5000, worker, [genetic_worker_sup]}
					],
	{ok, {{one_for_all, 2, 5}, Procs}}.
