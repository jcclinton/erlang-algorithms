-module(genetic_worker_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(ServerPid) ->
	supervisor:start_link(?MODULE, ServerPid).

init(ServerPid) ->
	Procs = [{genetic_worker,
					{genetic_worker, start_link, [ServerPid]},
					transient, 5000, worker, [genetic_server]}],
	{ok, {{simple_one_for_one, 3, 5}, Procs}}.
