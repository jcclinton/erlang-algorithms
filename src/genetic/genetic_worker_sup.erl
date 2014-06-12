-module(genetic_worker_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{genetic_worker,
					{genetic_worker, start_link, []},
					transient, 5000, worker, [genetic_server]}],
	{ok, {{simple_one_for_one, 3, 5}, Procs}}.
