-module(genetic_worker).


-export([start_link/1]).



start_link(ServerPid) ->
	spawn_link(?MODULE, fun run/1, [ServerPid]).


run(ServerPid) ->
	ServerPid ! success.
