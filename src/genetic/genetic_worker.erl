-module(genetic_worker).


-export([start_link/2, run/2]).



start_link(ServerPid, Bytes) ->
	%io:format("starting worker with pid ~p and bytes ~p~n", [ServerPid, Bytes]),
	spawn_link(?MODULE, run, [ServerPid, Bytes]).


run(ServerPid, Bytes) ->
	%io:format("running worker~n"),
	gen_server:cast(ServerPid, success).
