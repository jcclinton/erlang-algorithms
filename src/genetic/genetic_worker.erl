-module(genetic_worker).


-export([start_link/1, run/1]).



start_link(Bytes) ->
	%io:format("starting worker with bytes ~p~n", [Bytes]),
	spawn_link(?MODULE, run, [Bytes]).


run(Bytes) ->
	Num = calculation:run(Bytes),
	%% send back result and the bytes
	gen_server:cast(genetic_server, {success, {Num, Bytes}}).
