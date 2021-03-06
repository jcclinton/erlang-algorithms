-module(genetic_server).
-behavior(gen_server).

-record(state, {
				n,
				remaining,
				results,
				target,
				generation
			 }).


-export([start_link/0, run/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

% size of our chromosome in bytes
-define(Size, 4).
% 1 in MutationChance that a mutaton will occur
-define(MutationChance, 1000).
% number of generations algorithm runs before it ends
-define(End, 100).

run() ->
	gen_server:cast(genetic_server, create_first_gen).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed(A,B,C),
	io:format("genetic SERVER: started~n"),
	% number of workers to spawn at each step
	N = 10,
	% target value we are trying to get
	Target = 42,
	{ok, #state{n=N, target=Target}}.


handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast({success, Result}, State=#state{remaining=Remaining, n=N, results=Results}) ->
	NewRemaining = Remaining + 1,
	CombinedResults = [Result|Results],
	NewResults = if NewRemaining >= N ->
			%io:format("all received success~n"),
			gen_server:cast(self(), {breed, CombinedResults}),
			[];
		true -> CombinedResults
	end,
	{noreply, State#state{remaining=NewRemaining, results=NewResults}};
handle_cast({breed, Results}, State=#state{target=Target, generation=Generation}) ->
	Done = lists:foldl(fun({Num, _}, Bool) ->
			%io:format("number: ~p~n", [Num]),
							if Bool -> Bool;
								Num == Target -> true;
								true -> false
							end
				end, false, Results),
	if Done ->
			io:format("~p found after ~p generations~n", [Target, Generation]);
		Generation > ?End ->
			io:format("did not find after ~p generations~n", [Generation]);
		true ->
			%io:format("input parents: ~p~n", [Results]),
			Fitness = fitness:get_fitness(Results, Target),
			Total = fitness:get_fitness_total(Fitness),
			if Generation rem 1000 == 1 ->
					io:format("breeding generation ~p with total fitness: ~p~n", [Generation, Total]);
				true -> ok
			end,
			Children = breed:breed(Fitness, ?MutationChance, ?Size),
			gen_server:cast(self(), {next_step, Children})
			%io:format("output children: ~p~n", [Children]),
	end,
	{noreply, State#state{generation=Generation+1, results=[], remaining=0}};
handle_cast({next_step, Children}, State) ->
	lists:foreach(fun(Bytes) ->
		supervisor:start_child(genetic_worker_sup, [Bytes])
	end, Children),
	{noreply, State#state{}};
handle_cast(create_first_gen, State=#state{n=N}) ->
	lists:foreach(fun(_) ->
		RandBytes = crypto:strong_rand_bytes(?Size),
		<<_:4/integer, First:4/integer, Rest/binary>> = RandBytes,
		%% hardcode first four bytes to 15 so they get ignored
		Bytes = <<15:4/integer, First:4/integer, Rest/binary>>,
		supervisor:start_child(genetic_worker_sup, [Bytes])
	end, lists:seq(1, N)),
	{noreply, State#state{remaining=0, results=[], generation=0}};
handle_cast(Msg, State) ->
	io:format("genetic SERVER: received unknown cast: ~p~n", [Msg]),
	{noreply, State}.



handle_info(Msg, State) ->
	io:format("genetic SERVER: received unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.
