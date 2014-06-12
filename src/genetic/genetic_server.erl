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
-compile([export_all]).

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
			Fitness = get_fitness(Results, Target),
			Total = get_fitness_total(Fitness),
			if Generation rem 1000 == 1 ->
					io:format("breeding generation ~p with total fitness: ~p~n", [Generation, Total]);
				true -> ok
			end,
			Children = breed(Fitness),
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


%% private

% calculates fitness from the calculated results
get_fitness(Results, Target) ->
	get_fitness(Results, Target, []).
get_fitness([], _Target, Acc) -> Acc;
get_fitness([{Num, Bytes}|Rest], Target, Acc) ->
	Fitness = erlang:round(10000 * erlang:abs(1 / (Target - Num))),
	get_fitness(Rest, Target, [{Fitness, Bytes}|Acc]).


% takes fitness list and data, then selects parents, breeds and mutates children
breed(Fitnesses) ->
	Length = length(Fitnesses),
	Parents = lists:foldl(fun(_, AccIn) ->
													{Fitness1, Fitnesses1} = select(Fitnesses),
													{Fitness2, _} = select(Fitnesses1),
													%io:format("out of all fitnesses: ~p~nselected 1: ~p~nselected 2: ~p~n", [Fitnesses, Fitness1, Fitness2]).
													[{Fitness1, Fitness2} | AccIn]
											end, [], lists:seq(1, Length div 2)),
	NewChildren = breed_new_children(Parents),
	mutate:mutate_children(NewChildren, ?MutationChance, ?Size).





% breeds new children from selected parents
breed_new_children(Parents) ->
	breed_new_children(Parents, []).
breed_new_children([], Acc) -> Acc;
breed_new_children([{{_, ParentByte1}, {_, ParentByte2}}|Rest], Acc) ->
	Chance = random:uniform(10),
	{NewChild1, NewChild2} = if Chance > 3 -> crossover:create_children_pair(ParentByte1, ParentByte2, ?Size);
		true -> {ParentByte1, ParentByte2}
	end,
	%io:format("p1: ~p p2: ~p~n", [ParentByte1, ParentByte2]),
	%io:format("c1: ~p c2: ~p~n", [NewChild1, NewChild2]),
	breed_new_children(Rest, [NewChild1|[NewChild2|Acc]]).





% randomly selects a byte, the higher its fitness, the higher chance it has of being selected
select(Fitnesses) ->
	Total = get_fitness_total(Fitnesses),
	Random = random:uniform(Total),
	{_, Fitness, NewFitnesses} =
		lists:foldl(fun({Num, _}=Fit, {Floor, Found, NewFitnesses}) ->
			Ceil = Floor + Num,
			if Found == null ->
					if Random > Floor andalso Random =< Ceil ->
							{Ceil, Fit, NewFitnesses};
						true ->
							NewFits = [Fit|NewFitnesses],
							{Ceil, null, NewFits}
					end;
				true ->
					NewFits = [Fit|NewFitnesses],
					{Ceil, Found, NewFits}
			end
		end, {0, null, []}, Fitnesses),
	{Fitness, NewFitnesses}.
															


% calculates the combined fitness score of all the bytes
get_fitness_total(Fitnesses) ->
	get_fitness_total(Fitnesses, 0).
get_fitness_total([], Total) -> Total;
get_fitness_total([{Num, _}|Rest], Total) ->
	get_fitness_total(Rest, Total + Num).
