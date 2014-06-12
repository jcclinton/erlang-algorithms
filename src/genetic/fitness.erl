-module(fitness).

-export([get_fitness/2, select/1, get_fitness_total/1]).

% calculates fitness from the calculated results
get_fitness(Results, Target) ->
	get_fitness(Results, Target, []).
get_fitness([], _Target, Acc) -> Acc;
get_fitness([{Num, Bytes}|Rest], Target, Acc) ->
	Fitness = erlang:round(10000 * erlang:abs(1 / (Target - Num))),
	get_fitness(Rest, Target, [{Fitness, Bytes}|Acc]).







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
