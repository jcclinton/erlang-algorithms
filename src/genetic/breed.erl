-module(breed).

-export([breed/3]).

% takes fitness list and data, then selects parents, breeds and mutates children
breed(Fitnesses, MutationChance, Size) ->
	Length = length(Fitnesses),
	Parents = lists:foldl(fun(_, AccIn) ->
													{Fitness1, Fitnesses1} = fitness:select(Fitnesses),
													{Fitness2, _} = fitness:select(Fitnesses1),
													%io:format("out of all fitnesses: ~p~nselected 1: ~p~nselected 2: ~p~n", [Fitnesses, Fitness1, Fitness2]).
													[{Fitness1, Fitness2} | AccIn]
											end, [], lists:seq(1, Length div 2)),
	NewChildren = breed_new_children(Parents, Size),
	mutate:mutate_children(NewChildren, MutationChance, Size).





% breeds new children from selected parents
breed_new_children(Parents, Size) ->
	breed_new_children(Parents, Size, []).
breed_new_children([], _Size, Acc) -> Acc;
breed_new_children([{{_, ParentByte1}, {_, ParentByte2}}|Rest], Size, Acc) ->
	Chance = random:uniform(10),
	{NewChild1, NewChild2} = if Chance > 3 -> crossover:create_children_pair(ParentByte1, ParentByte2, Size);
		true -> {ParentByte1, ParentByte2}
	end,
	%io:format("p1: ~p p2: ~p~n", [ParentByte1, ParentByte2]),
	%io:format("c1: ~p c2: ~p~n", [NewChild1, NewChild2]),
	breed_new_children(Rest, Size, [NewChild1|[NewChild2|Acc]]).
