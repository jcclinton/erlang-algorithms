-module(genetic_server).
-behavior(gen_server).

-record(state, {
				n,
				worker_sup_pid,
				remaining,
				results,
				target,
				generation
			 }).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-compile([export_all]).

% size of our chromosome in bytes
-define(Size, 4).
% 1 in MutationChance that a mutaton will occur
-define(MutationChance, 1000).
% number of generations algorithm runs before it ends
-define(End, 10000).
-define(bit, :1/unsigned-integer).


start_link(ParentPid) ->
	gen_server:start_link(?MODULE, ParentPid, []).

init(ParentPid) ->
	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed(A,B,C),
	io:format("genetic SERVER: started~n"),
	gen_server:cast(self(), {init, ParentPid}),
	N = 10,
	Target = 42,
	{ok, #state{n=N, target=Target}}.


handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast({init, ParentPid}, State) ->
	Name = genetic_worker_sup,
	ChildSpec = {Name, {Name, start_link, [self()]}, permanent, 10000, supervisor, [Name]},
	{ok, WorkerSupPid} = supervisor:start_child(ParentPid, ChildSpec),
	gen_server:cast(self(), create_first_gen),
	{noreply, State#state{worker_sup_pid=WorkerSupPid}};
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
handle_cast({next_step, Children}, State=#state{worker_sup_pid=Pid}) ->
	lists:foreach(fun(Bytes) ->
		supervisor:start_child(Pid, [Bytes])
	end, Children),
	{noreply, State#state{}};
handle_cast(create_first_gen, State=#state{worker_sup_pid=Pid, n=N}) ->
	lists:foreach(fun(_) ->
		RandBytes = crypto:strong_rand_bytes(?Size),
		<<_:4/integer, First:4/integer, Rest/binary>> = RandBytes,
		%% hardcode first four bytes to 15 so they get ignored
		Bytes = <<15:4/integer, First:4/integer, Rest/binary>>,
		supervisor:start_child(Pid, [Bytes])
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
	mutate_children(NewChildren).


% iterates through children and may mutate
mutate_children(Children) ->
	mutate_children(Children, []).
mutate_children([], Acc) -> Acc;
mutate_children([Child|Rest], Acc) ->
	Chance = random:uniform(?MutationChance),
	NewChild = if Chance == 1 ->
		%io:format("mutating: ~p~n", [Child]),
		mutate(Child);
							true -> Child
						end,
		%io:format("mutated: ~p~n", [NewChild]),
	mutate_children(Rest, [NewChild|Acc]).

% mutates one byte in a 5 byte word
% kind of ugly
mutate(<<B1:8, B2:8, B3:8, B4:8>>) ->
	Offset = random:uniform(?Size),
	NewB1 = if Offset == 1 -> mutate_byte(B1);
							true -> B1
					end,
	NewB2 = if Offset == 2 -> mutate_byte(B2);
							true -> B2
					end,
	NewB3 = if Offset == 3 -> mutate_byte(B3);
							true -> B3
					end,
	NewB4 = if Offset == 4 -> mutate_byte(B4);
							true -> B4
					end,
	<<NewB1:8, NewB2:8, NewB3:8, NewB4:8>>.


%% flips one bit in a byte
mutate_byte(Byte) ->
	BitOffset = random:uniform(8),
	BitMask = 1 bsl (BitOffset-1),
	Odd = Byte band BitMask,
	if Odd == 1 -> Byte band (bnot BitMask);
		true -> Byte bor BitMask
	end.




% breeds new children from selected parents
breed_new_children(Parents) ->
	breed_new_children(Parents, []).
breed_new_children([], Acc) -> Acc;
breed_new_children([{{_, ParentByte1}, {_, ParentByte2}}|Rest], Acc) ->
	Chance = random:uniform(10),
	{NewChild1, NewChild2} = if Chance > 3 -> create_children_pair(ParentByte1, ParentByte2);
		true -> {ParentByte1, ParentByte2}
	end,
	%io:format("p1: ~p p2: ~p~n", [ParentByte1, ParentByte2]),
	%io:format("c1: ~p c2: ~p~n", [NewChild1, NewChild2]),
	breed_new_children(Rest, [NewChild1|[NewChild2|Acc]]).


%% crossover function
create_children_pair(Bytes1, Bytes2) ->
	CrossoverLength = random:uniform(?Size),
	SpecificCrossoverLength = random:uniform(8),
	Length = size(Bytes1),
	{Child1, Child2} = lists:foldl(fun(I, {Child1, Child2}) ->
			%io:format("iter: ~p: byte1: ~p byte2: ~p~n", [I, Bytes1, Bytes2]),
			Offset = I - 1,
			OffsetBits = Offset * 8,
			<<_:OffsetBits/unsigned-integer, B1:8/unsigned-integer, _/binary>> = Bytes1,
			<<_:OffsetBits/unsigned-integer, B2:8/unsigned-integer, _/binary>> = Bytes2,
			if I == CrossoverLength ->
					{NewB1, NewB2} = crossover(B1, B2, SpecificCrossoverLength),
					NewChild1 = <<Child1/binary, NewB2/binary>>,
					NewChild2 = <<Child2/binary, NewB1/binary>>,
					{NewChild1, NewChild2};
				I > CrossoverLength ->
					NewChild1 = <<Child1/binary, B2:8>>,
					NewChild2 = <<Child2/binary, B1:8>>,
					{NewChild1, NewChild2};
				true ->
					NewChild1 = <<Child1/binary, B1:8>>,
					NewChild2 = <<Child2/binary, B2:8>>,
					{NewChild1, NewChild2}
			end
	end, {<<>>, <<>>}, lists:seq(1, Length)),
	{Child1, Child2}.


%% crossover one byte into another
%% very ugly
crossover(B1, B2, Length) ->
	<<N11?bit,N12?bit,N13?bit,N14?bit,N15?bit,N16?bit,N17?bit,N18?bit>> = <<B1:8/unsigned-integer>>,
	<<N21?bit,N22?bit,N23?bit,N24?bit,N25?bit,N26?bit,N27?bit,N28?bit>> = <<B2:8/unsigned-integer>>,
	case Length of
		1 ->
			NB1 = <<N11?bit,N22?bit,N23?bit,N24?bit,N25?bit,N26?bit,N27?bit,N28?bit>>,
			NB2 = <<N21?bit,N12?bit,N13?bit,N14?bit,N15?bit,N16?bit,N17?bit,N18?bit>>,
			{NB1, NB2};
		2 ->
			NB1 = <<N11?bit,N12?bit,N23?bit,N24?bit,N25?bit,N26?bit,N27?bit,N28?bit>>,
			NB2 = <<N21?bit,N22?bit,N13?bit,N14?bit,N15?bit,N16?bit,N17?bit,N18?bit>>,
			{NB1, NB2};
		3 ->
			NB1 = <<N11?bit,N12?bit,N13?bit,N24?bit,N25?bit,N26?bit,N27?bit,N28?bit>>,
			NB2 = <<N21?bit,N22?bit,N23?bit,N14?bit,N15?bit,N16?bit,N17?bit,N18?bit>>,
			{NB1, NB2};
		4 ->
			NB1 = <<N11?bit,N12?bit,N13?bit,N14?bit,N25?bit,N26?bit,N27?bit,N28?bit>>,
			NB2 = <<N21?bit,N22?bit,N23?bit,N24?bit,N15?bit,N16?bit,N17?bit,N18?bit>>,
			{NB1, NB2};
		5 ->
			NB1 = <<N11?bit,N12?bit,N13?bit,N14?bit,N15?bit,N26?bit,N27?bit,N28?bit>>,
			NB2 = <<N21?bit,N22?bit,N23?bit,N24?bit,N25?bit,N16?bit,N17?bit,N18?bit>>,
			{NB1, NB2};
		6 ->
			NB1 = <<N11?bit,N12?bit,N13?bit,N14?bit,N15?bit,N16?bit,N27?bit,N28?bit>>,
			NB2 = <<N21?bit,N22?bit,N23?bit,N24?bit,N25?bit,N26?bit,N17?bit,N18?bit>>,
			{NB1, NB2};
		7 ->
			NB1 = <<N11?bit,N12?bit,N13?bit,N14?bit,N15?bit,N16?bit,N17?bit,N28?bit>>,
			NB2 = <<N21?bit,N22?bit,N23?bit,N24?bit,N25?bit,N26?bit,N27?bit,N18?bit>>,
			{NB1, NB2};
		8 ->
			NB1 = <<N11?bit,N12?bit,N13?bit,N14?bit,N15?bit,N16?bit,N17?bit,N18?bit>>,
			NB2 = <<N21?bit,N22?bit,N23?bit,N24?bit,N25?bit,N26?bit,N27?bit,N28?bit>>,
			{NB1, NB2}
		end.
			



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
