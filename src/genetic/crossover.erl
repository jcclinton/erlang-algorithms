-module(crossover).

-export([create_children_pair/3]).

-define(bit, :1/unsigned-integer).

%% crossover function
create_children_pair(Bytes1, Bytes2, Size) ->
	CrossoverLength = random:uniform(Size),
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
			
