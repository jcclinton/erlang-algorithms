-module(mutate).

-export([mutate_children/3]).

% iterates through children and may mutate
mutate_children(Children, Chance, Size) ->
	mutate_children(Children, Chance, Size, []).
mutate_children([], _Chance, _Size, Acc) -> Acc;
mutate_children([Child|Rest], Chance, Size, Acc) ->
	Rand = random:uniform(Chance),
	NewChild = if Rand == 1 ->
		%io:format("mutating: ~p~n", [Child]),
		mutate(Child, Size);
							true -> Child
						end,
		%io:format("mutated: ~p~n", [NewChild]),
	mutate_children(Rest, Chance, Size, [NewChild|Acc]).

% mutates one byte in a 5 byte word
% kind of ugly
mutate(<<B1:8, B2:8, B3:8, B4:8>>, Size) ->
	Offset = random:uniform(Size),
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

