-module(trie).

-export([new/0, insert/2, lookup/2]).


%% space-efficient trie implemenation that does not store values
%% used to lookup any words based on some prefix (ie autocomplete, suggestions, etc)


% create new empty trie
new() ->
	[].


% insert word into tree
insert(Word, Trie) when is_list(Word), is_list(Trie) ->
	WordBin = list_to_binary(Word),
	insert(WordBin, Trie);
insert(<<>>, Trie) -> Trie;
insert(<<LNum:8, Rest/binary>>, Trie) ->
	Letter = <<LNum:8>>,
	{Node, RestTrie} = get_letter(Letter, Trie),
	NewSubTrie = case Node of
		undefined -> insert_new(Rest);
		{Letter, LetterSubTrie} -> insert(Rest, LetterSubTrie)
	end,
	[{Letter, NewSubTrie}] ++ RestTrie;
insert(_,_) -> {error, badarg}.


%creates a new subtrie
insert_new(<<>>) -> new();
insert_new(<<Letter:8, Rest/binary>>) ->
	[{<<Letter:8>>, insert_new(Rest)}].



%checks if this letter is in this level of the given trie
get_letter(Letter, Trie) ->
	get_letter(Letter, Trie, []).

get_letter(_Letter, [], Acc) -> {undefined, Acc};
get_letter(Letter, [{HeadLetter, HeadTail}|Tail], Acc) ->
	Node = {HeadLetter, HeadTail},
	if Letter == HeadLetter ->
			NewAcc = Acc ++ Tail,
			{Node, NewAcc};
		true -> get_letter(Letter, Tail, [Node|Acc])
	end.


% looks up any stored words that start with the input word
lookup(WordList, Trie) when is_list(WordList) ->
	Word = list_to_binary(WordList),
	lookup(Word, Trie);
lookup(Word, Trie) when is_binary(Word) ->
	lists:flatten(lookup(Word, Trie, <<>>));
lookup(_, _) -> {error, badarg}.

lookup(<<>>, Trie, Word) ->
	% builds up lookup letters in the Word accumulator
	% Trie is the subtree of the Word letters
	extract_words(Trie, Word);
lookup(<<LNum:8, Rest/binary>>, Trie, <<Word/binary>>) ->
	Letter = <<LNum:8>>,
	{Node, _} = get_letter(Letter, Trie),
	case Node of
		undefined -> [];
		{Letter, LetterSubTrie} ->
			NewWord = <<LNum:8, Word/binary>>,
			lookup(Rest, LetterSubTrie, NewWord)
	end.


%% extracts all words in this subtrie
%% prepends the Word prefix to them to form full words
extract_words([], Word) -> [reverse(Word)];
extract_words([{<<LNum:8>>, SubTrie}], <<Word/binary>>) ->
	NewWord = <<LNum:8, Word/binary>>,
	extract_words(SubTrie, NewWord);
extract_words([{<<LNum:8>>, SubTrie}|Rest], <<Word/binary>>) ->
	NewWord = <<LNum:8, Word/binary>>,
	[extract_words(SubTrie, NewWord) | extract_words(Rest, Word)].

% reverse util function
reverse(Bin) when is_binary(Bin) ->
	reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<L:8, Rest/binary>>, <<Acc/binary>>) ->
	reverse(<<Rest/binary>>, <<L:8, Acc/binary>>).
