-module(trie).

-export([empty/0, insert/2, lookup/2]).
-compile([export_all]).



empty() ->
	[].


insert(Word, Trie) when is_list(Word), is_list(Trie) ->
	WordBin = list_to_binary(Word),
	insert(WordBin, Trie);
insert(<<>>, Trie) -> Trie;
insert(<<LNum:8, Rest/binary>>, Trie) ->
	L = <<LNum:8>>,
	{Node, RestTrie} = get_letter(L, Trie),
	NewNode = case Node of
		undefined -> [{L, insert_new(Rest)}];
		{L, LetterSubTrie} -> [{L, insert(Rest, LetterSubTrie)}]
	end,
	RestTrie ++ NewNode;
insert(_,_) -> {error, badarg}.


%inserts a new subtrie
insert_new(<<>>) -> empty();
insert_new(<<L:8, Rest/binary>>) ->
	[{<<L:8>>, insert_new(Rest)}].



%checks if this letter is in this level of the given trie
get_letter(L, Trie) ->
	get_letter(L, Trie, []).

get_letter(_L, [], Acc) -> {undefined, Acc};
get_letter(L, [{HeadLetter, HeadTail}|Tail], Acc) ->
	Node = {HeadLetter, HeadTail},
	if L == HeadLetter ->
			NewAcc = Acc ++ Tail,
			{Node, NewAcc};
		true -> get_letter(L, Tail, [Node|Acc])
	end.


% looks up any stored words that start with the input word
lookup(WordList, Trie) when is_list(WordList) ->
	Word = list_to_binary(WordList),
	lookup(Word, Trie);
lookup(<<>>, Trie) -> extract_words(Trie);
lookup(<<LNum:8, Rest/binary>>, Trie) ->
	L = <<LNum:8>>,
	{Node, _} = get_letter(L, Trie),
	case Node of
		undefined -> [];
		{L, LetterSubTrie} -> [L] ++ [lookup(Rest, LetterSubTrie)]
	end.

extract_words([]) -> [];
extract_words({L, SubTrie}) ->[L] ++ [extract_words(SubTrie)];
extract_words([Head|Rest]) -> extract_words(Head) ++ extract_words(Rest).
