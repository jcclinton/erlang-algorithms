-module(trie_tests).
-include_lib("eunit/include/eunit.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trie_test_() ->
[
{setup, fun build_trie1/0, fun stop/1, fun check/1},
{setup, fun build_trie2/0, fun stop/1, fun check/1},
{setup, fun build_trie3/0, fun stop/1, fun check/1}
].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_trie1() ->
	Words = [
		<<"hello">>,
		<<"hi">>,
		<<"hella">>,
		<<"hullooo">>,
		<<"hack">>
		],
	Trie = lists:foldl(fun(Word, TrieIn) ->
		trie:insert(Word, TrieIn)
	end, trie:new(), Words),
	Lookup = "",
	Expected = Words,
	{Expected, Lookup, Trie}.

build_trie2() ->
	Words = [
		<<"hello">>,
		<<"hi">>,
		<<"hella">>,
		<<"hullooo">>,
		<<"hack">>
		],
	Trie = lists:foldl(fun(Word, TrieIn) ->
		trie:insert(Word, TrieIn)
	end, trie:new(), Words),
	Lookup = "h",
	Expected = Words,
	{Expected, Lookup, Trie}.

build_trie3() ->
	Words = [
		"dog",
		"dogeatdog",
		"cat",
		"abcd",
		"helloni",
		"hellono",
		"hi",
		"hellas",
		"hellarr",
		"hellorr",
		"hullooo",
		"hack"
		],
	Trie = lists:foldl(fun(Word, TrieIn) ->
		trie:insert(Word, TrieIn)
	end, trie:new(), Words),
	Lookup = "hell",
	Expected = [
		<<"helloni">>,
		<<"hellono">>,
		<<"hellas">>,
		<<"hellarr">>,
		<<"hellorr">>
		],
	{Expected, Lookup, Trie}.





stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check({ExpectedResult, Lookup, Trie}) ->
	Result = trie:lookup(Lookup, Trie),
	[?_assertEqual(lists:sort(ExpectedResult), lists:sort(Result))].
