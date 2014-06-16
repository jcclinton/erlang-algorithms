-module(zipper_nary_trees_tests).
-include_lib("eunit/include/eunit.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zipper_nary_trees_test_() ->
[
{setup, fun build_tree1/0, fun stop/1, fun check/1}
].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_tree1() ->
	Input = [
			{down, l},
			{right, m},
			{right, n},
			{up, nil},
		{right, b},
			{down, o},
			{up, nil},
		{right, c},
			{down, p},
			{right, q},
			{right, r},
			{up, nil},
		{right, d},
			{down, s},
			{right, t},
				{down, 1},
				{right, 2},
				{right, 3},
				{right, 4},
				{right, 5},
				{right, 6},
				{right, 7},
				{right, 8},
				{right, 9},
				{up, nil},
			{right, u},
			{right, v},
			{right, x}
	],


	M = zipper_nary_trees,
	ZTreeStart = zipper_nary_trees:create(),
	ZTreeStart2 = zipper_nary_trees:insert(a, ZTreeStart),

	ZTreeEnd = lists:foldl(fun({Fun, Char}, ZTreeIn) ->
		ZTreeOut = M:Fun(ZTreeIn),
		if Char == nil -> ZTreeOut;
			true -> M:insert(Char, ZTreeOut)
		end
	end, ZTreeStart2, Input),

	ExpectedValue = x,
	{ExpectedValue, ZTreeEnd}.
		





stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check({ExpectedResult, ZTreeEnd}) ->
	Value = zipper_nary_trees:get_value(ZTreeEnd),
	[?_assertEqual(ExpectedResult, Value)].
