-module(kd_tree_tests).
-include_lib("eunit/include/eunit.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kd_tree_test_() ->
[
{setup, fun build_tree1/0, fun stop/1, fun check/1}
].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_tree1() ->
	Points = [{1,2}],
	Tree = kd_tree:create(Points),
	Expected = Points,
	{Tree, Expected}.






stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check({_Tree, ExpectedResult}) ->
	%Result = kd_tree:query_range(Range, Quad),
	Result = true,
	[?_assertEqual(ExpectedResult, Result)].
