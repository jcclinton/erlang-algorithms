-module(kd_tree_tests).
-include_lib("eunit/include/eunit.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kd_tree_test_() ->
[
{setup, fun build_tree1/0, fun stop/1, fun nn/1},
{setup, fun build_tree2/0, fun stop/1, fun nn/1},
{setup, fun build_tree_re/0, fun stop/1, fun rebalance/1}
].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_tree1() ->
	Points = get_points(),
	Point = {1,2,3,4},
	Tree = kd_tree:create(Points),
	{Tree, Point, Point}.

build_tree2() ->
	Points = get_points(),
	Point = {0,0,0,0},
	Expected = {1,2,3,4},
	Tree = kd_tree:create(Points),
	{Tree, Point, Expected}.


build_tree_re() ->
	Points = get_points(),
	Tree = kd_tree:create(Points),
	{Tree}.





stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%
%% helper functions
%%%%%%%%%%%%%%%%%%
get_points() ->
	lists:map(fun(K) ->
		{K, K+1,K+2,K+3}
	end, lists:seq(1,20)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nn({Tree, Point, ExpectedResult}) ->
	Result = kd_tree:nearest_neighbor(Point, Tree),
	[?_assertEqual(ExpectedResult, Result)].

rebalance({Tree}) ->
	Result = kd_tree:rebalance(Tree),
	[?_assertEqual(Tree, Result)].
