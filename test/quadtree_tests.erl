-module(quadtree_tests).
-include_lib("eunit/include/eunit.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quadtree_test_() ->
[
{setup, fun build_quad1/0, fun stop/1, fun check/1},
{setup, fun build_quad2/0, fun stop/1, fun check/1},
{setup, fun build_quad3/0, fun stop/1, fun check/1},
{setup, fun build_quad4/0, fun stop/1, fun check/1}
].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_quad1() ->
	Range = {{50, 30}, 100},
	Quad = quadtree:new(Range),
	Points = lists:map(fun(Num) ->
											{Num, Num}
						end, lists:seq(30, 60, 3)),
	QuadOut = lists:foldl(fun(Point, QuadIn) ->
							{_, Q} = quadtree:insert(Point, QuadIn),
							Q
						end, Quad, Points),

	QueryRange = {{40, 40}, 5},
	Expected = [{36,36}, {39,39}, {42, 42}, {45, 45}],
	{Expected, QueryRange, QuadOut}.

build_quad2() ->
	Range = {{50, 50}, 20},
	Quad = quadtree:new(Range),
	Points = lists:map(fun(Num) ->
											{Num, Num}
						end, lists:seq(30, 60, 3)),
	QuadOut = lists:foldl(fun(Point, QuadIn) ->
							{_, Q} = quadtree:insert(Point, QuadIn),
							Q
						end, Quad, Points),

	QueryRange = {{30, 30}, 5},
	Expected = [{30,30}, {33,33}],
	{Expected, QueryRange, QuadOut}.

build_quad3() ->
	Range = {{50, 50}, 10},
	Quad = quadtree:new(Range),
	Points = lists:map(fun(Num) ->
											{Num, Num}
						end, lists:seq(30, 60, 3)),
	QuadOut = lists:foldl(fun(Point, QuadIn) ->
							{_, Q} = quadtree:insert(Point, QuadIn),
							Q
						end, Quad, Points),

	QueryRange = {{30, 30}, 5},
	Expected = [],
	{Expected, QueryRange, QuadOut}.

build_quad4() ->
	Range = {{50, 50}, 10},
	Quad = quadtree:new(Range),
	Points = lists:map(fun(Num) ->
											{Num, Num}
						end, lists:seq(1, 100)),
	QuadOut = lists:foldl(fun(Point, QuadIn) ->
							{_, Q} = quadtree:insert(Point, QuadIn),
							Q
						end, Quad, Points),

	QueryRange = {{40, 40}, 1},
	Expected = [{40,40}, {41,41}],
	{Expected, QueryRange, QuadOut}.





stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check({ExpectedResult, Range, Quad}) ->
	Result = quadtree:query_range(Range, Quad),
	[?_assertEqual(ExpectedResult, Result)].
