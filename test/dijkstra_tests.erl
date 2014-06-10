-module(dijkstra_tests).
-include_lib("eunit/include/eunit.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dijkstra_test_() ->
[
{setup, fun build_graph/0, fun stop/1, fun run/1}
].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_graph() ->
	Start = 1,
	SideLength = 20,
	End = SideLength * SideLength,
	Graph = dijkstra:create_graph(Start, SideLength),
	PathLength = SideLength * 2 - 1,
	{PathLength, Start, End, Graph}.





stop(_SetupData) ->
	ok.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run({PathLength, Start, End, Graph}) ->
	OutGraph = dijkstra:run(Start, End, Graph),
	Path = dijkstra:get_path(Start, End, OutGraph),
	Result = length(Path),
	[?_assertEqual(PathLength, Result)].
