-module(kd_tree).

-export([create/0, create/1, insert/2]).



%creates empty tree
create() -> create([]).

% creates tree with some default values
% ([tuple]) -> tree
create(PointsList) ->
	create(PointsList, 0).


% ([tuple], int) -> tree
create([], _Depth) -> null;
create(PointsList, Depth) ->
	K = size(hd(PointsList)),
	Axis = (Depth rem K) + 1,
	PL2 = lists:sort(fun(A, B) ->
		Ak = element(Axis, A),
		Bk = element(Axis, B),
		Ak =< Bk
	end, PointsList),
	N = length(PL2) div 2,
	Median = lists:nth(N+1, PL2),
	RightPoints = lists:nthtail(N+1, PL2),
	{LeftPoints, _} = lists:split(N, PL2),
	Left = create(LeftPoints, Depth+1),
	Right = create(RightPoints, Depth+1),
	{Median, Left, Right}.


% inserts point into tree
% may cause tree to become imbalanced
% (point, tree) -> tree
insert(Point, Tree) ->
	insert(Point, Tree, 0).

% (point, tree, int) -> tree
insert(Point, null, Depth) ->
	create([Point], Depth);
insert(Point, Tree, Depth) ->
	K = size(Point),
	Axis = (Depth rem K) + 1,
	{Median, Left, Right} = Tree,
	MedianK = element(Axis, Median),
	PointK = element(Axis, Point),
	{NewLeft, NewRight} = if PointK > MedianK -> {Left, insert(Point, Right, Depth+1)};
													true -> {insert(Point, Left, Depth+1), Right}
												end,
	{Median, NewLeft, NewRight}.
