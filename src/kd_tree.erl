-module(kd_tree).

-export([create/0, create/1, insert/2, remove/2, rebalance/1, nearest_neighbor/2]).



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


% remove point from tree.
% once it finds the point to remove,
% it then gets all points below that point,
% then recreates the subtree with those points.
% doesnt cause tree to become imbalanced due to the way the subtree is recreated
% (point, tree) -> tree
remove(Point, Tree) ->
	remove(Point, Tree, 0).

% (point, tree, int) -> tree
remove(_Point, null, _Depth) -> null;
remove(Point, Tree, Depth) ->
	K = size(Point),
	Axis = (Depth rem K) + 1,
	{Median, Left, Right} = Tree,
	if Point == Median ->
			Points = lists:merge(flatten(Left), flatten(Right)),
			create(Points);
		Point /= Median ->
			MedianK = element(Axis, Median),
			PointK = element(Axis, Point),
			{NewLeft, NewRight} = if PointK > MedianK -> {Left, remove(Point, Right, Depth+1)};
															true -> {remove(Point, Left, Depth+1), Right}
														end,
			{Median, NewLeft, NewRight}
	end.

% flattens all points in a tree into a point list
% this is probably a slow function
% (tree) -> [tuple]
flatten(null) -> [];
flatten(Tree) ->
	{Median, Left, Right} = Tree,
	[Median] ++ flatten(Left) ++ flatten(Right).


% rebalances tree
% probably not the most efficient way to do it,
% but it is simple and effective
% (tree) -> tree
rebalance(Tree) ->
	Points = flatten(Tree),
	create(Points).


% finds nearest neighbor of a point
% (tuple, tree) -> tuple
nearest_neighbor(_Point, null) -> null;
nearest_neighbor(Point, Tree) ->
	nn(Point, Tree, 0).

% exhaustive nn search
% (tuple, tree, int) -> tuple
nn(Point, Tree, Depth) ->
	{Median, Left, Right} = Tree,
	if Left == null andalso Right == null -> Median;
		Left == null ->
			BestRight = nn(Point, Right, Depth+1),
			Rd = distance(Point, BestRight),
			Md = distance(Point, Median),
			if Rd < Md -> BestRight;
				true -> Median
			end;
		Right == null ->
			BestLeft = nn(Point, Left, Depth+1),
			Ld = distance(Point, BestLeft),
			Md = distance(Point, Median),
			if Ld < Md -> BestLeft;
				true -> Median
			end;
		true ->
			BestLeft = nn(Point, Left, Depth+1),
			BestRight = nn(Point, Right, Depth+1),
			Ld = distance(Point, BestLeft),
			Rd = distance(Point, BestRight),
			Md = distance(Point, Median),
			if Ld < Md andalso Ld < Rd -> BestLeft;
				Rd < Md andalso Rd < Ld -> BestRight;
				true -> Median
			end
		end.


% calculates the distance between n-dimensional points
% (tuple, tuple} -> float
distance(P1, P2) ->
	Size = size(P1),
	DiffSquared = lists:foldl(fun(K, AccIn) ->
										El1 = element(K, P1),
										El2 = element(K, P2),
										AccIn + math:pow(El1 - El2, 2)
									end, 0, lists:seq(1, Size)),
	math:sqrt(DiffSquared).

