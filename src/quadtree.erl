-module(quadtree).

-export([new/1, insert/2, query_range/2]).

% number of points allowed per quadtree
-define(N, 4).


% creates new, empty quadtree
% (range) -> quadtree
new({{CenterX, CenterY}, HalfDimension}) when is_number(CenterX), is_number(CenterY), is_number(HalfDimension) ->
	Range = {{CenterX, CenterY}, HalfDimension},
	Points = [],
	Regions = {null, null, null, null},
	{quad, Range, Points, Regions};
new(_) -> {error, badarg}.

% inserts point into quadtree
% (point, quadtree) -> {bool, quadtree}
insert(Point, Quad) when is_tuple(Point), is_tuple(Quad) ->
	{quad, Range, _, _} = Quad,
	IsBound = is_bound(Point, Range),
	if IsBound ->
			append(Point, Quad);
		IsBound == false ->
			{false, Quad}
	end;
insert(_, _) -> {error, badarg}.


% checks if a point is bound inside a box
% (point, range) -> bool
is_bound({X, Y}, {{CenterX, CenterY}, HalfDimension}) ->
	EastEdge = CenterX + HalfDimension,
	WestEdge = CenterX - HalfDimension,
	NorthEdge = CenterY + HalfDimension,
	SouthEdge = CenterY - HalfDimension,
	if X > EastEdge orelse X < WestEdge orelse Y > NorthEdge orelse Y < SouthEdge -> false;
		true -> true
	end.


% appends a point to a quadtree
% subdivides if necessary
% (point, quadtree) -> {bool, quadtree}
append(Point, Quad) ->
	{quad, Range, Points, Regions} = Quad,
	if length(Points) < ?N ->
			NewPoints = [Point|Points],
			NewQuad = {quad, Range, NewPoints, Regions},
			{true, NewQuad};
		true ->
			subdivide_insert(Point, Quad)
		end.
	
% handles subdivision
% then inserts the point
% (point, quadtree) -> {bool, quadtree}
subdivide_insert(Point, Quad) ->
	{quad, _, _, {OldNW, _, _, _}} = Quad,
	DividedQuad = if OldNW == null -> subdivide(Quad);
							true -> Quad
						end,
	{quad, Range, Points, {NW, NE, SW, SE}} = DividedQuad,
	% not efficient to call all 4, but should work
	% will only get inserted into the quadtree with the proper bounding box
	{_, NewNW} = insert(Point, NW),
	{_, NewNE} = insert(Point, NE),
	{_, NewSW} = insert(Point, SW),
	{_, NewSE} = insert(Point, SE),
	NewQuad = {quad, Range, Points, {NewNW, NewNE, NewSW, NewSE}},
	{true, NewQuad}.



% subdivides the quadtree
% returns the quadtree with new regions
% (quadtree) -> quadtree
subdivide(Quad) ->
	{quad, Range, Points, {null, null, null, null}} = Quad,
	{{CenterX, CenterY}, HalfDimension} = Range,
	NewHalf = HalfDimension / 2,
	NWCenter = {CenterX - NewHalf, CenterY + NewHalf},
	NECenter = {CenterX + NewHalf, CenterY + NewHalf},
	SWCenter = {CenterX - NewHalf, CenterY - NewHalf},
	SECenter = {CenterX + NewHalf, CenterY - NewHalf},
	NW = new({NWCenter, NewHalf}),
	NE = new({NECenter, NewHalf}),
	SW = new({SWCenter, NewHalf}),
	SE = new({SECenter, NewHalf}),
	{quad, Range, Points, {NW, NE, SW, SE}}.


% returns list of all points in this quadtree that are in range
% (range, quadtree) -> [point]
query_range(InputRange, Quad) when is_tuple(InputRange), is_tuple(Quad) ->
	{quad, Range, _, _} = Quad,
	Intersects = intersects(Range, InputRange),
	if Intersects ->
			query_range(InputRange, Quad, []);
		true -> []
	end;
query_range(_, _) -> {error, badarg}.

% recursive query_range
% (range, quadtree, [point]) -> [point]
query_range(InputRange, Quad, Acc) ->
	{quad, _, Points, {NW, NE, SW, SE}} = Quad,
	PointsAcc = lists:foldl(fun(Point, AccIn) ->
								IsInRange = is_bound(Point, InputRange),
								if IsInRange -> [Point|AccIn];
									true -> AccIn
								end
							end, Acc, Points),
	lists:foldl(fun(QuadIn, AccIn) ->
								if QuadIn == null -> AccIn;
									true -> AccIn ++ query_range(InputRange, QuadIn)
								end
							end, PointsAcc, [NW, NE, SW, SE]).

% checks if the range box intersects with the quadtrees box
% (range, range) -> bool
intersects({{QuadCenterX, QuadCenterY}, QuadHalf}, {{RangeCenterX, RangeCenterY}, RangeHalf}) ->
	QuadW = QuadCenterX - QuadHalf,
	QuadE = QuadCenterX + QuadHalf,
	QuadS = QuadCenterY - QuadHalf,
	QuadN = QuadCenterY + QuadHalf,
	RangeW = RangeCenterX - RangeHalf,
	RangeE = RangeCenterX + RangeHalf,
	RangeS = RangeCenterY - RangeHalf,
	RangeN = RangeCenterY + RangeHalf,
	if QuadW =< RangeE andalso (QuadN >= RangeS orelse QuadS =< RangeN) andalso QuadE >= RangeW ->true;
		QuadN =< RangeS andalso (QuadE >= RangeW orelse QuadW =< RangeE) andalso QuadS >= RangeN -> true;
		true -> false
	end.
