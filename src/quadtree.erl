-module(quadtree).

-export([new/2, insert/2, query_range/2]).


new(Center, HalfDimension) -> {quad, Center, HalfDimension, [], null, null, null, null}.

insert(Point, Quad) ->
	{quad, Center, HalfDimension, _Points, _NW, _NE, _SW, _SE} = Quad,
	IsBound = is_bound(Point, {Center, HalfDimension}),
	if IsBound ->
			append(Point, Quad);
		IsBound == false ->
			{false, Quad}
	end.

is_bound({X, Y}, {{CenterX, CenterY}, HalfDimension}) ->
	EastEdge = CenterX + HalfDimension,
	WestEdge = CenterX - HalfDimension,
	NorthEdge = CenterY + HalfDimension,
	SouthEdge = CenterY - HalfDimension,
	if X > EastEdge orelse X < WestEdge orelse Y > NorthEdge orelse Y < SouthEdge -> false;
		true -> true
	end.


append(Point, Quad) ->
	{quad, Center, HalfDimension, Points, NW, NE, SW, SE} = Quad,
	if length(Points) < 4 ->
			NewPoints = [Point|Points],
			NewQuad = {quad, Center, HalfDimension, NewPoints, NW, NE, SW, SE},
			{true, NewQuad};
		true ->
			subdivide_insert(Point, Quad)
		end.
	
subdivide_insert(Point, Quad) ->
	{quad, _, _, _, OldNW, _, _, _} = Quad,
	DividedQuad = if OldNW == null -> subdivide(Quad);
							true -> Quad
						end,
	{quad, Center, HalfDimension, Points, NW, NE, SW, SE} = DividedQuad,
	% not efficient to call all 4, but should work
	{_, NewNW} = insert(Point, NW),
	{_, NewNE} = insert(Point, NE),
	{_, NewSW} = insert(Point, SW),
	{_, NewSE} = insert(Point, SE),
	NewQuad = {quad, Center, HalfDimension, Points, NewNW, NewNE, NewSW, NewSE},
	{true, NewQuad}.



subdivide(Quad) ->
	{quad, {CenterX, CenterY}, HalfDimension, Points, null, null, null, null} = Quad,
	NewHalf = HalfDimension div 2,
	NWCenter = {CenterX - NewHalf, CenterY + NewHalf},
	NECenter = {CenterX + NewHalf, CenterY + NewHalf},
	SWCenter = {CenterX - NewHalf, CenterY - NewHalf},
	SECenter = {CenterX + NewHalf, CenterY - NewHalf},
	NW = new(NWCenter, NewHalf),
	NE = new(NECenter, NewHalf),
	SW = new(SWCenter, NewHalf),
	SE = new(SECenter, NewHalf),
	{quad, {CenterX, CenterY}, HalfDimension, Points, NW, NE, SW, SE}.


query_range(Range, Quad) ->
	{quad, Center, HalfDimension, _, _, _, _, _} = Quad,
	Intersects = intersects({Center, HalfDimension}, Range),
	if Intersects ->
			query_range(Range, Quad, []);
		true -> []
	end.

query_range(Range, Quad, Acc) ->
	{quad, _, _, Points, NW, NE, SW, SE} = Quad,
	PointsAcc = lists:foldl(fun(Point, AccIn) ->
								InRange = is_bound(Point, Range),
								if InRange -> [Point|AccIn];
									true -> AccIn
								end
							end, Acc, Points),
	lists:foldl(fun(QuadIn, AccIn) ->
								if QuadIn == null -> AccIn;
									true -> AccIn ++ query_range(Range, QuadIn)
								end
							end, PointsAcc, [NW, NE, SW, SE]).

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
