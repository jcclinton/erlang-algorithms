-module(kd_tree).

-export([create/0, create/1]).



create() -> create([]).

create(PointsList) ->
	create(PointsList, 0).


create([], _Depth) -> null;
create(PointsList, Depth) ->
	K = size(hd(PointsList)),
	Axis = Depth rem K,
	PL2 = lists:sort(fun(A, B) ->
		Ak = element(Axis+1, A),
		Bk = element(Axis+1, B),
		Ak =< Bk
	end, PointsList),
	N = length(PL2) div 2,
	Median = lists:nth(N+1, PL2),
	RightPoints = lists:nthtail(N+1, PL2),
	{LeftPoints, _} = lists:split(N, PL2),
	Left = create(LeftPoints, Depth+1),
	Right = create(RightPoints, Depth+1),
	{Median, Left, Right}.
