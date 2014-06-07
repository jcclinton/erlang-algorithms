-module(dijkstra).

-export([]).
-compile([export_all]).



create_node(Id, Distance, ConnectedNodeIds) ->
	HasBeenVisited = false,
	{node, Id, Distance, HasBeenVisited, ConnectedNodeIds}.

create_graph() ->
	Starting = 1,
	Funs = [fun bottom_check/2, fun top_check/2, fun left_check/2, fun right_check/2],
	lists:foldr(fun(N, Graph) ->
		Conns = lists:foldr(fun(Fun, Nodes) ->
							Fun(N, Nodes)
						end, [], Funs),
		Distance = if Starting == N -> 0; true -> infinity end,
		Node = create_node(N, Distance, Conns),
		[Node|Graph]
	end, [], lists:seq(1, 100)).

bottom_check(N, Nodes) ->
	if N < 11 -> Nodes;
		true -> [N-10|Nodes]
	end.
top_check(N, Nodes) ->
	if N > 90 -> Nodes;
		true -> [N+10|Nodes]
	end.
left_check(N, Nodes) ->
	if N div 10 == 1 -> Nodes;
		true -> [N-1|Nodes]
	end.
right_check(N, Nodes) ->
	if N div 10 == 0 -> Nodes;
		true -> [N+1|Nodes]
	end.
	

