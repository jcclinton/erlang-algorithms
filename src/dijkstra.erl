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
	if N rem 10 == 1 -> Nodes;
		true -> [N-1|Nodes]
	end.
right_check(N, Nodes) ->
	if N rem 10 == 0 -> Nodes;
		true -> [N+1|Nodes]
	end.
	

sort_graph(Graph) ->
	lists:sort(fun(A, B) ->
		{node, AId, _, _, _} = A,
		{node, BId, _, _, _} = B,
		if AId =< BId -> true;
			true -> false
		end
	end, Graph).
		

run() ->
	run(1, 100, create_graph()).
run(StartId, EndId, Graph) ->
	CurrentNode = get_node(Graph, StartId),
	OutGraph = check_current_node(Graph, CurrentNode, EndId),
	SortedOutGraph = sort_graph(OutGraph),
	%io:format("out graph: ~p~n", [SortedOutGraph]),
	get_path(StartId, EndId, OutGraph).

check_current_node(Graph, CurrentNode, EndId) ->
	{node, CurrId, Distance, false, ConnectedNodeIds} = CurrentNode,
	{Con, UnCon} = lists:foldl(fun(Node, {ConnectedNodes, UnConnectedNodes}) ->
								{node, Id, _, _, _} = Node,
								IsMember = lists:member(Id, ConnectedNodeIds),
								if IsMember ->
										{[Node|ConnectedNodes], UnConnectedNodes};
									true ->
										{ConnectedNodes, [Node|UnConnectedNodes]}
								end
	end, {[],[]}, Graph),
	NewCon = calc_tentative_distance(Con, Distance),
	NewCurrentNode = {node, CurrId, Distance, true, ConnectedNodeIds},
	NewGraph1 = NewCon ++ UnCon,
	NewGraph = replace_node(NewCurrentNode, NewGraph1),
	if EndId == CurrId ->
			NewGraph;
		true ->
			NextNode = get_next_node(NewGraph),
			if NextNode == [] ->
				no_connection;
			true ->
				check_current_node(NewGraph, NextNode, EndId)
			end
	end.

get_next_node(Nodes) ->
	get_next_node(Nodes, []).

get_next_node([], Next) -> Next;
get_next_node([Node|Rest], []) ->
	{node, _, Distance, HasVisited, _} = Node,
	Next = if HasVisited orelse Distance == infinity -> [];
					true ->
						Node
				end,
	get_next_node(Rest, Next);
get_next_node([Node|Rest], Next) ->
	{node, _, Distance, HasVisited, _} = Node,
	{node, _, NextDistance, _, _} = Next,
	NewNext = if not HasVisited andalso Distance < NextDistance ->
 Node;
							true -> Next
						end,
	get_next_node(Rest, NewNext).

	


remove_node(RemoveNode, Nodes) ->
	{node, RemoveId, _, _, _} = RemoveNode,
	lists:foldl(fun(Node, AccIn) ->
								{node, Id, _, _, _} = Node,
								if RemoveId == Id -> AccIn;
									true -> [Node|AccIn]
								end
							end, [], Nodes).

replace_node(Node, Graph) ->
	replace_node(Node, Graph, []).
replace_node(_Node, [], Acc) -> Acc;
replace_node(Node, [Head|Tail], Acc) ->
	{node, NodeId, _, true, _} = Node,
	{node, HeadId, _, _, _} = Head,
	if HeadId == NodeId ->
			Tail ++ [Node|Acc];
		true ->
			replace_node(Node, Tail, [Head|Acc])
	end.


replace_nodes(Nodes, Graph) ->
	lists:foldl(fun(GraphNode, GraphAcc) ->
													{node, Id, _, _, _} = GraphNode,
													NewNode = lists:foldl(fun(ReplaceNode, AccIn) ->
														{node, ReplaceId, _, _, _} = ReplaceNode,
														if Id == ReplaceId -> ReplaceNode;
															true -> AccIn
														end
													end, GraphNode, Nodes),
													[NewNode|GraphAcc]
												end, [], Graph).



get_node([], _) -> throw(badarg);
get_node([Node|Graph], LookupId) ->
	{node, Id, _, _, _} = Node,
	if LookupId == Id -> Node;
		true -> get_node(Graph, LookupId)
	end.

calc_tentative_distance(Nodes, CurrDistance) ->
	calc_tentative_distance(Nodes, CurrDistance, []).
calc_tentative_distance([], _CurrDistance, Acc) -> Acc;
calc_tentative_distance([Node|Rest], CurrDistance, Acc) ->
	{node, Id, Distance, Visited, Conns} = Node,
	EdgeLength = 1,
	NewDistance =
							if not Visited ->
								CurrDistPlusEdge = CurrDistance + EdgeLength,
								if Distance == infinity -> CurrDistPlusEdge;
										true ->
											if CurrDistPlusEdge > Distance -> Distance;
												true -> CurrDistPlusEdge
											end
								end;
								true -> Distance
							end,
	NewNode = {node, Id, NewDistance, Visited, Conns},
	calc_tentative_distance(Rest, CurrDistance, [NewNode|Acc]).


get_path(StartId, EndId, Graph) ->
	EndNode = get_node(Graph, EndId),
	get_path_list(EndNode, Graph, StartId).

get_path_list(CurrNode, Graph, StartId) ->
	get_path_list(CurrNode, Graph, StartId, []).

get_path_list(CurrNode, Graph, StartId, Ids) ->
	{node, CurrId, _, _, Conns} = CurrNode,
	if CurrId == StartId -> Ids;
		true ->
			NextNode = get_shortest_conn_node(Conns, Graph),
			get_path_list(NextNode, Graph, StartId, [CurrId|Ids])
	end.

get_shortest_conn_node(Conns, Graph) ->
	Empty = null,
	lists:foldl(fun(ConnId, ShortestNode) ->
		NextNode = get_node(Graph, ConnId),
		if ShortestNode == Empty -> NextNode;
			true ->
				{node, _, Distance, _, _} = NextNode,
				{node, _, ShortDistance, _, _} = ShortestNode,
				if Distance < ShortDistance -> NextNode;
					true -> ShortestNode
				end
		end
	end, Empty, Conns).

