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
	

test() ->
	Graph = create_graph(),
	run(1, 99, Graph).

run() ->
	run(1, 100, create_graph()).
run(StartId, EndId, Graph) ->
	CurrentNode = get_start_node(Graph, StartId),
	check_current_node(Graph, CurrentNode, EndId).

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
			io:format("out graph: ~p~n", [NewGraph]),
			EndId;
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



get_start_node([], _StartId) -> throw(badarg);
get_start_node([Node|Graph], StartId) ->
	{node, Id, _, _, _} = Node,
	if StartId == Id -> Node;
		true -> get_start_node(Graph, StartId)
	end.

calc_tentative_distance(Nodes, CurrDistance) ->
	calc_tentative_distance(Nodes, CurrDistance, []).
calc_tentative_distance([], _CurrDistance, Acc) -> Acc;
calc_tentative_distance([Node|Rest], CurrDistance, Acc) ->
	{node, Id, Distance, Visited, Conns} = Node,
	NewDistance = if Distance == infinity -> 1;
										true ->
											if CurrDistance > Distance + 1 -> Distance + 1;
												true -> CurrDistance
											end
								end,
	NewNode = {node, Id, NewDistance, Visited, Conns},
	calc_tentative_distance(Rest, CurrDistance, [NewNode|Acc]).
