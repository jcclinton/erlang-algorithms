-module(dijkstra).

-export([]).
-compile([export_all]).



%% node functions
create_node(Id, Distance, ConnectedNodeIds) ->
	HasBeenVisited = false,
	{node, Id, Distance, HasBeenVisited, ConnectedNodeIds}.

mark_visited_node(Node) ->
	{node, Id, Distance, false, ConnectedNodeIds} = Node,
	{node, Id, Distance, true, ConnectedNodeIds}.

update_node_distance(NewDistance, Node) ->
	{node, Id, _, HasVisited, ConnectedNodeIds} = Node,
	{node, Id, NewDistance, HasVisited, ConnectedNodeIds}.

get_node_id(Node) ->
	get_node_value(Node, id).

get_node_distance(Node) ->
	get_node_value(Node, distance).

get_node_visited(Node) ->
	get_node_value(Node, visited).

get_node_connected(Node) ->
	get_node_value(Node, connected).

get_node_value(Node, Value) ->
	{node, Id, Distance, HasBeenVisited, ConnectedNodeIds} = Node,
	case Value of
		id -> Id;
		distance -> Distance;
		visited -> HasBeenVisited;
		connected -> ConnectedNodeIds
	end.

%%% end node functions


%% graph functions

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
		AId = get_node_id(A),
		BId = get_node_id(B),
		if AId =< BId -> true;
			true -> false
		end
	end, Graph).
		

%% end graph functions


%% public api

test() ->
	run(1, 100, create_graph()).

run(StartId, EndId, Graph) ->
	CurrentNode = get_node(Graph, StartId),
	OutGraph = check_current_node(Graph, CurrentNode, EndId),
	%SortedOutGraph = sort_graph(OutGraph),
	%io:format("out graph: ~p~n", [SortedOutGraph]),
	get_path(StartId, EndId, OutGraph).

%% private functions

check_current_node(Graph, CurrentNode, EndId) ->
	CurrId = get_node_id(CurrentNode),
	Distance = get_node_distance(CurrentNode),
	ConnectedNodeIds = get_node_connected(CurrentNode),
	{Con, UnCon} = lists:foldl(fun(Node, {ConnectedNodes, UnConnectedNodes}) ->
								Id = get_node_id(Node),
								IsMember = lists:member(Id, ConnectedNodeIds),
								if IsMember ->
										{[Node|ConnectedNodes], UnConnectedNodes};
									true ->
										{ConnectedNodes, [Node|UnConnectedNodes]}
								end
	end, {[],[]}, Graph),
	NewCon = calc_tentative_distance(Con, Distance),
	NewCurrentNode = mark_visited_node(CurrentNode),
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
	Distance = get_node_distance(Node),
	HasVisited = get_node_visited(Node),
	Next = if HasVisited orelse Distance == infinity -> [];
					true ->
						Node
				end,
	get_next_node(Rest, Next);
get_next_node([Node|Rest], Next) ->
	Distance = get_node_distance(Node),
	HasVisited = get_node_visited(Node),
	NextDistance = get_node_distance(Next),
	NewNext = if not HasVisited andalso Distance < NextDistance ->
 Node;
							true -> Next
						end,
	get_next_node(Rest, NewNext).

	


remove_node(RemoveNode, Nodes) ->
	RemoveId = get_node_id(RemoveNode),
	lists:foldl(fun(Node, AccIn) ->
								Id = get_node_id(Node),
								if RemoveId == Id -> AccIn;
									true -> [Node|AccIn]
								end
							end, [], Nodes).

replace_node(Node, Graph) ->
	replace_node(Node, Graph, []).
replace_node(_Node, [], Acc) -> Acc;
replace_node(Node, [Head|Tail], Acc) ->
	NodeId = get_node_id(Node),
	HeadId = get_node_id(Head),
	if HeadId == NodeId ->
			Tail ++ [Node|Acc];
		true ->
			replace_node(Node, Tail, [Head|Acc])
	end.


replace_nodes(Nodes, Graph) ->
	lists:foldl(fun(GraphNode, GraphAcc) ->
													Id = get_node_id(GraphNode),
													NewNode = lists:foldl(fun(ReplaceNode, AccIn) ->
														ReplaceId = get_node_id(ReplaceNode),
														if Id == ReplaceId -> ReplaceNode;
															true -> AccIn
														end
													end, GraphNode, Nodes),
													[NewNode|GraphAcc]
												end, [], Graph).



get_node([], _) -> throw(badarg);
get_node([Node|Graph], LookupId) ->
	Id = get_node_id(Node),
	if LookupId == Id -> Node;
		true -> get_node(Graph, LookupId)
	end.

calc_tentative_distance(Nodes, CurrDistance) ->
	calc_tentative_distance(Nodes, CurrDistance, []).
calc_tentative_distance([], _CurrDistance, Acc) -> Acc;
calc_tentative_distance([Node|Rest], CurrDistance, Acc) ->
	Distance = get_node_distance(Node),
	Visited = get_node_visited(Node),
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
	NewNode = update_node_distance(NewDistance, Node),
	calc_tentative_distance(Rest, CurrDistance, [NewNode|Acc]).


%% path functions

get_path(StartId, EndId, Graph) ->
	EndNode = get_node(Graph, EndId),
	get_path_list(EndNode, Graph, StartId).

get_path_list(CurrNode, Graph, StartId) ->
	get_path_list(CurrNode, Graph, StartId, []).

get_path_list(CurrNode, Graph, StartId, Ids) ->
	CurrId = get_node_id(CurrNode),
	Conns = get_node_connected(CurrNode),
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
				Distance = get_node_distance(NextNode),
				ShortDistance = get_node_distance(ShortestNode),
				if Distance < ShortDistance -> NextNode;
					true -> ShortestNode
				end
		end
	end, Empty, Conns).

