-module(dijkstra).

-export([run/3, test/0]).



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

%% creates a square grid graph
create_graph(StartId, SideLength) ->
	Size = SideLength * SideLength,
	Funs = [fun bottom_check/3, fun top_check/3, fun left_check/3, fun right_check/3],
	lists:foldr(fun(N, Graph) ->
		Conns = lists:foldr(fun(Fun, Nodes) ->
							Fun(N, Nodes, SideLength)
						end, [], Funs),
		Distance = if StartId == N -> 0; true -> infinity end,
		Node = create_node(N, Distance, Conns),
		[Node|Graph]
	end, [], lists:seq(1, Size)).

bottom_check(N, Nodes, SideLength) ->
	if N < SideLength + 1 -> Nodes;
		true -> [N-SideLength|Nodes]
	end.
top_check(N, Nodes, SideLength) ->
	TopRowStart = SideLength * SideLength - SideLength,
	if N > TopRowStart -> Nodes;
		true -> [N+SideLength|Nodes]
	end.
left_check(N, Nodes, SideLength) ->
	if N rem SideLength == 1 -> Nodes;
		true -> [N-1|Nodes]
	end.
right_check(N, Nodes, SideLength) ->
	if N rem SideLength == 0 -> Nodes;
		true -> [N+1|Nodes]
	end.

%% end graph functions


%% public api

test() ->
	Start = 1,
	SideLength = 20,
	io:format("creating graph with ~p nodes~n", [SideLength*SideLength]),
	End = SideLength * SideLength,
	Graph = create_graph(Start, SideLength),
	OutGraph = run(Start, End, Graph),
	case OutGraph of
		no_connection ->
			io:format("no connection from start to finish was found in this graph~n");
		_ ->
			Path = get_path(Start, End, OutGraph),
			io:format("final path starting at ~p and ending at ~p: ~p~n", [Start, End, Path])
	end,
	ok.


% runs the algorithm
% returns the modified graph as output
run(StartId, EndId, Graph) ->
	CurrentNode = get_node(Graph, StartId),
	check_current_node(Graph, CurrentNode, EndId).


%% private functions

% runs algorithm for given node
% -finds distance to all neighbors through this node, if less than neighbors current length, replace neighbors distance with new value
% -take unvisited node with lowest distance as the next node to check
% repeat until end conditions are met
% algorithm ends when endid is mark visited or there are no unvisited nodes with finite distance

% should never get to the end of the graph, either of the end conditions will be met first
check_current_node([], _CurrentNode, _EndId) -> no_connection;
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

% returns an unvisited node with the lowest distance
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



% replaces a node in the graph
% used to mark a node as visited
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




% looks up node by id
get_node([], _) -> throw(badarg);
get_node([Node|Graph], LookupId) ->
	Id = get_node_id(Node),
	if LookupId == Id -> Node;
		true -> get_node(Graph, LookupId)
	end.

% calculates tentative distance
% tentative distance is the distance from node A to origin via Node B
% if node A's original distance is smaller than the tentative distance,
% node A will keep its original value
% once a node has been visited, its distance will no longer change
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

% returns a list of ids that constitutes the shortest path from start to end in this graph
% starts at the end node and finds the shortest path back to the start node,
% which just consists of picking the neighbor with the shortest distance
get_path(StartId, EndId, Graph) ->
	EndNode = get_node(Graph, EndId),
	get_path(EndNode, Graph, StartId, []).

get_path(CurrNode, Graph, StartId, Ids) ->
	CurrId = get_node_id(CurrNode),
	Conns = get_node_connected(CurrNode),
	if CurrId == StartId -> [StartId|Ids];
		true ->
			NextNode = get_shortest_conn_node(Conns, Graph),
			get_path(NextNode, Graph, StartId, [CurrId|Ids])
	end.

% returns the neighbor with the shortest distance back to the origin
% this will be the next node back in the path to the origin
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

