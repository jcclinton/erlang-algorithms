-module(dijkstra).

-export([run/3, test/0]).
-export([create_graph/2, get_path/3]).
-export([create_queue/0, insert_queue/2]).

-compile([export_all]).


%% node functions
create_node(Id, Distance, ConnectedNodeIds) ->
	HasBeenVisited = false,
	{node, Id, Distance, HasBeenVisited, ConnectedNodeIds}.

mark_node_as_visited(Node) ->
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
	lists:foldl(fun(N, Graph) ->
		Conns = lists:foldl(fun(Fun, Nodes) ->
							Fun(N, Nodes, SideLength)
						end, [], Funs),
		Distance = if StartId == N -> 0; true -> infinity end,
		Node = create_node(N, Distance, Conns),
		insert_graph(Node, Graph)
	end, gb_trees:empty(), lists:seq(1, Size)).

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

flatten_graph(Graph) -> gb_trees:values(Graph).

insert_graph(Node, Graph) ->
	N = get_node_id(Node),
	gb_trees:enter(N, Node, Graph).

%% end graph functions

%% queue functions

insert_tree_queue(Node, Tree) ->
	Id = get_node_id(Node),
	gb_trees:enter(Id, Node, Tree).

pop_tree_queue(Tree) ->
	List = gb_trees:to_list(Tree),
	{Smallest, NewList} = get_queue_smallest(List),
	SortedOrddict = lists:sort(fun({Aid, _}, {Bid, _}) ->
		Aid =< Bid
	end, NewList),
	OutList = gb_trees:from_orddict(SortedOrddict),
	{Smallest, OutList}.


get_queue_smallest(List) ->
%io:format("queue list: ~p~n", [List]),
	get_queue_smallest(List, {none, []}).

get_queue_smallest([], Acc) -> Acc;
get_queue_smallest([{Id, Node}|Rest], {none, List}) ->
	Distance = get_node_distance(Node),
	if Distance == infinity ->
			get_queue_smallest(Rest, {none, [{Id, Node}|List]});
		true ->
			get_queue_smallest(Rest, {Node, List})
	end;
get_queue_smallest([{Id, Node}|Rest], {Smallest, List}) ->
	Distance = get_node_distance(Node),
	SmallDistance = get_node_distance(Smallest),
	if Distance < SmallDistance ->
			SmallId = get_node_id(Smallest),
			get_queue_smallest(Rest, {Node, [{SmallId, Smallest} | List]});
		true ->
			get_queue_smallest(Rest, {Smallest, [{Id, Node}|List]})
	end.
	
fill_tree_queue(Nodes) ->
	lists:foldl(fun(Node, TreeIn) ->
		Id = get_node_id(Node),
		gb_trees:insert(Id, Node, TreeIn)
	end, gb_trees:empty(), Nodes).

% searches queue for id
% returns node plus queue without that node
get_tree_queue_node(Id, Tree) -> gb_trees:lookup(Id, Tree).







create_queue() -> [].


insert_queue(Node, []) ->
	Distance = get_node_distance(Node),
	[{Distance, [Node]}];
insert_queue(Node, [{PrioDist, PrioList}=Prio|Rest] = Queue) ->
	Distance = get_node_distance(Node),
	if Distance < PrioDist ->
			[{Distance, [Node]} | Queue];
		Distance == PrioDist ->
			NewPrioList = [Node|PrioList],
			[{PrioDist, NewPrioList}|Rest];
		true ->
			[Prio | insert_queue(Node, Rest)]
	end.


% gets next node from queue
% wont return element with infinity queue priorty
pop_queue([]) -> none;
pop_queue([{infinity, _PrioList}=Head | Rest]) -> {none, [Head | Rest]};
pop_queue([{Distance, [Head|Tail]} | Rest]) ->
	if length(Tail) == 0 -> {Head, Rest};
		true ->
			{Head, [{Distance, Tail} | Rest]}
	end.


fill_queue(Nodes) ->
	lists:foldl(fun(Node, QueueIn) ->
		insert_queue(Node, QueueIn)
	end, create_queue(), Nodes).

% searches queue for id
% returns node plus queue without that node
get_queue_node(_Id, []) -> {none, []};
get_queue_node(Id, [{_Distance, []} | Rest]) -> get_queue_node(Id, Rest);
get_queue_node(Id, [{Distance, [Head|Tail]} | Rest]) ->
	HeadId = get_node_id(Head),
	if HeadId == Id ->
			QueueOut = [{Distance, Tail} | Rest],
			{Head, QueueOut};
		true ->
			{Found, NewTail} = get_queue_node(Id, [{Distance, Tail} | Rest]),
			{Found, [Head | NewTail]}
	end.




%% end queue functions


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
			OutList = gb_trees:values(OutGraph),
			Path = get_path(Start, End, OutList),
			io:format("final path starting at ~p and ending at ~p: ~p~n", [Start, End, Path])
	end,
	ok.


% runs the algorithm
% returns the modified graph as output
run(_StartId, EndId, Graph) ->
	Nodes = flatten_graph(Graph),
	Queue = fill_tree_queue(Nodes),
	{StartNode, NextQueue} = pop_tree_queue(Queue),
	check_current_node(Graph, NextQueue, StartNode, EndId).


%% private functions

% runs algorithm for given node
% -finds distance to all neighbors through this node, if less than neighbors current length, replace neighbors distance with new value
% -take unvisited node with lowest distance as the next node to check
% repeat until end conditions are met
% algorithm ends when endid is mark visited or there are no unvisited nodes with finite distance

check_current_node(Graph, Queue, CurrentNode, EndId) ->
	CurrId = get_node_id(CurrentNode),
	Distance = get_node_distance(CurrentNode),
	ConnectedNodeIds = get_node_connected(CurrentNode),
	% retrieves unvisited and connected nodes
	% also returns queue without these nodes
	%io:format("input queue list: ~p~n", [gb_trees:to_list(Queue)]),
	%io:format("input queue tree: ~p~n", [Queue]),
	Cons = lists:foldl(fun(Id, ConsList) ->
						%Result = get_tree_queue_node(Id, Queue),
						Result = gb_trees:lookup(Id, Queue),
	%io:format("result for id ~p: ~p~n", [Id, Result]),
						case Result of
							none -> ConsList;
							{value, Node} -> [Node|ConsList]
						end
	end, [], ConnectedNodeIds),
	UpdatedCons = calc_tentative_distance(Cons, Distance),
	UpdatedQueue = lists:foldl(fun(Node, QueueIn) ->
		insert_tree_queue(Node, QueueIn)
	end, Queue, UpdatedCons),
	%io:format("updated queue list: ~p~n", [gb_trees:values(UpdatedQueue)]),
	MarkedCurrentNode = mark_node_as_visited(CurrentNode),
	NewGraph = insert_graph(MarkedCurrentNode, Graph),
	% put marked node back into graph
	if EndId == CurrId ->
			% algorithm is finished
			NewGraph;
		true ->
			% find the next node
			{NextNode, NextQueue} = pop_tree_queue(UpdatedQueue),
			if NextNode == none ->
				% no unvisited nodes were found with a finite distance
				no_connection;
			true ->
				check_current_node(NewGraph, NextQueue, NextNode, EndId)
			end
	end.





% calculates tentative distance and updates any nodes that need it
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
							%% only update nodes distance if it has not been visited
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

get_path_node([Head|Rest], Id) ->
	HeadId = get_node_id(Head),
	if HeadId == Id -> Head;
		true -> get_path_node(Rest, Id)
	end.

% starts at the end node and finds the shortest path back to the start node,
% which just consists of picking the neighbor with the shortest distance
get_path(StartId, EndId, List) ->
	EndNode = get_path_node(List, EndId),
	get_path(EndNode, List, StartId, []).

get_path(CurrNode, List, StartId, Ids) ->
	CurrId = get_node_id(CurrNode),
	Conns = get_node_connected(CurrNode),
	if CurrId == StartId -> [StartId|Ids];
		true ->
			NextNode = get_shortest_conn_node(Conns, List),
			get_path(NextNode, List, StartId, [CurrId|Ids])
	end.

% returns the neighbor with the shortest distance back to the origin
% this will be the next node back in the path to the origin
get_shortest_conn_node(Conns, List) ->
	Empty = null,
	lists:foldl(fun(ConnId, ShortestNode) ->
		NextNode = get_path_node(List, ConnId),
		if ShortestNode == Empty -> NextNode;
			true ->
				Distance = get_node_distance(NextNode),
				ShortDistance = get_node_distance(ShortestNode),
				if Distance < ShortDistance -> NextNode;
					true -> ShortestNode
				end
		end
	end, Empty, Conns).

