-module(zipper_trees).

-export([convert_tree/2, up/1, left/1, right/1]).


convert_tree(_, {0, nil}) -> nil;
convert_tree(Key, Data) ->
	{_, Tree} = Data,
	NewTree = traverse(Key, Tree, nil),
	NewTree.


traverse(FocusKey, {Key, Value, L, R}, Context) ->
	if FocusKey == Key -> {Key, Value, L, R, Context};
		true ->
			{NewContext, Tree} = if FocusKey > Key -> {{Key, Value, L, Context}, R};
														FocusKey < Key -> {{Key, Value, Context, R}, L}
													end,
			traverse(FocusKey, Tree, NewContext)
	end.


up(nil) -> nil;
up({Key, Value, L, R, Context}) ->
	Node = {Key, Value, L, R},
	traverse_up(Node, Context).

traverse_up({Key, _, _, _}=Node, {UpperKey, UpperValue, UpperL, UpperR}) ->
	{NewL, NewR, NewContext} = if Key > UpperKey -> {UpperL, Node, UpperR};
										true -> {Node, UpperR, UpperL}
									end,
	{UpperKey, UpperValue, NewL, NewR, NewContext}.


left({_, _, nil, _, _}=Zipper) -> Zipper;
left({Key, Value, L, R, Context}) ->
	NewContext = {Key, Value, Context, R},
	{LKey, LValue, LL, LR} = L,
	{LKey, LValue, LL, LR, NewContext}.


right({_, _, _, nil, _}=Zipper) -> Zipper;
right({Key, Value, L, R, Context}) ->
	NewContext = {Key, Value, L, Context},
	{RKey, RValue, RL, RR} = R,
	{RKey, RValue, RL, RR, NewContext}.
