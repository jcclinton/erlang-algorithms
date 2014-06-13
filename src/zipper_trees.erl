-module(zipper_trees).

-export([convert_tree/2]).


convert_tree(_, {0, nil}=Data) -> Data;
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
