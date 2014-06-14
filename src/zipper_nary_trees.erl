-module(zipper_tree_lists).

-export([create/0, insert/2, down/1, up/1]).


% n-ary tree with zipper


create() -> [].

create_node(Value) ->
	{Value, create()}.

insert(Value, {[], Context}) -> {[create_node(Value)], Context};
insert(Value, {List, Context}) ->
	NewTree = create_node(Value),
	{[NewTree|List], Context}.

down({[{Value, Child}|Tail], Context}) ->
	NewContext = [{Value, Tail} | Context],
	{Child, NewContext}.

up({List, []}) -> {List, []};
up({Child, Context}) ->
	[{Value, Tail}|CtxTail] = Context,
	NewList = [{Value, Child}|Tail],
	{NewList, CtxTail}.
