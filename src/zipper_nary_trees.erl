-module(zipper_nary_trees).

-export([create/0, insert/2, down/1, up/1, left/1, right/1, get_value/1, test/0]).


% n-ary tree with zipper

% each n-ary tree node is a zipper list,
% each element in the zipper list is a tuple of {Value, Child},
% where Child is a zipper list (ie the child node)


% create new tree
create() -> {zipper_list:create(), []}.

% create new tree node
create_node(Value) ->
	{Value, zipper_list:create()}.

% insert value into current focus of the tree
insert(Value, {ZList, Context}) ->
	NewNode = create_node(Value),
	NewZList = zipper_list:insert(NewNode, ZList),
	{NewZList, Context}.

% get value of current focus
get_value({ZList, _Context}) ->
	{Value, _Children} = zipper_list:get_focus(ZList),
	Value.


% moves focus down one level in the tree

% currently returns self when its hit bottom
% this could be easily changed based on actual implementation
down({ZList, Context}) ->
	{Value, Child} = zipper_list:get_focus(ZList),
	IsEmpty = zipper_list:is_empty(ZList),
	if IsEmpty ->
			{ZList, Context};
		true ->
			Rest = zipper_list:delete_focus(ZList),
			NewContext = [{Value, Rest} | Context],
			{Child, NewContext}
	end.


% moves focus up one level

% currently returns self when its hit top
% this could be easily changed based on actual implementation
up({_, []}=ZTree) -> ZTree;
up({ZList, Context}) ->
	[{Value, Rest}|CtxTail] = Context,
	%Nodes = [{Value, ZList} | Rest],
	NewZList = zipper_list:insert({Value, ZList}, Rest),
	{NewZList, CtxTail}.


% moves focus one list element to the right

% currently returns self when its hit end of list
% this could be easily changed based on actual implementation
right({ZList, Context}) ->
	NewZList = zipper_list:forward(ZList),
	{NewZList, Context}.

% moves focus one list element to the left

% currently returns self when its hit beginning of list
% this could be easily changed based on actual implementation
left({ZList, Context}) ->
	NewZList = zipper_list:backward(ZList),
	{NewZList, Context}.



test() ->
	Input = [
			{down, l},
			{right, m},
			{right, n},
			{up, nil},
		{right, b},
			{down, o},
			{up, nil},
		{right, c},
			{down, p},
			{right, q},
			{right, r},
			{up, nil},
		{right, d},
			{down, s},
			{right, t},
				{down, 1},
				{right, 2},
				{up, nil},
			{right, u},
			{right, v},
			{right, x}
	],


	M = zipper_nary_trees,
	ZTreeStart = zipper_nary_trees:create(),
	io:format("starting tree: ~p~n", [ZTreeStart]),
	ZTreeStart2 = zipper_nary_trees:insert(a, ZTreeStart),
	io:format("insert ~p into tree: ~p~n", [a, ZTreeStart2]),


	ZTreeEnd = lists:foldl(fun({Fun, Char}, ZTreeIn) ->
	io:format("command: ~p, insert: ~p~nzlist before: ~p~n", [Fun, Char, ZTreeIn]),
		ZTreeOut = M:Fun(ZTreeIn),
		ZT = if Char == nil -> ZTreeOut;
			true -> M:insert(Char, ZTreeOut)
		end,
		io:format("list after ~p~n~n", [ZT]),
		ZT
	end, ZTreeStart2, Input),

	_ExpectedValue = x,
	_Value = zipper_nary_trees:get_value(ZTreeEnd),
	io:format("end tree: ~p~n", [ZTreeEnd]),
	ok.

