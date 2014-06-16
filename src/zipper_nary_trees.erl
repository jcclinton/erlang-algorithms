-module(zipper_nary_trees).

-export([create/0, insert/2, down/1, up/1, left/1, right/1, get_value/1]).
-compile([export_all]).


% n-ary tree with zipper

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
				{right, 3},
				{right, 4},
				{right, 5},
				{right, 6},
				{right, 7},
				{right, 8},
				{right, 9},
				{up, nil},
			{right, u},
			{right, v},
			{right, x}
	],


	M = zipper_nary_trees,
	ZTreeStart = zipper_nary_trees:create(),
	ZTreeStart2 = zipper_nary_trees:insert(a, ZTreeStart),

	ZTreeEnd = lists:foldl(fun({Fun, Char}, ZTreeIn) ->
		ZTreeOut = M:Fun(ZTreeIn),
		if Char == nil -> ZTreeOut;
			true -> M:insert(Char, ZTreeOut)
		end
	end, ZTreeStart2, Input),

	ExpectedValue = x,
	Value = zipper_nary_trees:get_value(ZTreeEnd),
	io:format("end tree: ~p~n", [ZTreeEnd]),
	ok.


create() -> {zipper_list:create(), []}.

create_node(Value) ->
	{Value, create()}.

insert(Value, {ZList, Context}) ->
	NewNode = create_node(Value),
	NewZList = zipper_list:insert(NewNode, ZList),
	{NewZList, Context}.

get_value({ZList, _Context}) ->
	zipper_list:get_focus(ZList).

down({ZList, Context}) ->
	{Value, Child} = zipper_list:get_focus(ZList),
	Rest = zipper_list:delete_focus(ZList),
	NewContext = [{Value, Rest} | Context],
	{Child, NewContext}.

up({List, []}) -> {List, []};
up({ZList, Context}) ->
	[{Value, Rest}|CtxTail] = Context,
	NewZList = zipper_list:insert(Value, Rest),
	{NewZList, CtxTail}.


right({ZList, Context}) ->
	NewZList = zipper_list:forward(ZList),
	{NewZList, Context}.

left({ZList, Context}) ->
	NewZList = zipper_list:backward(ZList),
	{NewZList, Context}.
