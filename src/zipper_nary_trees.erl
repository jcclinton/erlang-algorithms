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


create() -> {zipper_list:create(), []}.

create_node(Value) ->
	{Value, zipper_list:create()}.

insert(Value, {ZList, Context}) ->
	NewNode = create_node(Value),
	NewZList = zipper_list:insert(NewNode, ZList),
	{NewZList, Context}.

get_value({ZList, _Context}) ->
	{Value, _ZListBelow} = zipper_list:get_focus(ZList),
	Value.


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

up({List, []}) -> {List, []};
up({ZList, Context}) ->
	[{Value, Rest}|CtxTail] = Context,
	%Nodes = [{Value, ZList} | Rest],
	NewZList = zipper_list:insert({Value, ZList}, Rest),
	{NewZList, CtxTail}.


right({ZList, Context}) ->
	NewZList = zipper_list:forward(ZList),
	{NewZList, Context}.

left({ZList, Context}) ->
	NewZList = zipper_list:backward(ZList),
	{NewZList, Context}.
