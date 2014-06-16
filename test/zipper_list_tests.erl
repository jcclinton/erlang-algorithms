-module(zipper_list_tests).
-include_lib("eunit/include/eunit.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zipper_list_test_() ->
[
	{setup, fun build_list1/0, fun stop/1, fun check/1}
].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_list1() ->
	Input = [
		{insert, a},
		{insert, b},
		{insert, c},
		{forward, nil},
		{replace, d},
		{forward, nil},
		{replace, e},
		{backward, nil},
		{backward, nil}
	],


	M = zipper_list,
	ZList = zipper_list:create(),

	ZListOut = lists:foldl(fun({Fun, Char}, ZListIn) ->
		if Char == nil ->
				M:Fun(ZListIn);
			true ->
				M:Fun(Char, ZListIn)
		end
	end, ZList, Input),

	ExpectedValue = c,
	{ExpectedValue, ZListOut}.
		





stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check({ExpectedResult, ZListOut}) ->
	Value = zipper_list:get_focus(ZListOut),
	[?_assertEqual(ExpectedResult, Value)].
