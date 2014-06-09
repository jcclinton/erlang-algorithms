-module(genetic_worker).


-export([start_link/2, run/2]).
-compile([export_all]).



start_link(ServerPid, Bytes) ->
	%io:format("starting worker with pid ~p and bytes ~p~n", [ServerPid, Bytes]),
	spawn_link(?MODULE, run, [ServerPid, Bytes]).


run(ServerPid, Bytes) ->
	%io:format("running worker~n"),
	Num = calc_bytes(Bytes),
	gen_server:cast(ServerPid, {success, Num}).



calc_bytes(Bytes) ->
	Symbols = decode_bytes(Bytes),

	{_, Equation} = lists:foldl(fun(Symbol, AccIn={Previous, List}) ->
		if Symbol == null -> AccIn;
			is_number(Symbol) ->
				if Previous == null -> {Symbol, List ++ [Symbol]};
					is_number(Previous) -> {Previous, List};
					true -> {Symbol, List ++ [Symbol]}
				end;
			is_atom(Symbol) ->
				if Previous == null -> AccIn;
					is_number(Previous) -> {Symbol, List ++ [Symbol]};
					true -> AccIn
				end
		end
	end, {null, []}, Symbols),
	Length = length(Equation),
	Result = if Length == 0 -> 0;
		true ->
			[Head|Rest] = lists:reverse(Equation),
			if is_atom(Head) ->
				compute_equation(lists:reverse(Rest));
			true ->
				compute_equation(lists:reverse(Equation))
		end
	end,
	io:format("equation: ~p~nresult: ~p~n~n", [lists:reverse(Equation), Result]),
	Result.


compute_equation([N]) -> N;
compute_equation(Equation) ->
	compute_equation(Equation, null).

compute_equation([], Acc) -> Acc;
compute_equation([N1|[S|[N2|Rest]]], null) ->
	NewAcc = compute_equation_step(N1, N2, S),
	compute_equation(Rest, NewAcc);
compute_equation([S|[N2|Rest]], Acc) ->
	NewAcc = compute_equation_step(Acc, N2, S),
	compute_equation(Rest, NewAcc).

compute_equation_step(N1, N2, S) ->
	case S of
		'+' -> N1 + N2;
		'-' -> N1 - N2;
		'*' -> N1 * N2;
		'/' ->
			if N2 == 0 -> 0;
				true -> N1 / N2
			end;
		_ -> io:format("bad case symbol: ~p~n", [S]), 0
	end.



decode_bytes(Bytes) ->
	decode_bytes(Bytes, []).

decode_bytes(<<>>, Acc) -> Acc;
decode_bytes(<<N1:4/unsigned-integer, N2:4/unsigned-integer, Rest/binary>>, Acc) ->
	S1 = get_symbol(N1),
	S2 = get_symbol(N2),
	decode_bytes(Rest, Acc ++ [S1] ++ [S2]).

get_symbol(Num) ->
	case Num of
		10 -> '+';
		11 -> '-';
		12 -> '*';
		13 -> '/';
		14 -> null;
		15 -> null;
		Int -> Int
	end.
