-module(zipper_list).

-export([create/0, insert/2, replace/2, forward/1, backward/1, get_focus/1, delete_focus/1, is_empty/1]).


% create zipper list
create() ->
	{[], []}.

% checks if a zlist is empty
is_empty(ZList) ->
	create() == ZList.


% insert element into zipper list at focus
insert(El, {Prev, Rest}) ->
	{Prev, [El|Rest]}.

replace(Val, {Prev, [_|Rest]}) ->
	{Prev, [Val|Rest]}.

% get current focus
get_focus({_, []}) -> {error, none};
get_focus({_, [Focus|_]}) -> Focus.

delete_focus({Prev, [_|Rest]}) -> {Prev, Rest}.


% move focus forward in the list
forward({_, []}=ZList) -> ZList;
forward({Prev, [Focus|Rest]}) -> {[Focus|Prev], Rest}.

% set previous element as focus
backward({[], _}=ZList) -> ZList;
backward({[Focus|Prev], Rest}) -> {Prev, [Focus|Rest]}.
