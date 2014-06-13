-module(zipper_list).

-export([new/0, insert/2, replace/2, forward/1, backward/1, get_focus/1]).


% create new zipper list
new() -> {[], []}.


% insert element into zipper list at focus
insert(El, {Prev, Rest}) -> {Prev, [El|Rest]}.

replace(Val, {Prev, [_|Rest]}) -> {Prev, [Val|Rest]}.

% get current focus
get_focus({_, []}) -> {error, none};
get_focus({_, [Focus|_]}) -> Focus.


% move focus forward in the list
forward({Prev, [Focus|Rest]}) -> {[Focus|Prev], Rest}.

% set previous element as focus
backward({[], Rest}) -> {[], Rest};
backward({[Focus|Prev], Rest}) -> {Prev, [Focus|Rest]}.
