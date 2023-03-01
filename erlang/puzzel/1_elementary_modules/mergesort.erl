-module(mergesort).
-export([sort/1]).

% Sorts a given list.
sort([]) -> [] ;
sort([X]) -> [X] ;
sort(Xs) ->
    {L, R} = lists:split(length(Xs) div 2, Xs),
    merge(sort(L), sort(R)).

% If an empty list is passed as an argument, only the non-empty list is returned.
merge(L, []) ->
    L;
merge([], R) ->
   R;
% Recursively merges the atoms based on which one is smaller.
merge([H1 | T1], [H2 | T2]) ->
    if
        H1 > H2 -> [H2] ++ merge([H1 | T1], T2);
        true    -> [H1] ++ merge(T1, [H2 | T2])
    end.
