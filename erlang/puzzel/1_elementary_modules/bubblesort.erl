-module(bubblesort).
-export([sort/1]).

sort(L) -> sort(L, [], true).
sort([], L, true) ->
    lists:reverse(L);
sort([], L, false) ->
    sort(lists:reverse(L), [], true);
sort([ X, Y | T ], L, _) when X > Y ->
    sort([ X | T ], [ Y | L ], false);
    sort([ X | T ], L, Halt) ->
        sort(T, [ X | L ], Halt).

%https://stackoverflow.com/questions/5651060/erlang-bubble-sort
%
%round 1
%=> sort([2,4,3,5,1], [], true)
%=> sort([4,3,5,1], [2], true)
%=> sort([4,5,1], [3,2], false)
%=> sort([5,1], [4,3,2], false)
%=> sort([5], [1,4,3,2], false)
%=> sort([], [5,1,4,3,2], false)
%=> sort([2,3,4,1,5], [], true)
%
%round 2
%=> sort([3,4,1,5], [2], true)
%=> sort([4,1,5], [3,2], true)
%=> sort([4,5], [1,3,2], false)
%=> sort([5], [4,1,3,2], false)
%=> sort([], [5,4,1,3,2], false)
%=> sort([2,3,1,4,5], [], true)
%round 3
%=> sort([3,1,4,5], [2], true)
%=> sort([3,4,5], [1,2], false)
%=> sort([4,5], [3,1,2], false)
%=> sort([5], [4,3,1,2], false)
%=> sort([], [5,4,3,1,2], false)
%=> sort([2,1,3,4,5], true)
%round 4
%=> sort([2,3,4,5], [1], false)
%=> sort([3,4,5], [2,1], false)
%=> sort([4,5], [3,2,1], false)
%=> sort([5], [4,3,2,1], false)
%=> sort([], [5,4,3,2,1], false)
%=> sort([1,2,3,4,5], [], true)
%round 5
%=> sort([2,3,4,5], [1], true)
%=> sort([3,4,5],[2,1], true)
%=> sort([4,5],[3,2,1], true)
%=> sort([5],[4,3,2,1], true)
%=> sort([], [5,4,3,2,1], true)
%=> [1,2,3,4,5]

%


