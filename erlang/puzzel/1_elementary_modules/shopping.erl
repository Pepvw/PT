-module(shopping).
-export([total/1, total2/1, double_shopping/1]).

% recursively prints each item and the amount.
total(L) when length(L) == 0 -> ok;
total([H|T]) ->
    io:format('~w\n', [H]),
    total(T).

% Returns the total amount of items in the shopping list.
total2(L) ->
    All = [element(2,X) || X <- L ],
    All.

% Doubles the items needed in the shopping list.
double_shopping(L) ->
    All = [{element(1,X), 2 *element(2,X)} || X <- L ],
    All.