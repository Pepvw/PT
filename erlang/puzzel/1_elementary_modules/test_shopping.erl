-module(test_shopping).

-include_lib("eunit/include/eunit.hrl").

-compile({pi, []}).
-export([run_tests/0]).
-import(shopping, [total2/1, double_shopping/1]).
-import(init, [stop/1]).

run_tests() ->
    halt(case eunit:test(?MODULE) of
        ok -> 0;
        _ -> 1
    end).

get_list() -> [{oranges, 3}, {milk, 1}, {pizza, 2}, {beer, 5}].

total2_test() ->
    ?assertEqual([], total2([])),
    ?assertEqual([3, 1, 2, 5], total2(get_list())).

double_shopping_test() ->
    ?assertEqual([], double_shopping([])),
    ?assertEqual([{oranges, 6}, {milk, 2}, {pizza, 4}, {beer, 10}], double_shopping(get_list())).