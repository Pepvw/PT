-module(test_mergesort).

%%% This is a subset of the tests on Codegrade.

-include_lib("eunit/include/eunit.hrl").

-export([run_tests/0]).
-import(mergesort, [sort/1]).
-import(init, [stop/1]).

run_tests() ->
    halt(case eunit:test(?MODULE) of
        ok -> 0;
        _ -> 1
    end).

mergesort_test() ->
    ?assertEqual([1, 2, 25, 100, 200, 201], sort([201, 100, 200, 2, 25, 1])).


