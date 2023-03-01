-module(test_pi).

%%% This is a subset of the tests on Codegrade.

-include_lib("eunit/include/eunit.hrl").

-compile({pi, []}).
-export([run_tests/0]).
-import(pi, [pi/0]).
-import(init, [stop/1]).

run_tests() ->
    halt(case eunit:test(?MODULE) of
        ok -> 0;
        _ -> 1
    end).

pi_test() ->
    ?assert(abs(pi() - 3.14159) < 0.001).


