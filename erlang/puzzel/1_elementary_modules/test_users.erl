-module(test_users).

-include_lib("eunit/include/eunit.hrl").

-export([run_tests/0]).
-import(users, [get_females/1, get_id_name/1, split_by_age/1]).
-import(init, [stop/1]).

get_users() ->
    [{user, 1, "Bob", male, 22},
     {user, 2, "Helen", female, 14},
     {user, 3, "Bill", male, 11},
     {user, 4, "Kate", female, 18}].

run_tests() ->
    halt(case eunit:test(?MODULE) of
        ok -> 0;
        _ -> 1
    end).

get_females_test() ->
    ?assertEqual([], get_females([])),
    Answer = [{user, 2, "Helen", female, 14}, {user, 4, "Kate", female, 18}],
    ?assertEqual(Answer, get_females(get_users())),
    ?assertEqual(Answer, get_females([{user, 5, "Not Kate", male, 81} | get_users()])).

split_by_age_test() ->
    ?assertEqual({[], []}, split_by_age([])),
    Answer = {[{user, 2, "Helen", female, 14}, {user, 3, "Bill", male, 11}], [{user, 1, "Bob", male, 22}, {user, 4, "Kate", female, 18}]},
    ?assertEqual(Answer, split_by_age(get_users())).

get_id_name_test() -> 
    ?assertEqual([], get_id_name([])),
    Answer = [{1, "Bob"}, {2, "Helen"}, {3, "Bill"}, {4, "Kate"}],
    ?assertEqual(Answer ++ [{5, "Not Kate"}], get_id_name(get_users() ++ [{user, 5, "Not Kate", male, 81}])).