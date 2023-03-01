-module(users).

-export([get_users/0, get_females/1, split_by_age/1, get_id_name/1]).

get_users() ->
    [{user, 1, "Bob", male, 22},
     {user, 2, "Anna", female, 14},
     {user, 3, "Casper", male, 11},
     {user, 4, "Julia", female, 18}].

% Returns a list with the females.
get_females(Users) ->
    Females = [X || X <- Users, element(4,X) == female],
    Females.

% Returns two lists, one with ages <18, other with ages >= 18.
split_by_age(Users) ->
    Age_under = [X || X <- Users, element(5,X) < 18],
    Age_above = [X || X <- Users, element(5,X) >= 18],
    {Age_under, Age_above}.

% Returns a list with the ID and name of the users.
get_id_name(Users) ->
    Id_name = [{element(2,X), element(3,X)} || X <- Users],
    Id_name.
