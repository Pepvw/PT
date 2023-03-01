-module(grid).
-export([show_hlines/2, show_vlines/2, print/1, new/2, get_wall/3, add_wall/2,
    has_wall/2, get_cell_walls/2, get_all_walls/2, get_open_spots/1,
    choose_random_wall/1, legal_move/2, completed_boxes/1, game_completed/1,
    get_open_cell_walls/3, get_completable_walls/1]).

% Creates a new empty play-grid.
new(Width,Height) -> {Width, Height, []}. % TODO: ERROR HANDELING FOR 1 ARGUMENT ETC.

% Returns a tuple with the coordinates of a potential wall to the given direction
% of a cell.
get_wall(X,Y,Dir) ->
    if
        Dir == north -> {{X, Y-1}, {X, Y}};
        Dir == east -> {{X, Y}, {X+1, Y}};
        Dir == south -> {{X, Y}, {X, Y+1}};
        Dir == west -> {{X-1, Y}, {X, Y}};
        true -> -1 % TODO: ERROR HANDLEING ALS GEEN GELDIGE DIRECTIE WORDT GEKOZEN.
    end.

% Returns a new grid with a wall added on between the given tuple on the grid.
add_wall(Wall,Grid) ->
    Exists = has_wall(Wall, Grid),
    if
        (Exists == true) or (Wall == -1) -> Grid; % TODO: COORDINATEN BUITEN DE GRID MOET NIET KUNNEN?
        true -> {element(1, Grid), element(2, Grid), element(3, Grid) ++ [Wall]}
    end.

% Returns true if a wall has been placed on the given position.
has_wall(Wall,Grid) -> lists:member(Wall, element(3, Grid)).

% Returns true if the move is legal
legal_move(Wall, Grid) ->
    lists:member(Wall, get_open_spots(Grid)).

% Returns a string showing horizontal lines of the given row of the grid.
show_hlines(Row, Grid) ->
    Line_bool =  [has_wall(get_wall(X, Row, north), Grid) || X <- lists:seq(0, element(1, Grid)-1)], % Creates a bool list of the walls.
    Line_dash = lists:map(fun(X) -> % Converts true -> -- and false ->   .
        case X and true of
            true -> "--";
            false -> "  "
        end
    end, Line_bool),
    "+" ++ lists:concat(lists:join("+", Line_dash)) ++ "+~n". % Converts list to string.

% Returns a string showing the vertical lines of the given row of the grid.
show_vlines(Row, Grid) ->
    Line_bool = [has_wall(get_wall(X, Row, west), Grid) || X <- lists:seq(0, element(1, Grid))],
    Line_dash = lists:map(fun(X) -> % Converts true -> -- and false ->   .
        case X and true of
            true -> "|";
            false -> " "
        end
    end, Line_bool),
    lists:concat(lists:join("  ", Line_dash)) ++ "~n".

% Prints this grid in a structured format
% using the show_Xlines functions.
print(Grid) ->
    {_, H, _} = Grid,
    io:fwrite("\nGame board:\n\n"),

    lists:map(fun(Row) ->
        io:fwrite(show_hlines(Row, Grid)),

        case Row < H of
            true ->
                io:fwrite(show_vlines(Row, Grid));
            false ->
                ok
        end
    end, lists:seq(0, H)),

    io:fwrite("~n"),
    ok.

% Returns a list with the walls of a given cell position.
get_cell_walls(X,Y) ->
    [get_wall(X, Y, north),
     get_wall(X, Y, east),
     get_wall(X, Y, south),
     get_wall(X, Y, west)].

% returns a list with every wall in the grid, with the duplicates removed.
get_all_walls(W,H) ->
    lists:usort([X || C <- lists:seq(0,W-1),R <- lists:seq(0,H-1), X <- get_cell_walls(C,R)]).

% Returns a list with every possible wall that has not been built already.
get_open_spots(Grid) ->
    {W, H, _} = Grid,
    lists:subtract(get_all_walls(W, H), element(3, Grid)).

% Returns the number of completed boxes.
completed_boxes(Grid) ->
    {W, H, _} = Grid,
    length([{R, C} || C <- lists:seq(0,W-1),R <- lists:seq(0,H-1), get_open_spots(Grid) =:= lists:subtract(get_open_spots(Grid), get_cell_walls(C, R))]).

% Returns true if there are no more open spots. False if otherwise.
game_completed(Grid) ->
    Walls_free = get_open_spots(Grid),
    if
        Walls_free =:= [] -> true;
        true -> false
    end.

% Returns a random wall that has not been built yet.
choose_random_wall(Grid) ->
    Open_spots = get_open_spots(Grid),
    Open_spots_length = length(Open_spots),
    if
        Open_spots_length == 0 -> [];
        true -> lists:nth(rand:uniform(Open_spots_length), Open_spots)
    end.


% Returns the open walls for a given cell.
get_open_cell_walls(X,Y,Grid) ->
    lists:reverse(lists:sort([ Walls || Walls <- get_cell_walls(X, Y), has_wall(Walls, Grid) == false])). % TODO: ZIET ER TROEP UIT MAAR WAS NODIG OM TEST TE HALEN, HOE TE FIXEN?

% Returns a list with completable boxes. (Boxes where only one open wall is left).
get_completable_walls(Grid) ->
    {W, H, _} = Grid,
    lists:merge([get_open_cell_walls(C, R, Grid) || C <- lists:seq(0,W-1),R <- lists:seq(0,H-1), length(get_open_cell_walls(C, R, Grid)) == 1]).
