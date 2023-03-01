-module(game_server).

-behaviour(gen_server).

-export([start_link/1, handle_call/3, handle_cast/2, handle_continue/2]).
-export([init/1, move/2]).

start_link({W, H, Players}) ->
    gen_server:start_link(game_server, {W, H, Players}, []).

% Abstraction to make a move.
move(Pid, Wall) ->
    gen_server:call(Pid, {move, Wall}).


% TODO: You need to inform the first player to move.
init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    [First|_] = Players,
    First ! {move, self(), Grid},
    {ok, {Grid, Players}}.


% Handles continue for move.
% handle_continue(next_player, {Grid, [_One]}) ->
%     {noreply, {Grid, [_One]}};
handle_continue(next_player, {Grid, Players}) ->
    Game_completed = grid:game_completed(Grid),
    [Next|_] = Players,
    if
        Game_completed =:= false -> Next ! {move, self(), Grid}; % Informs next player to move.
        true -> lists:map(fun(X) ->
                X ! finished
            end, Players), % Sends finished atom to every player when game is finished.
            {stop, normal}
    end,
    {noreply, {Grid, Players}}.

% Handles call for move.
handle_call({move, Wall}, _From, State) ->
    {Grid, Players} = State,
    [Current_player|Tail] = Players,
    New_grid = grid:add_wall(Wall, Grid), % Adds new wall, if move is illegal grid stays the same.
    Score = grid:completed_boxes(New_grid) - grid:completed_boxes(Grid), % Score, calculated by number of completed boxes after move - completed boxes before move.
    if
        Score > 0 ->
            {reply, {ok, Score}, {New_grid, Players}, {continue, next_player}}; % Case a point(s) has been made.
        true ->
            New_players = lists:append(Tail, [Current_player]), % Case no points has been made.
            {reply, {ok, Score}, {New_grid, New_players}, {continue, next_player}} % Returns the reply with the score (0), and the new state.
    end;

% Used for testing.
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call({setWalls, Walls}, _From, {{W, H, _}, Players}) ->
    {reply, ok, {{W, H, Walls}, Players}}.

% Required for gen_server behaviour.
% Normally you would implement this too,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.
