-module(client).
-export([move/0, new/0]).

% When message "finished" is receieved, return the message "I am done". If the
% tuple is received, call the game server to make a random move, or a move to
% complete a box if possible.
move() ->
    <<S1:32, S2:32, S3:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exs1024,{S1, S2, S3}),
    receive
        finished -> % TODO: NOG CLIENT STOPPEN NOG? "VERGEET NIET OM IN JE CLIENT DE RAND NUMBER GEN SEED AAN TE PASSEN"??
            io:format("~p: I am done~n", [self()]);

        {move, ServerPid, Grid} ->
            Completable_walls = length(grid:get_completable_walls(Grid)), % Checks for boxes that can be
            if % If there are boxes that can be compelted, complete a box, else place a random wall.
                % Completable_walls > 0 -> ServerPid ! {move, lists:last(grid:get_completable_walls(Grid))};
                Completable_walls > 0 -> gen_server:call(ServerPid, {move, lists:last(grid:get_completable_walls(Grid))});
                % true -> ServerPid ! {move, grid:choose_random_wall(Grid)}
                true -> gen_server:call(ServerPid, {move, grid:choose_random_wall(Grid)})
            end,
            move()
    end.


new() ->
    spawn(client, move, []).

