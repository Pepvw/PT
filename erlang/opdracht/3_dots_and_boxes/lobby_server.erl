-module(lobby_server).

-behaviour(gen_server).

-export([start/0, handle_call/3, handle_cast/2, handle_info/2]).
-export([init/1, new_game/3, games/0]).

% Starts a new game.
new_game(W, H, Players) ->
    gen_server:call(lobby_server, {new_game, W, H, Players}).

% Gets every current game.
games() ->
    gen_server:call(lobby_server, games).

% Call this when you want to start the lobby server.
start() ->
    gen_server:start({local, lobby_server}, lobby_server, [], []).

init([]) ->
    {ok, []}.

% Handles the call for a new game. Creates the new game, adds the PID to the
% state, and monitors the game. When the game crashes or is finished, handle_info
% picks it up and removes it from the state.
handle_call({new_game, W, H, Players}, _From, State) ->
    process_flag(trap_exit, true),
    {ok, Pid} = game_server:start_link({W, H, Players}),
    New_state = State ++ [Pid],
    {reply, {ok, Pid}, New_state};
% Returns a reply with the current state (a list with ongoing game-server PID's).
handle_call(games , _From, Games) ->
    {reply, Games, Games}.


% When a game is finished or crashes, removes this game from the state.
handle_info({_, Pid_game, _Reason} , State) ->
    {noreply, lists:delete(Pid_game, State)}.

% Required for gen_server behaviour.
% Normally you would implement this to,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.
