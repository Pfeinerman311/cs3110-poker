(** The abstraction for the main.ml file, which controls the interface of the
    poker game. *)

(** [print_community_cards st] prints the field community_cards in the state
    [st] *)
val print_community_cards : State.t -> bool -> unit

(** [get_command str] parses a user's input into a Command.command which is then
    processed into a state transition or some response from the interface. *)
val play_command : State.t -> Command.command -> State.t

(** [play_bots st] is a function that processes the actions of the "bots," aka
    the n-1 players at the table that are not the user. This necessarily occurs
    every round once the user's action is processed. *)
val play_bots : State.t -> State.t

(** [transition st trans] takes in a game state [st] (at whatever point in the 
    game of poker that it may represent) and applies to it a transition function
    [trans], which in turn creates a new State.t object with said transition 
    applied. *)
val transition : State.t -> (State.t -> State.t) -> State.t

(** [prompt_user st] parses a user's input into a command, and performs the
    command by applying an action_command (one of "raise," "call," or "fold") to
    the state [st], or alternatively, a show_command (one of "show table," 
    "show hole", "show hand") in order to print information to the user. *)
val prompt_user_command : State.t -> State.t

(** [print_state st] prints information about the current state of the game [st],
    for example, the very first time print_state is called, it should show the
    players in the game, their stacks, and the pot. Later it should include the
    community cards as well as the user's hole cards. *)
val print_state : State.t -> unit

(** [play_round st] takes in a state [st] and transition function and returns 
    the state of the game after said transition is applied. The pattern for each
    individual round is:
    i) Apply the transition function (one of "deal," "flop," "turn," "river")
    ii) Display the updated state of the game
    iii) Prompt the user for a command (the first of n player_actions)
    iv) Process the user's action, then the player_actions of the n-1 "bots."
    v) Display an updated state of the game (This time, after the round of bets)
*)
val play_round : State.t -> (State.t -> State.t) -> State.t


(** [play_game num_players] begins a game of poker with a certain amount of
    players [num_players]. Usually this will be referred to as an n-player game
    or n-player table, where n is [num_players] *)
val play_game : int -> unit