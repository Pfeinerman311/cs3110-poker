(** The abstract type of the stage of the game *)
type stage = Init | Deal | Flop | Turn | River

(** The abstract type of values representing the game state. *)
type t

(** The type representing the result of an attempted bet *)
type result = Legal of t | Illegal

(** Initalizes a game state with the players. Blind is small blind big_blind is 
    2x small blind *)
val init_state : Poker.player list -> int -> result

(** [get_subgame t] returns how many subgames have past
    Example: when the game is first initalized, [get_subgame t] returns 0
          after end_subgame has been called once [get_subgame t] returns 1*)
val get_subgame : t -> int

(** [get stage t] returns the current stage of the subgame *)
val get_stage : t -> stage

(** [current_player t] returns the player to act*)
val current_player : t -> Poker.player

(** [get_players t] returns a list of players in the game*)
val get_players : t -> Poker.player list

(** [get_active_players t] returns a list of ACTIVE players in the game*)
val get_active_players : t -> Poker.player list

(** [get_big_blind t] returns the player who currently has the big blind *)
val get_big_blind : t -> Poker.player

(** [get_community_cards t] returns a list containing the community cards.
    'community cards' refers to the cards on the table not in players hands
    i.e. the cards from the flop turn and river*)
val get_community_cards : t -> Poker.card list

(** [get_pot t] returns the pot
    The pot is the sum of all the players bets *)
val get_pot : t -> int

(** [get_call_cost t] returns the call cost
    the call cost is the price a player must bet to continue playing *)
val get_call_cost : t -> int

(** [get_deck] returns all the cards that have not been delt *)
val get_deck : t -> Poker.card list

(** [incr_subgame t] increments the subgame counter by 1*)
val incr_subgame : t -> t

(** [incr_stage t] returns t with the next state
    example: if t was in stage Flop, [incr_state t] would return
    {t with stage=Turn} *)
val incr_stage : t -> t

(** [raise t p amount] returns legal of t if p has more than amount in stack
    and Illegal otherwise. t is also updated to include new call cost, pot, and 
    updated players *)
val raise : t -> Poker.player -> int -> result

(** [call t p] returns legal of t if player p has enough in stack to cover the cost
    to call. returns Illegal otherwise. if legal, t is updated to reflect new 
    pot and player stacks. *)
val call : t -> Poker.player -> result

(** [fold t p] returns t with p as inactive. inactive players are no longer able
    to win the subgame. *)
val fold : t -> Poker.player -> t

(** [deal t] gives all players two hole cards *)
val deal : t -> t

(**  [flop t] adds three cards from the deck to the community cards. These
     cards are removed from the deck.*)
val flop : t -> t

(** [turn t] adds one card from the deck to the community cards. This card is 
    removed from the deck*)
val turn : t -> t

(** [river t] adds one card from the deck to the community cards. This card is 
    removed from the deck*)
val river : t -> t

(** [end_subgame t} ends current subgame. 
    the returned state gives winner(s) the pot and moves blinds. 
    it also gets a newly shuffled deck and resets the pot and call costs*)
val end_subgame : t -> t
