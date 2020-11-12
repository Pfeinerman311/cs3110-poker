(** The abstract type of the stage of the game *)
type stage = Init | Deal | Flop | Turn | River

(** The abstract type of values representing the game state. *)
type t

(** The type representing the result of an attempted bet *)
type result = Legal of t | Illegal

(** Initalizes a game state with the players in *)
val init_state : Poker.player list -> result

val get_subgame : t -> int

val get_stage : t -> stage

val current_player : t -> Poker.player

val get_players : t -> Poker.player list

val get_big_blind : t -> Poker.player

val get_community_cards : t -> Poker.card list

val get_pot : t -> int

val get_call_cost : t -> int

val get_deck : t -> Poker.card list

val incr_subgame : t -> t

val incr_stage : t -> t

val raise : t -> Poker.player -> int -> result

val call : t -> Poker.player -> result

val fold : t -> Poker.player -> t

val deal : t -> t

val flop : t -> t

val turn : t -> t

val river : t -> t

(** ends current round. Gives winner the pot and moves blind.*)
val end_round : t -> t
