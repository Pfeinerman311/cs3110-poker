
(** The abstract type of values representing the game state. *)
type t

(** The type representing the result of an attempted bet *)
type result = Legal of t | Illegal

(** Initalizes a game state with the players in *)
val init_state : Poker.player list -> t

val current_player : t -> Poker.player

val raise : t -> Poker.player -> int -> result

val call : t -> Poker.player -> result

val fold : t -> Poker.player -> t