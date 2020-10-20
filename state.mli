
(** The abstract type of values representing the game state. *)
type t

(** The abstract type of values representing a player *)
type player

(** The type representing the result of an attempted bet *)
type result = Legal of t | Illegal

(** Initalizes a game state with the players in *)
val init_state : player list -> t

val raise : t -> player -> int -> result

val call : t -> player -> result

val fold : t -> player -> t