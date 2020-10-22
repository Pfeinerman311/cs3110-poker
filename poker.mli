(** The abstract type of values representing a player *)
type player

(** The abstract type of values representing a card *)
type card

type hand

(** val compare : hand -> hand -> int *)

val get_stack : player -> int

val get_ID : player -> int

val is_active : player -> bool

val get_best_hand : player -> card list -> hand

val create_player : string -> int -> int -> player

val create_players : string list -> int -> player list

(** alters the players stack by amount*)
val alter_stack : player -> int -> player
