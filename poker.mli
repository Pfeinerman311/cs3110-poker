(** The abstract type of values representing a player *)
type player

(** The abstract type of values representing a card *)
type card

type hand


val get_shuffled_deck: unit -> card list

(** val compare : hand -> hand -> int *)

val get_stack : player -> int

val get_ID : player -> int

val is_active : player -> bool

val set_hole_cards : player -> card list -> player

val get_best_hand : player -> card list -> hand

val create_player : string -> int -> int -> player

val create_players : string list -> int -> player list

(** alters the players stack by amount*)
val alter_stack : player -> int -> player

val set_active : player -> player

val set_inactive : player -> player