(** The abstract type of values representing a player *)
type player

(** The abstract type of values representing a card *)
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace

type suit = Clubs | Diamonds | Hearts | Spades

type card = rank * suit

type hand_tp = Royal_Flush | Straight_Flush | Four_Kind | Full_House
             | Flush | Straight | Three_Kind | Two_Pair | Pair
             | High_Card

type hand = {
  tp : hand_tp;
  cards : card list;
}


val get_shuffled_deck: unit -> card list

(** val compare : hand -> hand -> int *)

val card_combos : card list -> int -> (card list) list

val get_stack : player -> int

val get_ID : player -> int

val get_name : player -> string

val is_active : player -> bool

val set_hole_cards : player -> card list -> player

val get_hole_cards : player -> card list

val get_best_hand : player -> card list -> hand

val create_player : string -> int -> int -> player

val create_players : string list -> int -> player list

(** alters the players stack by amount*)
val alter_stack : player -> int -> player

val set_active : player -> player

val set_inactive : player -> player

val card_list_to_string_list : card list -> string list

val card_list_to_string : card list -> string

val combos_to_string_list : (card list) list -> string list