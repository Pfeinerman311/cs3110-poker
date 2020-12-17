(** The abstract type of values representing a card *)
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace

type suit = Clubs | Diamonds | Hearts | Spades

type card = rank * suit

(** The abstract type of values representing a player *)
type player = {
  name : string;
  id : int;
  active: bool;
  stack : int;
  hole_cards: card list
}

type hand_tp = Royal_Flush | Straight_Flush | Four_Kind | Full_House
             | Flush | Straight | Three_Kind | Two_Pair | Pair
             | High_Card

type hand = {
  tp : hand_tp;
  cards : card list;
}

type rank_tal = {
  rank : rank;
  tally : int;
}

(** [hand_type h] returns the hand type of hand [h]. *)
val hand_type: hand -> hand_tp

(** [hand_cards h] returns the list of cards in hand [h]. *)
val hand_cards: hand -> card list

(** [card_rank c] returns the rank of card [c]. *)
val card_rank: card -> rank

(** [card_rank c] returns the rank of card [c]. *)
val card_suit: card -> suit

(** [first_card [c1; ... ; cn]] returns the first card in the
    list of cards [c1; ... ; cn] or raises an exception if the
    list is empty. *)
val first_card: card list -> card

(** [last_card [c1; ... ; cn]] returns the last card in the
    list of cards [c1; ... ; cn] or raises an exception if the
    list is empty. *)
val last_card: card list -> card

val get_shuffled_deck: unit -> card list

val compare : card -> card -> int

val hand_compare : hand -> hand -> int

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

val tp_to_string : hand_tp -> string

val hand_to_string : hand -> string