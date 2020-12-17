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

(** [sub_list [a1; ... ; an] n [a1; ... ; an]] returns a sub-list of
    [a1; ... ; an] consisting of the first [n] items in the list, or
    raises an exception if the list is empty. *)
val sub_list: 'a list -> int -> 'a list -> 'a list

(** [same_rank c1 c2] returns true if cards [c1] and [c2] are the same
    rank and false if they are not. *)
val same_rank: card -> card -> bool

(** [compare c1 c2] compares cards [c1] and [c2], first by rank,
    then by suit if ranks are equal (suits ordered alphabetically),
    and returns a positive int if [c1] is higher, a negative int if
    [c2] is higher, or a 0 if they are equal. *)
val compare: card -> card -> int

(** [hand_compare h1 h2] compares hands [h1] and [h2], first by hand type,
    then by highest card if hands are equal, and returns a positive int 
    if [h1] is higher, a negative int if [h2] is higher, or a 0 if they
    are equal. *)
val hand_compare: hand -> hand -> int

(** [shuffle [c1; ... ; cn]] randomly shuffles the list of cards
    [c1; ... ; cn] and returns the new shuffled list. *)
val shuffle: card list -> card list

(** [deck] returns the standard 52 playing card deck (without Jokers) in
    an unshuffled order. *)
val deck: card list

(** [get_shuffled_deck ()] returns a uniquely shuffled full deck each
    time it is called. *)
val get_shuffled_deck: unit -> card list

(** [create_player name id stack] creates a new active player value with
    name [name], id [id], and stack [stack]. *)
val create_player: string -> int -> int -> player

(** [create_players names stack] creates a list of players for each name
    in [names], each with an automatically generated id and stack [stack] *)
val create_players: string list -> int -> player list

(** [get_id p] returns the id for player [p]. *)
val get_ID : player -> int

(** [get_name p] returns the name for player [p]. *)
val get_name : player -> string

(** [is_active p] returns the boolean value for player [p]'s active
    status. *)
val is_active : player -> bool

(** [set_active p] sets the boolean value for player [p]'s active
    status to true. *)
val set_active : player -> player

(** [set_inactive p] sets the boolean value for player [p]'s active
    status to false. *)
val set_inactive : player -> player

(** [get_stack p] returns the stack for player [p]. *)
val get_stack : player -> int

(** [alter_stack p amnt] changes the stack for player [p] to [amnt]. *)
val alter_stack : player -> int -> player

(** [get_hole_cards p] returns the list of hole cards for player [p]. *)
val get_hole_cards : player -> card list

(** [set_hole_cards p cards] sets the hole cards for player [p] to
    [cards]. *)
val set_hole_cards : player -> card list -> player

val card_combos : card list -> int -> (card list) list

val get_best_hand : player -> card list -> hand

val card_list_to_string_list : card list -> string list

val card_list_to_string : card list -> string

val combos_to_string_list : (card list) list -> string list

val tp_to_string : hand_tp -> string

val hand_to_string : hand -> string