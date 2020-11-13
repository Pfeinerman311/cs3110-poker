exception UnknownRank
exception UnknownSuit

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace

type suit = Clubs | Diamonds | Hearts | Spades

type card = rank * suit

type player = {
  name : string;
  id : int;
  active: bool;
  stack : int;
  hole_cards: card list
}

let ranks =
  [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; 
   Jack; Queen; King; Ace]

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

let hand_type hand =
  match hand with
  | (h, t) -> h

let hand_cards hand =
  match hand with
  | (h, t) -> t

let card_rank card =
  match card with
  | (h, t) -> h

let card_suit card =
  match card with
  | (h, t) -> t

let first_card cards =
  match cards with
  | [] -> failwith "No cards"
  | h::t -> h

let rec last_card cards =
  match cards with
  | [] -> failwith "No cards"
  | h::t -> if t = [] then h 
    else last_card t

let rank_to_int rank =
  match rank with
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14

let suit_to_int suit =
  match suit with
  | Clubs -> 0
  | Diamonds -> 1
  | Hearts -> 2
  | Spades -> 3

let tp_to_int tp =
  match tp with
  | High_Card -> 0
  | Pair -> 1
  | Two_Pair -> 2
  | Three_Kind -> 3
  | Straight -> 4
  | Flush -> 5
  | Full_House -> 6
  | Four_Kind -> 7
  | Straight_Flush -> 8
  | Royal_Flush -> 9


let compare c1 c2 = 
  let c1_rank = c1 |> card_rank |> rank_to_int in
  let c2_rank = c2 |> card_rank |> rank_to_int in
  match c1_rank - c2_rank with
  | 0 -> (c1 |> card_suit |> suit_to_int) - (c2 |> card_suit |> suit_to_int)
  | x -> x

let hand_compare h1 h2 =
  let h1_type = h1.tp |> tp_to_int in
  let h2_type = h2.tp |> tp_to_int in
  match h1_type - h2_type with
  | 0 -> let h1_best = last_card h1.cards in
    let h2_best = last_card h2.cards in
    compare h1_best h2_best
  | x -> x

let same_rank c1 c2 =
  let c1_rank = c1 |> card_rank |> rank_to_int in
  let c2_rank = c2 |> card_rank |> rank_to_int in
  c1_rank - c2_rank = 0

let create_player name id stack =
  let p = {name= name;id=id;active=true;stack=stack;hole_cards=[]} in
  p

let rec create_player_helper names cur stack acc = 
  match names with
  | [] -> acc
  | h::t -> let new_player = create_player h cur stack in
    create_player_helper t (cur+1) stack (new_player::acc)

let create_players names stack =
  create_player_helper (List.rev names) 0 stack []

let rec deck_builder ranks deck =
  match ranks with
  | [] -> deck
  | h::t -> deck_builder t deck@[(h, Clubs); (h, Diamonds); 
                                 (h, Hearts); (h, Spades)]

(**
   let shuffle d = 
   Random.self_init ();
   let arr = Array.of_list d in
   for x = (Array.length arr - 1) downto 1 do
    let a = Random.int (x + 1) in
    let b = arr.(a) in
    arr.(a) <- arr.(x);
    arr.(x) <- b
   done;
   Array.to_list arr
*)

let shuffle deck =
  Random.self_init ();
  let nd = List.map (fun x -> (Random.bits (), x)) deck in
  let sond = List.sort Stdlib.compare nd in
  List.map snd sond

let tal_compare t1 t2 =
  (rank_to_int t1.rank) -(rank_to_int t2.rank)

let rec tally_upd card tally =
  match tally with
  | [] -> [{rank = card_rank card; tally = 1}]
  | h::t -> if h.rank = card_rank card then
      {rank = h.rank; tally = h.tally + 1}::t else
      h::(tally_upd card t)

let rec rank_tally (cards : card list) tally =
  match cards with
  | [] -> tally
  | h::t -> rank_tally t (tally_upd h tally)

let tally_filter cards n =
  rank_tally cards [] 
  |> List.filter (fun x -> x.tally = n) 
  |> List.sort_uniq tal_compare

let get_rank_cards cards rank =
  cards 
  |> List.filter (fun x -> card_rank x = rank) 
  |> List.sort_uniq compare

let rec sub_list list n sub =
  if n = 0 then sub else
    match list with
    | [] -> failwith "List isn't big enough."
    | h::t -> sub_list t (n-1) (h::sub)

let high_card cards =
  let high = cards |> List.sort_uniq compare |> List.rev |> first_card in
  {tp = High_Card; cards = [high]}

let pair_check cards =
  match List.rev (tally_filter cards 2) with
  | [] -> failwith "No Pairs."
  | h::t -> {tp = Pair; cards = get_rank_cards cards h.rank}

let two_pair_helper cards tally =
  let pairs = sub_list tally 2 [] 
              |> List.map (fun x -> get_rank_cards cards x.rank) 
              |> List.flatten in
  {tp = Two_Pair; cards = pairs}

let two_pair_check cards = 
  let twos = List.rev (tally_filter cards 2) in
  if List.length twos < 2 
  then pair_check cards
  else two_pair_helper cards twos

let three_kind_check cards =
  match List.rev (tally_filter cards 3) with
  | [] -> two_pair_check cards
  | h::t -> {tp = Three_Kind; cards = get_rank_cards cards h.rank}

let rec straight_helper cards n =
  if List.length cards = 1 && n = 4 then true else
    match cards with
    | [] -> failwith "Must be 5 cards."
    | h::t -> let c1 = h in
      let c2 = first_card t in
      if same_rank c1 c2 || compare c1 c2 != -1 
      then false
      else straight_helper t (n+1)

let rec straight_check to_check checked =
  match to_check with
  | [] -> failwith "No Straight"
  | h::t -> if straight_helper h 0
    then {tp = Straight; cards = h}
    else straight_check t (checked@[h])

let rec flush_helper cards n =
  if List.length cards = 1 && n = 4 then true else
    match cards with
    | [] -> failwith "Must be 5 cards."
    | h::t -> if card_suit h = (t |> first_card |> card_suit) 
      then flush_helper t (n+1)
      else false

let rec flush_check to_check checked =
  match to_check with
  | [] -> straight_check checked []
  | h::t -> if flush_helper h 0
    then {tp = Flush; cards = h}
    else flush_check t (checked@[h])

let full_house_helper cards tally =
  match List.rev tally with
  | [] -> failwith "No Full House."
  | h::t -> let r1 = h.rank in
    let sorted = t |> List.sort_uniq tal_compare |> List.rev in
    match sorted with
    | [] -> failwith "No Full House."
    | h::t -> let r2 = h.rank in
      let f2 = (r2 |> get_rank_cards cards |> List.rev) in
      let hand = List.sort_uniq compare 
          (sub_list f2 2 [])@(get_rank_cards cards r1) in
      {tp = Full_House; cards = hand}

let full_house_check cards =
  let threes = tally_filter cards 3 in
  match threes with
  | [] -> failwith "No Full House."
  | h::t -> let twos = tally_filter cards 2 in
    full_house_helper cards (twos@threes)

let four_kind_check cards =
  match tally_filter cards 4 with
  | [] -> full_house_check cards
  | h::t -> {tp = Four_Kind; cards = get_rank_cards cards h.rank}

let straight_flush_helper cards =
  (straight_helper cards 0) && (flush_helper cards 0)

let rec straight_flush_check to_check checked =
  match to_check with
  | [] -> failwith "No straight flush."
  | h::t -> if straight_flush_helper h
    then {tp = Straight_Flush; cards = h}
    else straight_flush_check t (checked@[h])

let royal_helper cards =
  match cards with
  | [] -> failwith "No cards."
  | h::t -> if card_rank h = Ten
    then straight_flush_helper cards
    else false

let rec royal_check to_check checked =
  match to_check with
  | [] -> straight_flush_check checked []
  | h::t -> if royal_helper h
    then {tp = Royal_Flush; cards = h}
    else royal_check t (checked@[h])


let rec card_combos cards size =
  if size <= 0 then [[]]
  else
    match cards with
    | [] -> []
    | h::t ->
      let h = List.map (fun x -> h::x) 
          (card_combos (List.sort_uniq compare t) (size-1)) in
      let no_h = card_combos (List.sort_uniq compare t) size in
      h@no_h


let deck = 
  deck_builder ranks []

let get_shuffled_deck () = 
  shuffle deck

let get_stack p =  
  p.stack

let get_ID p =
  p.id

let get_name p =
  p.name

let get_hole_cards p = 
  p.hole_cards

let get_best_hand p com_cards= 
  let hole = get_hole_cards p in
  let full = List.sort_uniq compare hole@com_cards in
  let combos = List.rev (card_combos full 5) in
  match royal_check combos [] with
  | x -> x
  | exception e ->
    match four_kind_check full with
    | x -> x
    | exception e -> 
      match flush_check combos [] with
      | x -> x
      | exception e ->
        match three_kind_check full with
        | x -> x
        | exception e -> high_card hole


let is_active p = 
  p.active

let set_active p = 
  {p with active=true}

let set_inactive p = 
  {p with active=false}

let set_hole_cards p cards = 
  {p with hole_cards=cards}

let get_hole_cards p =
  p.hole_cards

let alter_stack p amount = 
  {p with stack = p.stack + amount}

let rank_to_string rank =
  match rank with
  | Two -> "Two"
  | Three -> "Three"
  | Four -> "Four"
  | Five -> "Five"
  | Six -> "Six"
  | Seven -> "Seven"
  | Eight -> "Eight"
  | Nine -> "Nine"
  | Ten -> "Ten"
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"
  | Ace -> "Ace"

let suit_to_string suit =
  match suit with
  | Clubs -> "Clubs"
  | Diamonds -> "Diamonds"
  | Hearts -> "Hearts"
  | Spades -> "Spades"

let tp_to_string tp =
  match tp with
  | High_Card -> "High Card"
  | Pair -> "Pair"
  | Two_Pair -> "Two Pair"
  | Three_Kind -> "Three of a Kind"
  | Straight -> "Straight"
  | Flush -> "Flush"
  | Full_House -> "Full House"
  | Four_Kind -> "Four of a Kind"
  | Straight_Flush -> "Straight Flush"
  | Royal_Flush -> "Royal Flush"

let card_to_string card =
  let rank = card |> card_rank |> rank_to_string in
  let suit = card |> card_suit |> suit_to_string in
  String.concat "" ["("; rank; " of "; suit; ")"]

let rec card_list_to_string_list cards =
  match cards with
  | [] -> []
  | h::t -> [card_to_string h]@(card_list_to_string_list t)

let card_list_to_string cards =
  String.concat ", " (card_list_to_string_list cards)

let rec combos_to_string_list cards_list =
  match cards_list with
  | [] -> []
  | h::t -> [card_list_to_string h]@(combos_to_string_list t)

let hand_to_string hand =
  String.concat " of " 
    [tp_to_string hand.tp; card_list_to_string hand.cards]