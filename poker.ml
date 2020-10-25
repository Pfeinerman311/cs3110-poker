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


let compare c1 c2 = 
  let c1_rank = c1 |> card_rank |> rank_to_int in
  let c2_rank = c2 |> card_rank |> rank_to_int in
  match c1_rank - c2_rank with
  | 0 -> (c1 |> card_suit |> suit_to_int) - (c2 |> card_suit |> suit_to_int)
  | x -> x

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

let shuffle d = 
  let arr = Array.of_list d in
  for x = (Array.length arr - 1) downto 1 do
    let a = Random.int (x + 1) in
    let b = arr.(a) in
    arr.(a) <- arr.(x);
    arr.(x) <- b
  done;
  Array.to_list arr

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
  failwith "Unimplemented"

let is_active p = 
  p.active

let set_active p = 
  {p with active=true}

let set_inactive p = 
  {p with active=false}

let set_hole_cards p cards = 
  {p with hole_cards=cards}

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