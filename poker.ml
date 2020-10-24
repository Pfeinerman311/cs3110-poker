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

type hand_type = Royal_Flush | Straight_Flush | Four_Kind | Full_House
               | Flush | Straight | Three_Kind | Two_Pair | Pair
               | High_Card

type hand = {
  tp : hand_type;
  cards : card list;
}

(**
   let compare = 
   failwith "Unimplemented"
*)

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

let rec hand_combos cards size =
  match cards with
  | [] -> []
  | h :: t ->
    let h = List.map (fun x -> h :: x) (hand_combos t (size-1)) in
    let no_h = hand_combos t size in
    h@no_h


let deck = 
  deck_builder ranks []

let get_shuffled_deck () = 
  failwith "Unimplemented"

let get_stack p =  
  p.stack

let get_ID p =
  p.id

let get_best_hand p com_cards= 
  failwith "Unimplemented"

let is_active p = 
  p.active

let set_active p = 
  failwith "Unimplemented"

let set_inactive p = 
  failwith "Unimplemented"

let set_hole_cards p cards = 
  failwith "Unimplemented"

let alter_stack p amount = 
  {p with stack = p.stack + amount}