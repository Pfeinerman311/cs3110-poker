type player = {
  name : string
  (**TODO *)
}

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace

type suit = Clubs | Diamonds | Hearts | Spades

type card = rank * suit

let ranks =
  [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; 
   Jack; Queen; King; Ace]

let rec deck_builder ranks deck =
  match ranks with
  | [] -> deck
  | h::t -> deck_builder t deck@[(h, Clubs); (h, Diamonds); 
                                 (h, Hearts); (h, Spades)]

let deck = 
  deck_builder ranks []


let get_stack p =  
  failwith "Unimplemented"