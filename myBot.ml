open Bot

module Make = functor (I : BotInfo) -> struct

  module Info = I

  let get_cards_that_improve_hand hand stage = 
    failwith "Unimplemented"

  let calculate_prob_of_drawing_cards cards_list state = 
    let stage = State.get_stage state in
    (** the probability of drawing a specific cards in the next b draws 
        out of n cards is: (n - a) choose (b - a) / (n choose b) 
        the calculations in get_prob are pre-computed by this formula to save
        computation time *)
    let get_prob cards = 
      match List.length cards with
      | 1 -> begin 
          match stage with
          | Deal -> 0.1
          | Flop -> 2. /. 47.
          | Turn -> 1. /. 46.
          | _ -> failwith "this should be unreachable"
        end
      | 2 -> begin 
          match stage with
          | Deal -> 2. /. 245.
          | Flop -> 1. /. 1081.
          | Turn -> 0.
          | _ -> failwith "this should be unreachable"
        end
      | 3 -> begin 
          match stage with
          | Deal -> 1. /. 1960.
          | Flop -> 0.
          | Turn -> 0.
          | _ -> failwith "this should be unreachable"
        end
      | _ -> failwith "Unimplemented"
    in 
    List.map get_prob cards_list

  let calculate_probabilities_of_hands_winning hands state = 
    failwith "Unimplemented"

  let get_prob_of_winning hand_probs probs_hands_win = 
    failwith "Unimplemented"

  let formulate_bet prob = 
    failwith "Unimplemented"

  let get_action s p = 
    let stage = State.get_stage s in
    let hand = Poker.get_hole_cards in 
    failwith "Unimplemnted"

end