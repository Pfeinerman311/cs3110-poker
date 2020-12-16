open Bot

module Make = functor (I : BotInfo) -> struct

  module Info = I

  let get_cards_that_improve_hand hand stage = 
    failwith "Unimplemented"

  let calculate_prob_of_drawing_cards cards state = 
    failwith "Unimplemented"

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