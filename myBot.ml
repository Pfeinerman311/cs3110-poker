open Bot

module Make = functor (I : BotInfo) -> struct

  module Info = I

  type outs = {
    hand_type : Poker.hand_tp;
    single : float;
    double : float;
    tripple : float;
  }
  type out_prob = {
    hand_type : Poker.hand_tp;
    prob: float;
  }

  let choose n k = 
    let rec helper i acc = 
      if i = k then acc
      else let prod = (n +. 1. -. i) /. i in
        helper (i +. 1.) (acc *.  prod)
    in
    helper 1.0 1.0


  let get_cards_that_improve_hand player com_cards stage = 
    let best_hand = Poker.get_best_hand player com_cards in 
    match best_hand.tp with 
    | Royal_Flush -> failwith ""
    | Straight_Flush -> failwith ""
    | Four_Kind -> failwith ""
    | Full_House -> failwith ""
    | Flush -> failwith ""
    | Straight -> failwith ""
    | Three_Kind -> failwith ""
    | Two_Pair -> failwith ""
    | Pair -> failwith ""
    | High_Card -> failwith ""

  let calculate_prob_of_drawing_cards outs_list state = 
    let stage = State.get_stage state in
    (** the probability of drawing a specific cards in the next b draws 
        out of n cards is: (n - a) choose (b - a) / (n choose b)

        explanation: if you are choosing [a] cards then those are fixed.
        therefore, you need to ask the question: "how many combinations of the 
        remaining cards are there?" Well, you have n-a choises (you can't pick 
        the [a] cards you already picked) for b-a draws. 
        As such, there are (n-a) choose (b-a) hands which contain the [a] cards
        you care about. In total, there are n choose b hands of b cards. 
        This gets us the proability: (n - a) choose (b - a) / (n choose b)

        the calculations in get_prob are pre-computed by this formula to save
        computation time *)
    let get_prob out = 
      let single_prob = 
        match stage with
        | Deal -> 0.1 
        | Flop -> 2. /. 47.
        | Turn -> 1. /. 46.
        | _ -> failwith "this should be unreachable"
      in
      let double_prob = 
        match stage with
        | Deal -> 2. /. 245.
        | Flop -> 1. /. 1081.
        | Turn -> 0.
        | _ -> failwith "this should be unreachable"
      in 
      let tripple_prob = 
        match stage with
        | Deal -> 1. /. 1960.
        | Flop -> 0.
        | Turn -> 0.
        | _ -> failwith "this should be unreachable"
      in
      {hand_type = out.hand_type;prob = (single_prob+.double_prob+.tripple_prob)}
    in
    List.map get_prob outs_list

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