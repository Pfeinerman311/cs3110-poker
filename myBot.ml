open Bot
open Poker

module Make = functor (I : BotInfo) -> struct

  module Info = I

  type outs = {
    hand_type : hand_tp;
    single : float;
    double : float;
    tripple : float;
    quad : float
  }

  type out_prob = {
    hand_type : hand_tp;
    prob: float
  }

  (** Calculates ([n] choose [k]) *)
  let choose n k = 
    let rec helper i acc = 
      if i = k then acc
      else helper (i +. 1.) (acc *.  ( n +. 1. -. i ) /. i) 
    in 
    helper 1.0 1.0

  let inc_hand h = 
    match h with 
    | Royal_Flush-> failwith "inc_hand should not be called on royal flush"
    | Straight_Flush -> Royal_Flush
    | Four_Kind -> Straight_Flush
    | Full_House -> Four_Kind
    | Flush -> Full_House
    | Straight -> Flush
    | Three_Kind -> Straight
    | Two_Pair -> Three_Kind
    | Pair -> Two_Pair
    | High_Card -> Pair

  let highcard_helper (cards : card list) = 
    let high_card = List.hd cards in
    let rank = Poker.rank_to_int (fst high_card) in 
    let single_outs = 14.0 -. float_of_int (rank) in 
    {hand_type=High_Card;
     single=single_outs;
     double=0.0;
     tripple=0.0;
     quad=0.0}

  let rec generate_outs_list_helper curr hand cards acc stage = 
    match curr with 
    | Royal_Flush -> failwith ""
    | Straight_Flush -> failwith ""
    | Four_Kind -> failwith ""
    | Full_House -> failwith ""
    | Flush -> failwith ""
    | Straight -> failwith ""
    | Three_Kind -> failwith ""
    | Two_Pair -> failwith ""
    | Pair -> failwith ""
    | High_Card -> 
      generate_outs_list_helper (inc_hand curr) hand cards acc stage

  let generate_outs_list start hand cards stage = 
    generate_outs_list_helper start cards stage []

  let get_cards_that_improve_hand player com_cards stage = 
    let best_hand = Poker.get_best_hand player com_cards in 
    generate_outs_list best_hand.tp best_hand.cards 
      (List.concat [player.hole_cards;com_cards] |> 
       List.sort compare |> List.rev )
      stage

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
      let quad_prob = 0.0
      in
      {hand_type = out.hand_type;
       prob = (single_prob+.double_prob+.tripple_prob+.quad_prob)}
    in
    List.map get_prob outs_list

  let calculate_probabilities_of_hands_winning hands state = 
    failwith "Unimplemented"

  let get_prob_of_winning hand_probs probs_hands_win = 
    failwith "Unimplemented"

  let formulate_bet prob = 
    failwith "Unimplemented"

  let get_action s p = 
    (** let stage = State.get_stage s in
        let hand = get_hole_cards in *)
    (Fold : Command.command)

end 