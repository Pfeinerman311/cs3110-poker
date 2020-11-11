type stage = Init | Deal | Flop | Turn | River

type t = {
  game_stage: stage;
  players: Poker.player list;
  big_blind : Poker.player;
  player_to_act: Poker.player;
  pot: int;
  community_cards: Poker.card list;
  call_cost: int;
  deck : Poker.card list;
}

type result = Legal of t | Illegal

let init_state players = 
  if 1< List.length players && List.length players < 10 then
    Legal {game_stage = Init; players = players; big_blind= List.hd players;
           player_to_act = List.hd (List.tl players); pot = 0;
           community_cards = []; call_cost=0; deck=[]}
  else Illegal

let get_stage state =
  state.game_stage

let current_player state = 
  state.player_to_act

let get_players state = 
  state.players

let get_big_blind state =
  state.big_blind

let get_community_cards state = 
  state.community_cards

let get_pot state = 
  state.pot

let get_call_cost state = 
  state.call_cost

let incr_stage state =
  let new_stage =
    match get_stage state with
    | Init -> Deal
    | Deal -> Flop
    | _ -> River
  in
  {state with game_stage = new_stage}

let raise state player amount = 
  if amount + state.call_cost > Poker.get_stack player || amount < 0 then Illegal
  else let new_players = List.map (fun x -> 
      if Poker.get_ID x = Poker.get_ID player then 
        Poker.alter_stack x (-(amount+state.call_cost))
      else x) state.players in 
    Legal {state with players =  new_players; 
                      pot = state.pot + state.call_cost + amount;
                      call_cost = state.call_cost + amount}

let call state player = 
  if state.call_cost > Poker.get_stack player then Illegal
  else let new_players = List.map (fun x -> 
      if Poker.get_ID x = Poker.get_ID player then 
        Poker.alter_stack x (-state.call_cost)
      else x) state.players in 
    Legal {state with players =  new_players; pot = state.pot + state.call_cost}

let fold state player = 
  let new_players = List.map (fun x -> 
      if Poker.get_ID x = Poker.get_ID player then Poker.set_inactive x
      else x) state.players in
  {state with players = new_players}

let rec first_n_helper list num stop acc = 
  match list with
  | [] -> acc,list
  | h::t -> if num = stop then acc,list 
    else first_n_helper t (num+1) stop (h::acc)


let first_n list n = 
  first_n_helper list 0 n []

let rec deal_helper players deck acc = 
  match players with
  | [] -> acc
  | h::t -> 
    let hole_cards , remaining_deck = first_n deck 2 in
    let new_player = Poker.set_hole_cards h hole_cards in
    deal_helper t (List.tl remaining_deck) (new_player::acc)

let deal state =
  let deck = Poker.get_shuffled_deck () in
  let rev_player = List.rev state.players in
  {state with players= deal_helper rev_player deck [] }


let flop state = 
  let flop_cards, remaining_deck = first_n state.deck 3 in
  {state with community_cards=flop_cards; deck = remaining_deck}

let turn state = 
  let turn_card, remaining_deck = first_n state.deck 1 in
  {state with community_cards= List.concat [turn_card;state.community_cards];
              deck = remaining_deck;}

let river state = 
  let river_card, remaining_deck = first_n state.deck 1 in
  {state with community_cards= List.concat [river_card;state.community_cards];
              deck = remaining_deck;}

let end_round state = 
  let best_hands = List.map (fun x -> 
      let avil_cards = 
        List.concat [Poker.get_hole_cards x;state.community_cards] in
      (x, (Poker.get_best_hand x avil_cards))) state.players in
  (** 
     let sorted_best_hands = List.sort (fun x y -> Poker.compare hand)
  *)
  failwith "unimplemented"