open Poker

type t = {
  players: Poker.player list;
  big_blind : Poker.player;
  player_to_act: Poker.player;
  pot: int;
  community_cards: Poker.card list;
  call_cost: int;
}

type result = Legal of t | Illegal

let init_state players = 
  if 1< List.length players && List.length players < 10 then
    Legal {players = players; big_blind= List.hd players;
           player_to_act = List.hd (List.tl players); pot = 0;
           community_cards = []; call_cost=0}
  else Illegal

let current_player state = 
  state.player_to_act

let get_players state = 
  state.players

let get_community_cards state = 
  state.community_cards

let get_pot state = 
  state.pot

let get_call_cost state = 
  state.call_cost

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
  failwith "Unimplemented"

let deal state =
  failwith "Unimplemented"

let flop state = 
  failwith "Unimplemented"

let turn state = 
  failwith "Unimplemented"

let river state = 
  failwith "Unimplemented"

let end_round state = 
  failwith "Unimplemented"