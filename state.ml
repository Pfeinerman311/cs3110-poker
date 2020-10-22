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
  failwith "Unimplemented"

let get_players state = 
  failwith "Unimplemented"

let get_community_cards state = 
  failwith "Unimplemented"

let get_pot state = 
  state.pot

let get_call_cost state = 
  failwith "Unimplemented"

let raise state player amount = 
  if amount > Poker.get_stack player then Illegal
  else let new_players = List.map (fun x -> 
      if Poker.get_ID x = Poker.get_ID player then Poker.alter_stack x (-amount)
      else x) state.players in 
    Legal {state with players =  new_players; pot = state.pot + amount}

let call state player = 
  failwith "Unimplemented"

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