(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = s

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ ", ") t'
    in loop 0 "" lst
  in pp_elts lst

(* JUST PRINTER FUNCTIONS ABOVE ————————————————————————————————————————————— *)

(* [build_table] creates a list of players with a given stack size (100) for
   simplicity *)
let build_table (names : string list) (stack_size : int) =
  match State.init_state (Poker.create_players names 100) with
  | Legal t -> t
  | Illegal -> failwith "unable to initialize table"

(* [get_name] prompts the user to type a name for player [name_num] and
   returns the corresponding name *)
let get_name (name_num : int): string =
  print_string ("Please provide a name for Player " ^ string_of_int (name_num) ^ ": \n> ");
  read_line()

(* [get_names] prompts the user to input names for each player and returns
   a list of their names *)
let get_names (num_players) : string list =
  print_string ("You have chosen to play with " ^ string_of_int num_players ^ " player(s).\n");
  let rec add_name = function
    | 0 -> []
    | num -> get_name num :: add_name (num - 1)
  in
  add_name num_players

(* [print_active_players table] takes a state [table] and prints out those
   players which are still in the game, aka, those which have an active field
   equal to true *)
let print_active_players table : unit =
  let open Poker in
  let open State in
  let open List in
  let active_players = 
    table 
    |> get_players 
    |> filter (fun x -> is_active x = true) 
    |> map (fun x -> get_name x)
  in
  print_string ("\nACTIVE PLAYERS——— " ^ (pp_list pp_string active_players))

(* [print_big_blind table] prints the name of the player in state [table] who
   is the big blind *)
let print_big_blind table : unit =
  let open Poker in
  let open State in
  let open List in
  let big_blind_name = table |> get_big_blind |> get_name in
  print_string("\nBIG BLIND———————— " ^ (pp_string big_blind_name))

(* [print_pot table] prints the amount in the pot *)
let print_pot table : unit =
  let open Poker in
  let open State in
  let pot = table |> get_pot in
  print_string("\n | POT: " ^ (string_of_int pot) ^ "\n")

(* [print_community_cards table] prints the community cards for the game at
   state [table] *)
let print_community_cards table : unit =
  let open Poker in
  let open State in
  let open List in
  let community_cards = 
    table
    |> get_community_cards
    |> card_list_to_string in
  print_string "\n\nCOMMUNITY CARDS—— ";
  print_string (community_cards ^ "\n")

(* [print_hole_cards table] prints the hole cards for player 1, which we assume 
   here to be the user *)
let print_hole_cards table : unit =
  let open Poker in
  let open State in
  let open List in
  let hole_cards = 
    table
    |> get_players
    |> hd
    |> get_hole_cards
    |> card_list_to_string in
  print_string "\nYOUR CARDS——————— ";
  print_string (hole_cards ^ "\n\n")


(* [get_player_stacks players] returns a string representation of players' 
   current information.
   For example, information for three players, each with a stack of 150 would 
   be:
    [ | Player 1-150 | Player 2-150 | Player 3-150 | ]
*)
let rec get_player_stacks (players : Poker.player list) (cp_name: string) (bb_name : string) : string =
  let open Poker in
  match players with
  | [] -> " |"
  | h :: t ->
    (" | " ^ get_name h ^ "—" ^ (string_of_int (get_stack h))
     ^ (if get_name h = cp_name then " (Current Player)" else "")
     ^ (if get_name h = bb_name then " (Big Blind)" else ""))
    ^ get_player_stacks t cp_name bb_name


(* [print_player_stacks players] returns information about the players in the 
   game. Specifically it prints two lines, the first with name of players and 
   their stack amounts, the second with their role in the game.

   For example, information for three players, each with a stack of 150, player 
   1 has the big blind, player 3 is the current player would be:
   [ | Player1-150 (Big Blind) | Player2-150 | Player3-150 (Current Player) | ]
*)
let print_player_info (st : State.t) : unit =
  let open State in
  let open Poker in
  let players = get_players st in
  let cp_name = get_name ((current_player) st) in
  let bb_name = get_name ((get_big_blind) st) in
  print_string (get_player_stacks (players) (cp_name) (bb_name))


(* [get_action table] takes in the state and either returns the current state of
   the game [table] or a state with a given transition applied to it.

   For example, ["start"] will be parsed as a command to deal cards and return
   the state after cards are dealt.

   Requires: The user is player 1. *)
(* let get_action_deal table : State.t =
   let open Poker in
   let open State in
   let open Command in
   print_string  "> ";
   match parse(read_line ()) with
   | Start -> deal table
   | Hand -> table
   | Hole -> table
   | Table -> table
   | Call -> begin match call table (table |> get_players |> List.hd) with
      | Legal t -> t
      | Illegal -> table
    end
   | Fold -> fold table (table |> get_players |> List.hd)
   | Quit -> Stdlib.exit 0
   | Raise c -> begin match raise table (table |> get_players |> List.hd) c with
      | Legal t -> t
      | Illegal -> table
    end *)

(* let print_action table : unit =
   let updated_table = get_action_deal table in
   (* print_string "\nTABLE INFO ———————————————————————————————————\n"; *)
   print_state updated_table *)

(* [print_state table] prints information about the state [table], primarily
   - players (as well as their roles and stack amounts)
   - the pot
   - community cards (if any)
   - the user's hole cards (if any)
*)
let print_state (st : State.t) : unit =
  let open Poker in
  let open State in
  let open Command in
  print_string "\n";
  print_player_info (st);
  print_pot st;
  print_community_cards st;
  print_hole_cards st

let transition (st : State.t) (trans : State.t -> State.t) : State.t =
  trans st


let play_bots (st : State.t) : State.t =
  failwith "Unimplemented"


let play_round (st : State.t) (trans : State.t -> State.t) : State.t =
  let updated_state = transition st trans in
  updated_state

(* [play_command current_st str] prompts the user, some player in the current
   game [st] for a commmand, which is parsed to some transition
   function *)
let play_command (st : State.t) (str : string) : State.t =
  let open State in
  let open Command in
  let open Poker in
  match parse(str) with
  | Start -> deal st
  | Hand -> st (* Should show the best hand *)
  | Hole -> print_hole_cards st; st
  | Table -> print_community_cards st; st
  | Call -> begin match call st (st |> get_players |> List.hd) with
      | Legal new_st -> new_st
      | Illegal -> st
    end
  | Fold -> fold st (st |> get_players |> List.hd)
  | Raise c -> begin match raise st (st |> get_players |> List.hd) c with
      | Legal new_st -> new_st
      | Illegal -> st
    end
  | Quit -> Stdlib.exit 0

let rec prompt_user_command (st : State.t) : unit =
  print_string "Please input a command\n";
  print_string ("> ");
  let after_command = play_command st (read_line()) in
  if after_command = st then prompt_user_command st 
  else print_state after_command; print_string "\n"

let play_game (num_players : int) : unit =
  let open Poker in
  let open State in
  let open Command in
  let name_list = get_names num_players in
  let table = build_table name_list 100 in
  prompt_user_command table;
  prompt_user_command (deal table)
(* let deal_state = play_round table deal in
   print_state deal_state *)
(* let flop_state = play_round deal_state flop in
   print_state flop_state;
   let river_state = play_round flop_state river in
   print_state river_state *)


(* print_string ("\n\nYOUR OPTIONS \n");
   print_string ("start: put down flop card" ^ "\n" ^ "quit: end the game." ^ "\n\n");
   print_action_flop table *)

(* Not sure if the number prompted for will include the user themselves *)
(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to 3110 Poker.\n");
  print_endline "Please enter the number of players you would like at your table.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number_of_players -> play_game (int_of_string number_of_players)

(* Execute the game engine. *)
let () = main ()
