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
  let rec add_names = function
    | 0 -> []
    | num -> get_name num :: add_names (num - 1)
  in
  List.rev (add_names num_players)

(* [print_stage st] prints the stage in which the state of the game [st] is *)
let print_stage (st : State.t) : unit =
  let open Poker in
  let open State in
  let label = "\nGame Stage: " in 
  match get_stage st with
  | Init -> print_string (label ^ "Initial\n")
  | Deal -> print_string (label ^ "Deal\n")
  | Flop -> print_string (label ^ "Flop\n")
  | Turn -> print_string (label ^ "Turn\n")
  | River -> print_string (label ^ "River\n")

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
(* let print_big_blind table : unit =
   let open Poker in
   let open State in
   let open List in
   let big_blind_name = table |> get_big_blind |> get_name in
   print_string("\nBIG BLIND———————— " ^ (pp_string big_blind_name)) *)

(* [print_pot table] prints the amount in the pot *)
let print_pot table : unit =
  let open Poker in
  let open State in
  let pot = table |> get_pot in
  print_string("\n | Pot: " ^ (string_of_int pot) ^ "\n")

(* [print_community_cards table] prints the community cards for the game at
   state [table] *)
let print_community_cards (st : State.t) : unit =
  let open Poker in
  let open State in
  let open List in
  let community_cards = 
    st
    |> get_community_cards
    |> card_list_to_string in
  print_string (" | Community cards: " ^ community_cards ^ "\n")

(* [print_hole_cards table] prints the hole cards for player 1, which we assume 
   here to be the user *)
let print_hole_cards st : unit =
  let open Poker in
  let open State in
  let open List in
  let hole_cards = 
    st
    |> get_players
    |> hd
    |> get_hole_cards
    |> card_list_to_string in
  print_string (" | Cards: " ^ hole_cards ^ "\n\n")


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
    (" | " ^ get_name h ^ " — " ^ (string_of_int (get_stack h))
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


(* [print_state st] prints information about the state [st], primarily
   - players (as well as their roles and stack amounts)
   - the pot
   - community cards (if any)
   - the user's hole cards (if any)
*)
let print_state (st : State.t) : unit =
  let open Poker in
  let open State in
  let open Command in
  print_stage st;
  print_player_info (st);
  print_pot st;
  print_community_cards st;
  print_hole_cards st

let transition (st : State.t) (trans : State.t -> State.t) : State.t =
  let open State in
  let new_stage = incr_stage st in
  trans new_stage

let play_bots (st : State.t) : State.t =
  failwith "Unimplemented"

let play_round (st : State.t) (trans : State.t -> State.t) : State.t =
  let updated_state = transition st trans in
  updated_state

(* [play_command st cmd] takes in a commmand [cmd] from the user and uses it to
   pattern match it to a new state which is just a transition function applied
   to the input state [st] *)
let rec play_command (st : State.t) (cmd : Command.command) : State.t =
  let open State in
  let open Command in
  let open Poker in
  let to_next_stage =
    match get_stage st with
    | Init -> deal
    | Deal -> flop
    | Flop -> turn
    | _ -> river
  in
  match cmd with
  | Start -> play_round st to_next_stage
  | Hand -> 
    if (get_stage st = Init) 
    then (print_string "You have not been dealt cards yet. Please pick a different option. \n"; st)
    else (print_string "Your best hand is: ______\n"; st) (* Should show the user's best hand *)
  | Hole -> 
    if (get_stage st = Init) 
    then (print_string "You have not been dealt cards yet. Please pick a different option. \n"; st) 
    else (print_hole_cards st; st)
  | Table -> 
    if (get_stage st = Init) 
    then (print_string "You have not been dealt cards yet. Please pick a different option. \n"; st) 
    else (print_community_cards st; print_string "\n"; st)
  | Call -> 
    if (get_stage st = Init) 
    then (print_string "You have not been dealt cards yet. Please pick a different option. \n"; st) 
    else 
      begin match call st (st |> get_players |> List.hd) with
        | Legal new_st -> print_string "You have chosen to call.\n"; new_st
        | Illegal -> print_string "You are unable to call.\n"; st
      end
  | Fold -> print_string "You have chosen to fold\n"; fold st (st |> get_players |> List.hd)
  | Raise c -> 
    if (get_stage st = Init) 
    then (print_string "You have not been dealt cards yet. Please pick a different option. \n"; st) 
    else 
      begin match raise st (st |> get_players |> List.hd) c with
        | Legal new_st -> print_string ("You have chosen to raise" ^ string_of_int c ^ ".\n"); new_st
        | Illegal -> print_string "You are unable to raise thsi amount.\n"; st
      end
  | Quit -> print_string "\n\nThanks for playing!\n\n"; Stdlib.exit 0


let rec prompt_user_command (st : State.t) : State.t =
  let open Command in
  print_string "Please input a command\n";
  print_string "  ——————————————————————————————————————————————————————————\n";
  print_string " | start | hand | hole | table | call | fold | raise | quit |\n" ;
  print_string "  ——————————————————————————————————————————————————————————\n";
  print_string ("> ");
  let input = read_line() in
  match parse input with
  | exception Malformed -> print_string "This command is not appropriate, please enter one of the commands above.\n"; prompt_user_command st
  | cmd -> 
    begin match play_command st cmd with
      | same_st when same_st = st -> prompt_user_command same_st
      | new_st -> print_state new_st; new_st
    end


(* let rec game_flow (st : State.t) : State.t =
   let open State in
   if get_stage st = River then (print_string "Good job! Game over."; st)
   else (prompt_user_command st; game_flow (st)) *)



let play_game (num_players : int) : unit =
  let open Poker in
  let open State in
  let open Command in
  let name_list = get_names num_players in
  let init_st = build_table name_list 100 in
  let deal_st = prompt_user_command init_st in
  let flop_st = prompt_user_command deal_st in
  let turn_st = prompt_user_command flop_st in
  let river_st = prompt_user_command turn_st in
  print_string "Game over, nice!";
  print_state river_st
(* let deal_state = play_round table deal in
   print_state deal_state *)
(* let flop_state = play_round deal_state flop in
   print_state flop_state;
   let river_state = play_round flop_state river in
   print_state river_state *)


(* [try_game input] is simply a way of constraining the user's input when 
   prompted for the number of players they would like to play with. *)
let rec try_game (input : string) =
  match int_of_string input with
  | exception (Failure s) -> print_string ("'" ^ input ^ "'" ^ " is not a valid number. Please enter an integer from 2-10\n"); print_string "> "; try_game (read_line())
  | x when (x > 10 || x < 2) -> print_string ("'" ^ input ^ "'" ^ " is not a valid number. Please enter an integer from 2-10\n"); print_string "> "; try_game (read_line())
  | x -> play_game x

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to 3110 Poker.\n");
  print_endline "Please enter the number of players you would like at your table.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | num_players -> try_game num_players

(* Execute the game engine. *)
let () = main ()
