open Bot
open State
open Poker
open List
open Command

(* This is a list of predetermined names for input into build_table *)
let table_names = 
  ["Cesar"; 
   "Dean"; 
   "Parker"; 
   "Clarkson";
   "Gries";
   "White";
   "Fan";
   "Foster";
   "Hopcroft"
  ]

(* [line_div n] is a function for creating a dividing line that spans a given 
   number of spaces *)
let line_div (length : int) : unit =
  for i = 0 to length - 1 do
    print_string "—"
  done;
  print_string "\n"

module TestBotInfo = struct
  let diff = Test
  let seed = 0
end

module MyTestBot = TestBot.Make(TestBotInfo)

(* [build_table] creates a list of players with a given stack size (100) for
   simplicity *)
let build_table (names : string list) (stack_size : int) =
  match init_state (create_players (names) 100) 0 with
  | Legal t -> t
  | Illegal -> failwith "unable to initialize table"

(* [get_name] prompts the user to type a name for player [name_num] and
   returns the corresponding name *)
let get_name (name_num : int): string =
  let player_num = string_of_int name_num in
  ANSITerminal.(print_string [green] ("\n Please provide a name for Player " ^ player_num ^ ": \n"));
  print_string " > ";
  read_line()

(* [get_names] prompts the user to input names for each player and returns
   a list of their names *)
let get_names (num_players) : string list =
  ANSITerminal.(print_string [green] (" \n You have chosen to play with " ^ string_of_int num_players ^ " player(s).\n"));
  let username = print_string(" What's your name?\n\n > "); read_line() in
  let rec add_names = function
    | 1 -> []
    | num -> get_name num :: add_names (num - 1)
  in
  username :: add_names num_players

let name_list_generic (num_players : int) : string list =
  let username = print_string(" What's your name?\n\n > "); read_line() in
  let tbl_names = map (fun x -> if (x = username) then "Mimno" else x) table_names in
  let gen_names = fst (first_n tbl_names (num_players - 1)) in
  rev (username :: gen_names)


(* [print_stage st] prints the stage in which the state of the game [st] is *)
let print_stage (st : State.t) : unit =
  ANSITerminal.(print_string [green;Bold] "\n Game Stage: ");
  match get_stage st with
  | Init -> print_string ("Initial\n");
  | Deal -> ANSITerminal.(print_string [cyan] ("Deal\n"));
  | Flop -> ANSITerminal.(print_string [magenta] ("Flop\n"));
  | Turn -> ANSITerminal.(print_string [yellow] ("Turn\n"));
  | River -> ANSITerminal.(print_string [green] ("River\n"))

(* [get_active_players table] takes in a state [st] and returns out those
   players which are still in the game, aka, those which have an active field
   equal to true *)
let get_active_players (st : State.t) : Poker.player list =
  st 
  |> get_players 
  |> filter (fun x -> is_active x = true)

(* [print_pot table] prints the amount in the pot *)
let print_pot table : unit =
  let pot = table |> get_pot in
  print_string("\n | Pot: " ^ (string_of_int pot) ^ "\n")

(* [print_community_cards table] prints the community cards for the game at
   state [table] *)
let print_community_cards (st : State.t) (color_print : bool) : unit =
  let community_cards = 
    st
    |> get_community_cards
    |> card_list_to_string in
  if color_print then
    ANSITerminal.(print_string [blue] ("\n The cards on the table are: " ^ community_cards))
  else
    print_string (" | Community cards: " ^ community_cards ^ "\n")

(* [print_hole_cards table] prints the hole cards for player 1, which we assume 
   here to be the user *)
let print_hole_cards (st : State.t) (color_print : bool) : unit =
  let hole_cards = 
    st
    |> get_players
    |> hd
    |> get_hole_cards
    |> card_list_to_string in
  if color_print then
    ANSITerminal.(print_string [blue] ("Your hole cards are: " ^ hole_cards ^ "\n\n"))
  else
    print_string (" | Cards: " ^ hole_cards ^ "\n\n")


(* [get_player_stacks players] returns a string representation of players' 
   current information.
   For example, information for three players, each with a stack of 150 would 
   be:
    [ | Player 1-150 | Player 2-150 | Player 3-150 | ] *)
let rec get_player_stacks (players : Poker.player list) (cp_name: string) (bb_name : string) : string =
  match players with
  | [] -> " |"
  | h :: t ->
    (" | " ^ Poker.get_name h ^ " — " ^ (string_of_int (get_stack h)) ^ " ID: " ^ (h |> get_ID |> string_of_int)
     (* ^ (if get_name h = cp_name then " (Current Player)" else "") *)
     ^ (if Poker.get_name h = bb_name then " (Big Blind)" else ""))
    ^ get_player_stacks t cp_name bb_name


(* [print_player_info players] returns information about the players in the 
   game. Specifically it prints a line with name of the active players and 
   their stack amounts.

   For example, information for three players, each with a stack of 150, player 
   1 has the big blind would be:
   [ | Player1-150 (Big Blind) | Player2-50 | Player3-50 | ] *)
let print_player_info (st : State.t) : unit =
  let players = get_active_players st in
  let cp_name = Poker.get_name ((current_player) st) in
  let bb_name = Poker.get_name ((get_big_blind) st) in
  print_string (get_player_stacks (players) (cp_name) (bb_name))


(* [print_state st] prints information about the state [st], primarily
   - players (as well as their roles and stack amounts)
   - the pot
   - community cards (if any)
   - the user's hole cards (if any)
*)
let print_state (st : State.t) : unit =
  print_stage st;
  print_player_info (st);
  print_pot st;
  print_community_cards st false;
  print_hole_cards st false

let transition (st : State.t) (trans : State.t -> State.t) : State.t =
  let new_stage = incr_stage st in
  trans new_stage

(* [play_bot_acton st] takes in the command for a bot represented by player [p]
   and returns [st], the state after the command is performed on the game. *)
let play_bot_action
    (st : State.t) 
    (p : Poker.player)
  : State.t =
  print_string (string_of_int (st |> current_player |> get_ID));
  print_string ( (st |> current_player |> Poker.get_name));
  match MyTestBot.get_action st p with
  | Call ->
    begin match State.call st p with
      | Legal new_st -> ANSITerminal.(print_string [green] ("\n\n " ^ (Poker.get_name p) ^ " has chosen to call\n")) ; new_st
      | Illegal -> failwith "Bot cannot call"
    end
  | Fold -> begin match State.call st p with 
      | Legal new_st -> ANSITerminal.(print_string [green] ("\n\n " ^ (Poker.get_name p) ^ " has chosen to fold\n")) ; new_st
      | Illegal -> failwith "Bot cannot call"
    end
  | _ -> failwith "unimplemented"

(* [play_bots st] plays the commands for the bots in a round where only the
   user (aka player 1) has had their action processed. 

   The idea here is to use fold_left on the list of players besides the user.
   As such, when replaced with the types being used here, the type of fold_left
   would be:
    (state -> player -> state) -> player list -> state

   Therefore, the state of the game [st] must be such that the current player is 
   the second player. This function returns a game state in which the last n - 1 
   players in an n-player table have made a decision.
*)
let play_bots (st : State.t) : State.t =
  print_player_info st;
  st
  |> get_active_players
  |> tl
  |> fold_left play_bot_action st

let play_round (st : State.t) (trans : State.t -> State.t) : State.t =
  let after_bots = if (State.get_stage st = Init) then st else play_bots st in
  let after_trans = transition after_bots trans in
  after_trans

(* [finish_game st] is a function that prints the necessary things once a game
   in state [st] is recognized to be over (aka there is only player with money
   left). *)
let finish_game (st : State.t) : unit =
  let last_man_standing = st |> get_active_players |> List.hd |> Poker.get_name in
  print_state st;
  ANSITerminal.(print_string [green] (" Congratulations " ^ last_man_standing ^ "! \n\n You've reached the end of the game. \n\n"))

let opt_to_keyword (input : string) : string =
  match input with
  | "ante" -> "go"
  | "check" -> "call"
  | _ -> input

let opt_descriptions (opt : string) : string =
  match opt with
  | "ante" -> " pay blind and deal cards."
  | "check" -> "bet nothing."
  | "call" -> " match the current bet or raise."
  | "fold" -> " forfeit your hand."
  | "raise" -> "bet a number on your hand."
  | "leave" -> "leave the table and quit the game."
  | "hand" -> " see your best current hand."
  | _ -> ""

let print_opts (opts : string list) : unit =
  print_string "  ——————————————————————————————————————————\n";
  for i = 0 to (length opts) - 1 do
    print_string (" | ");
    ANSITerminal.(print_string [Bold] (nth opts i) );
    print_string (": " ^ opt_descriptions (nth opts i) ^ "\n");
  done;
  print_string "  ——————————————————————————————————————————\n";
  print_string "\n"

let get_opts (st : State.t) : string list =
  match get_stage st with
  | Init -> ["ante"; "leave"]
  | Deal -> ["check"; "call"; "fold"; "raise"; "leave"]
  | Flop -> ["hand"; "check"; "call"; "fold"; "raise"; "leave"]
  | Turn -> ["hand"; "check"; "call"; "fold"; "raise"; "leave"]
  | River -> ["hand"; "check"; "call"; "fold"; "raise"; "leave"]

let print_malformed (st : State.t) : unit =
  let 
    msg = "\n This command is not appropriate, please enter one of the commands" 
          ^ " above or type help.\n"
  in
  ANSITerminal.(print_string [red] msg)

(* [play_command st cmd] takes in a commmand [cmd] from the user and uses it to
   pattern match it to a new state which is just a transition function applied
   to the input state [st] *)
let rec play_command (st : State.t) (cmd : Command.command) : State.t =
  let to_next_stage =
    match get_stage st with
    | Init -> deal
    | Deal -> flop
    | Flop -> turn
    | Turn -> river
    | River -> deal
  in
  let user = st |> get_players |> get_next_player (current_player st) in
  match cmd with
  | Start -> play_round st to_next_stage
  | Hand -> 
    if (get_stage st = Init || get_stage st = Deal) 
    then (ANSITerminal.(print_string [red] "\n You have not been dealt any cards yet. Please pick a different option. \n\n"); st)
    else (ANSITerminal.(print_string [green] ("\n Your best hand is: " ^ hand_to_string (get_best_hand user (get_community_cards st)) ^ "\n\n")); st)
  | Call -> 
    if (get_stage st = Init) 
    then (ANSITerminal.(print_string [red] "\n You have not been dealt any cards yet. Please pick a different option. \n\n"); st) 
    else
      begin match call st user with
        | Legal new_st -> ANSITerminal.(print_string [green] "\n You have chosen to call.\n\n"); play_round new_st to_next_stage
        | Illegal -> ANSITerminal.(print_string [red] " \nYou are unable to call.\n\n"); st
      end
  | Fold -> print_string " You have chosen to fold\n\n"; play_round (fold st user) to_next_stage
  | Raise c -> 
    if (get_stage st = Init) 
    then (ANSITerminal.(print_string [red] " \nYou have not been dealt any cards yet. Please pick a different option. \n\n"); st) 
    else 
      begin match raise st user c with
        | Legal new_st -> print_string (" You have chosen to raise " ^ string_of_int c ^ ".\n"); play_round new_st to_next_stage
        | Illegal -> ANSITerminal.(print_string [red] " \nYou are unable to raise this amount.\n\n"); st
      end
  | Help -> print_opts (get_opts st); st
  | Quit -> ANSITerminal.(print_string [green] "\n\n Thanks for playing!\n\n"); Stdlib.exit 0

let rec prompt_user_command (st : State.t) : State.t =
  if (st |> get_active_players |> List.filter (fun x -> get_ID x = 0) |> List.length = 0)
  then play_command st Start 
  else(
    let msg = 
      " It's your turn. Please input a command, "
      ^ {|or type "help" for a list of possible commands.|}
      ^ "\n "
    in
    ANSITerminal.(print_string [green] msg);
    line_div 87;
    print_string (" > ");
    let input = read_line() in
    match parse (opt_to_keyword input) with
    | exception Malformed -> print_malformed st; prompt_user_command st
    | cmd -> 
      begin match play_command st cmd with
        | same_st when same_st = st -> prompt_user_command same_st
        | new_st -> print_state new_st; new_st
      end
  )

let rec print_winners (winners : (Poker.player * Poker.hand) list) : unit =
  match winners with
  | [] -> ()
  | (player, hand) :: t -> 
    let name = Poker.get_name player in
    let hole = card_list_to_string (get_hole_cards player) in
    let hand = hand_to_string hand in
    ANSITerminal.(print_string [Bold; blue] ("\n - " ^ name ^ " with hand: " ^hand ^ "\n   and hole: " ^ hole ^ " \n")); print_winners t

let rec game_flow (st : State.t) : unit =
  let init_st = st in
  let deal_st = prompt_user_command (pay_big_blind init_st) in
  let flop_st = prompt_user_command deal_st in
  let turn_st = prompt_user_command flop_st in
  let river_st = prompt_user_command turn_st in
  let end_round = prompt_user_command river_st in
  let after_subgame_st = (end_subgame end_round) in
  ANSITerminal.(print_string [Bold; blue] " WINNER(S): ");
  print_winners ((get_winners river_st));
  print_string ("\n");
  if List.length (get_active_players after_subgame_st) < 2 
  then finish_game after_subgame_st 
  else game_flow (incr_subgame after_subgame_st)

let play_game (num_players : int) : unit =
  let name_list = name_list_generic num_players in
  let init_st = build_table name_list 100 in
  (* print_string (List.nth (init_st |> get_players |> List.map (fun x -> Poker.get_name x)) 0); *)
  print_state init_st;
  game_flow init_st

(* [try_game input] is simply a way of constraining the user's input when 
   prompted for the number of players they would like to play with. *)
let rec try_game (input : string) =
  match int_of_string input with
  | exception (Failure s) -> print_string ("'" ^ input ^ "'" ^ " is not a valid number. Please enter an integer from 2-9\n"); print_string " > "; try_game (read_line())
  | x when (x > 9 || x < 2) -> print_string ("'" ^ input ^ "'" ^ " is not a valid number. Please enter an integer from 2-9\n"); print_string " > "; try_game (read_line())
  | x -> play_game x

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [green]
                  "\n\n Welcome to 3110 Poker.\n");
  print_endline " Please enter a number of players [2 - 9] that you would like at your poker table.\n";
  print_string  " > ";
  match read_line () with
  | exception End_of_file -> ()
  | num_players -> try_game num_players

(* Execute the game engine. *)
let () = main ()
