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
  let open List in
  let pot = table |> get_pot in
  print_string("\nPOT—————————————— " ^ (string_of_int pot))

(* [print_state table] prints information about the state [table] *)
let print_state table : unit =
  let open Poker in
  let open State in
  let open Command in
  print_active_players table;
  print_big_blind table;
  print_pot table;
  print_string ("\nCURRENT PLAYER——— " ^ (table |> current_player |> get_name))


(* [get_action table] takes in the state and either returns the current state of
   the game [table] or a state with a given transition applied to it.

   For example, ["start"] will be parsed as a command to deal cards and return
   the state after cards are dealt.

   Requires: The user is player 1. *)
let get_action_deal table : State.t =
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
    end

let print_action table : unit =
  let updated_table = get_action_deal table in
  print_string "\n\nUPDATED TABLE:\n";
  print_state updated_table

(** [play_game f] starts the poker game by requesting a list of names. *)
let play_game (num_players : int) : unit =
  let open Poker in
  let open State in
  let open Command in
  let name_list = get_names num_players in
  let table = build_table name_list 100 in
  print_string "\nTABLE INFO ———————————————————————————————————\n";
  print_string ("\nPLAYERS—————————— " ^ (pp_list pp_string name_list));
  print_state table;
  print_string ("\n\nYOUR OPTIONS ———————————————————————————————————\n");
  print_string ("start: deal the cards" ^ "\n" ^ "quit: end the game." ^ "\n");
  print_action table

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
