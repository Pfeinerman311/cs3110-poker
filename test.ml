open OUnit2
open Poker
open Command
open State
open Bot

(** These helper functions are from the A2 release *)
(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [make_cmp_tests] creates test [name] and checks whether [input] and the 
    [expected_output] are equivalent set-like lists. *)
let make_cmp_tests
    (name : string)
    (input : _ list)
    (expected_output : _ list) : test =
  name >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        input expected_output)

(********************************************************************
   End helper functions.
 ********************************************************************)

let pot_test  
    (name: string)   
    (players: player list)
    (expected: int) : test = 
  let state = match (init_state players) with
    | Illegal -> failwith("Illegal Raise should be legal")
    | Legal t -> t
  in
  let mid = match raise state (List.hd players) 50 with
    | Illegal -> failwith("Illegal Raise should be legal")
    | Legal t -> t
  in
  let final = match raise mid (List.hd (List.tl players) ) 250 with
    | Illegal -> failwith("Illegal Raise should be legal")
    | Legal t -> t
  in
  name >:: (fun _ ->
      assert_equal expected (get_pot final)
        ~printer:string_of_int)

let rec players_have_2_cards players acc = 
  match players with 
  | [] -> acc
  | h::t -> if List.length (get_hole_cards h) = 2 then 
      players_have_2_cards t (acc && true)
    else
      players_have_2_cards t (acc && false)

let deal_test
    (name: string)   
    (players: player list) = 
  let state = match (init_state players) with
    | Illegal -> failwith("Illegal Raise should be legal")
    | Legal t -> t
  in
  let dealt_state = State.deal state in
  let dealt_players = State.get_players dealt_state in
  name >:: (fun _ ->
      assert_bool "not all players had 2 cards" 
        (players_have_2_cards dealt_players true)
    )

let community_test
    (name : string)
    (players: player list)
    (expected : int) = 
  let state = match (init_state players) with
    | Illegal -> failwith("Illegal intial state")
    | Legal t -> t
  in
  let dealt_state = State.deal state |> State.flop in
  let community_cards = State.get_community_cards dealt_state in
  name >:: (fun _ ->
      assert_equal expected (List.length community_cards)
        ~printer:string_of_int)

let c2 = (Two, Spades)
let c1 = [(Two, Spades); (Five, Clubs); (Ace, Clubs); (Seven, Diamonds);
          (Jack, Diamonds); (Four, Spades); (Ace, Diamonds)]

let poker_tests = 
  [

  ]

let command_tests = 
  [

  ]

let player_names = ["Alice";"Bob"]
let start_stack = 1000
let extended_player_names = ["Alice";"Bob";"Alice";"Bob";
                             "Alice";"Bob";"Alice";"Bob"]
let players = create_players player_names start_stack
let extended_players = create_players player_names start_stack
let state_tests = [
  pot_test "simple pot increase test with a player raising" players 350;
  deal_test "check 2 players are dealt cards" players;
  deal_test "Check 8 players are dealt cards" extended_players;
  community_test "check that community has 3 cards post flop" players 3;
]

module FoldBotInfo = struct
  let diff = Fold
  let seed = 0
end

module MyFoldBot = FoldBot.Make(FoldBotInfo)
let state = match (init_state players) with
  | Illegal -> failwith("Illegal Raise should be legal")
  | Legal t -> t 

let test_foldbot  
    (name: string)   
    (bot_command : command ) : test = 
  name >:: (fun _ ->
      assert_equal bot_command Fold
    )

let bot_tests = [
  test_foldbot "Test fold bot folds" (MyFoldBot.get_action state)
]

let suite =
  "test suite for CS3110 Poker Project"  >::: List.flatten [
    poker_tests;
    command_tests;
    state_tests;
    bot_tests;
  ]

let _ = run_test_tt_main suite