open OUnit2
open Poker
open Command
open State

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

let poker_tests = 
  [

  ]

let command_tests = 
  [

  ]

let player_names = ["Alice";"Bob"]
let start_stack = 1000
let players = create_players player_names start_stack
let state_tests = [
  pot_test "simple pot increase test with a player raising" players 300
]

let suite =
  "test suite for CS3110 Poker Project"  >::: List.flatten [
    poker_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
