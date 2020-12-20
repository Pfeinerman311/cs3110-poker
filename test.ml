open OUnit2
open Poker
open Command
open State
open Bot

(** TESTING METHODOLOGY *)




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
  let uniq1 = List.sort_uniq Stdlib.compare lst1 in
  let uniq2 = List.sort_uniq Stdlib.compare lst2 in
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

(* Below are functions used for testing. Primarily, they are ways of checking
   what each player has at each point of the game. *)

(* [get_all_hole_cards st] displays all of the hole cards around the table
   at a given state [st] *)
let get_all_hole_cards (st : State.t) : string list =
  st
  |> State.get_active_players
  |> List.map get_hole_cards
  |> List.map card_list_to_string


(* let 2ptest_st = State. *)




(* [deck_tracker st] returns the first five elements of the deck throughout
   an entire round by creating a State.t object and transitioning through
   a round. *)
let deck_tracker (num_players : int) : string list =
  let open State in
  let open Poker in
  let name_list = ["p1";"p2";"p3";"p4";"p5";"p6";"p7";"p8";"p9"] in
  let pn = "p" ^ (string_of_int num_players) in
  let truncated_lst = List.filter (fun p -> p <= pn) name_list in
  let tst_tbl =
    match State.init_state (create_players truncated_lst 100) 50 with
    | Legal st -> st
    | Illegal -> failwith "Illegal table"
  in
  (* let trans_list = [deal; flop; turn; river] in *)
  (* List.fold_left
     (fun trans -> card_list_to_string (State.get_deck (trans tst_tbl) )) 
     (card_list_to_string State.get_deck tst_tbl  :: []) 
     trans_list *)
  let init_deck = card_list_to_string (get_deck tst_tbl) in
  let deal_tbl = deal tst_tbl in
  let deal_deck = card_list_to_string (get_deck deal_tbl) in
  let flop_tbl = flop deal_tbl in
  let flop_deck = card_list_to_string (get_deck flop_tbl) in
  let turn_tbl = turn flop_tbl in
  let turn_deck = card_list_to_string (get_deck turn_tbl) in
  let river_tbl = river turn_tbl in
  let river_deck = card_list_to_string (get_deck river_tbl) in
  river_deck :: turn_deck :: flop_deck :: deal_deck :: init_deck :: []

let pot_test  
    (name: string)   
    (players: player list)
    (expected: int) : test = 
  let state = match (init_state players) 0 with
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
  let state = match (init_state players) 0 with
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
  let state = match (init_state players 0) with
    | Illegal -> failwith("Illegal intial state")
    | Legal t -> t
  in
  let dealt_state = State.deal state |> State.flop in
  let community_cards = State.get_community_cards dealt_state in
  name >:: (fun _ ->
      assert_equal expected (List.length community_cards)
        ~printer:string_of_int)

let hand_comparer h1 h2 =
  if hand_compare h1 h2 = 0 then true else false

let best_hand_test
    (name : string)
    (player : player)
    (community_cards : card list)
    (expected_output : hand) : test =
  name >:: (fun _ ->
      assert_equal ~cmp:hand_comparer ~printer:(hand_to_string)
        expected_output (get_best_hand player community_cards)) 

let c1 = [(Three, Clubs); (Ace, Clubs); (Seven, Diamonds); (Six, Clubs);
          (Four, Diamonds)]
let c2 = [(Two, Spades); (Five, Clubs); (Ace, Clubs); (Two, Diamonds);
          (Jack, Diamonds); (Four, Spades); (Ace, Diamonds)]
let c3 = [(Three, Clubs); (Ace, Clubs); (Seven, Diamonds); (Six, Clubs);
          (Four, Clubs)]
let c4 = [(Three, Clubs); (Ace, Spades); (Jack, Spades); (Nine, Spades);
          (King, Spades)]

let hl_0 = [(Nine, Clubs); (King, Hearts)]
let hl_1 = [(Two, Spades); (Four, Clubs)]
let hl_2 = [(Three, Spades); (Four, Clubs)]
let hl_3 = [(Seven, Hearts); (Seven, Clubs)]
let hl_4 = [(Two, Spades); (Five, Diamonds)]
let hl_5 = [(Jack, Clubs); (Seven, Clubs)]
let hl_6 = [(Two, Hearts); (Ace, Hearts)]
let hl_7 = [(Ace, Spades); (Ace, Hearts)]
let hl_8 = [(Two, Clubs); (Five, Clubs)]
let hl_9 = [(Queen, Spades); (Ten, Spades)]

let h0 = {tp = High_Card; cards = [(King, Hearts)]}
let h1 = {tp = Pair; cards = [(Four, Clubs); (Four, Diamonds)]}
let h2 = {tp = Two_Pair; cards = [(Three, Clubs); (Three, Spades); 
                                  (Four, Clubs); (Four, Diamonds)]}
let h3 = {tp = Three_Kind; cards = [(Seven, Clubs); (Seven, Diamonds); 
                                    (Seven, Hearts)]}
let h4 = {tp = Straight; cards = [(Three, Clubs); 
                                  (Four, Diamonds); (Five, Diamonds); 
                                  (Six, Clubs); (Seven, Diamonds)]}
let h5 = {tp = Flush; cards = [(Three, Clubs); (Six, Clubs);
                               (Seven, Clubs); (Jack, Clubs); (Ace, Clubs)]}
let h6 = {tp = Full_House; cards = [(Two, Diamonds); (Two, Hearts);
                                    (Two, Spades); (Ace, Diamonds); 
                                    (Ace, Hearts)]}
let h7 = {tp = Four_Kind; cards = [(Ace, Clubs); (Ace, Diamonds);
                                   (Ace, Hearts); (Ace, Spades)]}
let h8 = {tp = Straight_Flush; cards = [(Two, Clubs); (Three, Clubs); 
                                        (Four, Clubs); (Five, Clubs); 
                                        (Six, Clubs);]}
let h9 = {tp = Royal_Flush; cards = [(Ten, Spades); (Jack, Spades); 
                                     (Queen, Spades); (King, Spades); 
                                     (Ace, Spades)]}

let make_player cards =
  {name = "Parker";
   id = 2;
   active = true;
   stack = 0;
   hole_cards = cards}


let poker_tests = 
  [
    best_hand_test "Parker highcard test" (make_player hl_0) c1 h0; 
    best_hand_test "Parker pair test" (make_player hl_1) c1 h1;
    best_hand_test "Parker two pair test" (make_player hl_2) c1 h2;
    best_hand_test "Parker three of a kind test" (make_player hl_3) c1 h3;
    best_hand_test "Parker straight test" (make_player hl_4) c1 h4;
    best_hand_test "Parker flush test" (make_player hl_5) c1 h5;
    best_hand_test "Parker full house test" (make_player hl_6) c2 h6;
    best_hand_test "Parker four of a kind test" (make_player hl_7) c2 h7;
    best_hand_test "Parker straight flush test" (make_player hl_8) c3 h8;
    best_hand_test "Parker royal flush test" (make_player hl_9) c4 h9;
  ]

let command_tests = 
  [

  ]

let community_cards_string (st : State.t) : string =
  st |> State.get_community_cards |> Poker.card_list_to_string


let player_names = ["Alice";"Bob"]
let start_stack = 1000
let extended_player_names = ["Alice";"Bob";"Alice";"Bob";
                             "Alice";"Bob";"Alice";"Bob"]
let players = create_players player_names start_stack
let extended_players = create_players player_names start_stack

let alice = {name = "Alice"; id = 1; active = true; stack = 100; 
             hole_cards = [(Ace, Clubs); (Ace, Diamonds);]}
let bob = {name = "Bob"; id = 2; active = true; stack = 100; 
           hole_cards = [(Queen, Clubs); (Queen, Diamonds)]}

let pre_end_state = 
  let init =  match init_state [alice;bob] 0 with
    | Illegal -> failwith("Illegal init should be legal")
    | Legal t -> t
  in 
  let mid = match raise init bob 50 with
    | Illegal -> failwith("Illegal Raise should be legal")
    | Legal t -> t
  in fold mid bob

let test_state_end_subgame 
    (name : string)
    (pre_end_state : State.t)
    (expected_winner : Poker.player)
    (expected_winner_stack : int) : test = 
  let ended_subgame = end_subgame pre_end_state in
  let matched_ids = List.filter 
      (fun x -> Poker.get_ID x = Poker.get_ID expected_winner) 
      (State.get_players ended_subgame)
  in
  let actual_stack = Poker.get_stack (List.hd matched_ids) in
  name >:: (fun _ ->
      assert_equal ~printer:string_of_int
        expected_winner_stack actual_stack) 


let rec first_n_helper list counter stop acc = 
  match list with
  | [] -> acc,list
  | h::t -> if counter = stop then acc,list 
    else first_n_helper t (counter+1) stop (acc@[h])


let first_n list n = 
  first_n_helper list 0 n []

let test_first_n 
    (name : string)
    (list)
    (n : int): test = 
  let first,last = first_n list n in
  name >:: (fun _ ->
      assert_equal ~printer:string_of_int
        n (List.length first)) 

let state_tests = [
  pot_test "simple pot increase test with a player raising" players 350;
  deal_test "check 2 players are dealt cards" players;
  deal_test "Check 8 players are dealt cards" extended_players;
  community_test "check that community has 3 cards post flop" players 3;
  (** This test should pass but does not *)
  test_state_end_subgame "simple test of end subgame" pre_end_state alice 150;
  test_first_n "test right number" [1;2;3;4;5;6;7;8] 4;
  test_first_n "test right number, entire list." [1;2;3;4] 4;
]

let ex_st =
  let open State in
  let open Poker in
  let player_names = ["Cesar"; "Dean"; "Parker"] in
  let start_stack = 100 in
  let players = create_players player_names start_stack in
  match init_state players 0 with
  | Legal st -> st
  | Illegal -> failwith "Illegal"

let ex_deal_st = deal ex_st
let ex_deal_st = flop ex_deal_st

let get_community_card_test 
    (name : string)
    (st : State.t)
    (expected_output : Poker.card list)
  : test =
  name >:: (fun _ -> assert_equal expected_output (get_community_cards st)) 

let community_card_test 
    (name : string)
    (st : State.t)
    (expected_output : string)
  : test =
  name >:: (fun _ -> assert_equal expected_output (community_cards_string st) ~printer: pp_string) 

let main_tests = [
  community_card_test "In Init stage community cards should not be dealt" 
    (ex_st) "";
  community_card_test "In Deal stage community cards should not be dealt" 
    (deal ex_st) "";
]

module TestBotInfo = struct
  let diff = Test
  let seed = 0
end

module MyTestBot = TestBot.Make(TestBotInfo)
let state = match (init_state players 0) with
  | Illegal -> failwith("Illegal init state should be legal")
  | Legal t -> t 

module MyBot = MyBot.Make(TestBotInfo)
let mybot_state_good = match (init_state players 10) with
  | Illegal -> failwith("Illegal init state should be legal")
  | Legal t -> 
    t |> incr_stage |> pay_big_blind


let bob_bad = {name = "Bob"; id = 2; active = true; stack = 100; 
               hole_cards = [(Two, Clubs); (Seven, Diamonds)]}
let bad_players = [alice;bob_bad]

let mybot_state_bad = match (init_state players 10) with
  | Illegal -> failwith("Illegal init state should be legal")
  | Legal t -> let st = t |> incr_stage |> pay_big_blind in 
    match raise st alice 20 with 
    | Illegal -> failwith("Illegal raise state should be legal")
    | Legal t -> t


let test_testbot  
    (name: string)   
    (bot_command : Command.t ) : test = 
  name >:: (fun _ ->
      assert_equal bot_command Call
    )

let test_mybot_goodhand
    (name: string)   
    (bot_command : Command.t ) : test = 
  name >:: (fun _ ->
      assert_equal bot_command Call
    )

let test_mybot_badhand    
    (name: string)   
    (bot_command : Command.t ) : test = 
  name >:: (fun _ ->
      assert_equal bot_command Fold
    )

let bot_tests = [
  test_testbot "Test that test bot calls" (MyTestBot.get_action state bob);
  test_mybot_goodhand "test mybot calls with good hand" 
    (MyBot.get_action mybot_state_good alice);
  test_mybot_badhand "test mybot folds with bad hand"
    (MyBot.get_action mybot_state_bad bob_bad);
]

let suite =
  "test suite for CS3110 Poker Project"  >::: List.flatten [
    poker_tests;
    command_tests;
    state_tests;
    main_tests;
    bot_tests;
  ]

let _ = run_test_tt_main suite