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

let p0 = {name = "Parker"; id = 2; active = true; stack = 0; hole_cards = hl_0}
let p1 = {name = "Parker"; id = 2; active = true; stack = 0; hole_cards = hl_1}
let p2 = {name = "Parker"; id = 2; active = true; stack = 0; hole_cards = hl_2}
let p3 = {name = "Parker"; id = 2; active = true; stack = 0; hole_cards = hl_3}
let p4 = {name = "Parker"; id = 2; active = true; stack = 0; hole_cards = hl_4}
let p5 = {name = "Parker"; id = 2; active = true; stack = 0; hole_cards = hl_5}
let p6 = {name = "Parker"; id = 2; active = true; stack = 0; hole_cards = hl_6}
let p7 = {name = "Parker"; id = 2; active = true; stack = 0; hole_cards = hl_7}
let p8 = {name = "Parker"; id = 2; active = true; stack = 0; hole_cards = hl_8}
let p9 = {name = "Parker"; id = 2; active = true; stack = 0; hole_cards = hl_9}


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

let poker_tests = 
  [
    best_hand_test "Parker highcard test" p0 c1 h0; 
    best_hand_test "Parker pair test" p1 c1 h1;
    best_hand_test "Parker two pair test" p2 c1 h2;
    best_hand_test "Parker three of a kind test" p3 c1 h3;
    best_hand_test "Parker straight test" p4 c1 h4;
    best_hand_test "Parker flush test" p5 c1 h5;
    best_hand_test "Parker full house test" p6 c2 h6;
    best_hand_test "Parker four of a kind test" p7 c2 h7;
    best_hand_test "Parker straight flush test" p8 c3 h8;
    best_hand_test "Parker royal flush test" p9 c4 h9;
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

let state_tests = [
  pot_test "simple pot increase test with a player raising" players 350;
  deal_test "check 2 players are dealt cards" players;
  deal_test "Check 8 players are dealt cards" extended_players;
  community_test "check that community has 3 cards post flop" players 3;
  (** This test should pass but does not *)
  test_state_end_subgame "simple test of end subgame" pre_end_state alice 150;
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
  community_card_test "In Init stage community cards should not be dealt" (ex_st) "";
  community_card_test "In Deal stage community cards should not be dealt" (deal ex_st) "";
  (* The tests below are passing, but shouldn't 
     get_community_card_test 
     "In Flop stage community cards should be dealt, aka a non-empty Poker.card list" 
     (flop ex_st) [];
     get_community_card_test 
     "In Turn stage community cards should be dealt, aka a non-empty Poker.card list" 
     (turn ex_st) []; *)
]

module TestBotInfo = struct
  let diff = Test
  let seed = 0
end

module MyTestBot = TestBot.Make(TestBotInfo)
let state = match (init_state players 0) with
  | Illegal -> failwith("Illegal Raise should be legal")
  | Legal t -> t 

let test_testbot  
    (name: string)   
    (bot_command : command ) : test = 
  name >:: (fun _ ->
      assert_equal bot_command Call
    )

let bot_tests = [
  test_testbot "Test fold bot folds" (MyTestBot.get_action state)
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