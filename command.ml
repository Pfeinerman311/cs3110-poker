open Poker
open State
open Command

(* type action_amount = string list *)

type command =
  | Raise of int
  | Start
  | Hand
  | Table
  | Call
  | Fold
  | Quit

exception Malformed

exception InsufficientFunds


(* [rid_spaces object_phrase] is a helper function for parse_branch that takes
   in a string list [object_phrase] which represents the object_phrase, and gets
   rid of trailing or leading spaces in each element and then filters for only
   those elements which are empty strings. *)
let rid_spaces object_phrase : string list =
  let open List in
  object_phrase 
  |> List.map (fun s -> String.trim s) 
  |> filter (fun x -> String.length x > 0)

(* [parse_branch verb object_phrase] is a helper function for parse which
   evaluates [object_phrase] (assuming that it is what follows [verb] in the
   argument for parse) and returns the command that results from [object_phrase]
   or throws an exception. *)
(* let parse_raise (amount : string) : command = 
   try (int_of_string amount) with
   | Failure "int_of_string" -> raise Malformed
   | value -> Raise ("raise", value) *)



(* match amount with
   | [] -> raise Malformed
   | op -> Go (rid_spaces op)
   else if verb = "quit" then 
   match object_phrase with
   | [] -> Quit
   | _ -> raise Malformed
   else raise Malformed *)


let parse (str : string) : command =
  let open String in
  let cmd_lst = str |> trim |> split_on_char ' ' |> rid_spaces in
  match cmd_lst with
  | [] ->  raise Malformed
  | "start" :: [] -> Start
  | "hand" :: [] -> Hand
  | "table" :: [] -> Table
  | "call" :: [] -> Call
  | "fold" :: [] -> Fold
  | "quit" :: [] -> Quit
  | "raise" :: num :: [] -> Raise (int_of_string num)
  | _ -> raise Malformed