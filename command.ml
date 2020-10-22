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
let rid_spaces cmd_lst : string list =
  let open List in
  cmd_lst 
  |> List.map (fun s -> String.trim s) 
  |> filter (fun x -> String.length x > 0)

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