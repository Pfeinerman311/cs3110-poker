type command =
  | Raise of int
  | Start
  | Hand
  | Hole
  | Table
  | Call
  | Fold
  | Quit

exception Malformed

exception InadequateCommand of string


(* [rid_spaces cmd_lst] is a helper function for parse_branch that takes
   in a string list [cmd_lst] which represents the command, and gets
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
  | "go" :: [] -> Start
  | "hand" :: [] -> Hand
  | "hole" :: [] -> Hole
  | "table" :: [] -> Table
  | "call" :: [] -> Call
  | "fold" :: [] -> Fold
  | "leave" :: [] -> Quit
  | "raise" :: num :: [] -> Raise (int_of_string num)
  | _ -> raise Malformed