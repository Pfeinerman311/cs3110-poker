open Bot

module Make = functor (I : BotInfo) -> struct

  module Info = I

  let valid_bot = 
    match I.diff with
    | Test -> ()
    | _ -> failwith "FoldBot must have difficulty Fold"

  let get_action s = 
    if State.get_stage s = River then (Fold :Command.command)
    else Call

end