
type difficulty = Fold | Easy | Medium | Hard

module type BotInfo = sig
  val diff : difficulty
  val seed : int
end

module type Bot = sig
  module Info : BotInfo
  val get_action : State.t -> Command.command
end

module type BotMaker = 
  functor (I : BotInfo) -> Bot with module Info = I