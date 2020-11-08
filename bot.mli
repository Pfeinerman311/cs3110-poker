(** [difficulty] represents a bot's level of complexity and skill*)
type difficulty = Fold | Easy | Medium | Hard

(** A module that matches [BotInfo] is suitable for use as
    the type of Info in a [Bot]. *)
module type BotInfo = sig
  val diff : difficulty
  val bot_ID : int
  val seed : int
end

(** A [Bot] takes a game state and produces a command *)
module type Bot = sig

  (** [Info] is a module representing information about the bot*)
  module Info : BotInfo

  (** [get_action t] take a game state and produces a command *)
  val get_action : State.t -> Command.command
end

module type BotMaker = 
  functor (I : BotInfo) -> Bot with module Info = I