(** Parsing of player commands in a game of virtual poker *)


(** A command consists of an action and possibly an action amount as well. For
    example, a command can be ["start"] or it can also be ["Raise 56"]. The
    former begins the game, the latter raises the bet by 56 coins. *)
type command =
  | Raise of int
  | Start
  | Hand
  | Call
  | Fold
  | Quit
  | Help


(** Insufficient funds is raised when a player's input string consists of
    anything other than "raise" and has more  *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word will always be the action taken by the player. This can be to raise the
    bet, start the game, show your hand, call, fold for the round, or quit the 
    game.

    Requires: 
    1. [str] contains only lowercase alphanumeric (a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).
    2. If there is a substring following the action, then the command is 
    Raise and the substring consists of a string representation of an int.

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the string contains no action,
    or if the action is "raise" and there is not a number following it,
    or if the action is not "raise" but there is something following it.
    [Failure "int_of_string"] if the substring following "raise" is not a
    string representation of an int *)
val parse : string -> command


(** NOTES:
    Antes are automatic, so agreeing to start is agreeing to ante *)

(** There should be two sets of commands, playing commands and game commands *)

(** fold commmand should allow a player to transition their player type in 
    the game state into the inactive players field *)


