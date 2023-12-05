(*Determines the start and end of a round in the game *)
open Card
open Hand

(* [pass] represents the choice of each player in the game to either pass or
   play their turn *)
type pass =
  | Pass
  | NotPass

(* [playerList] records if each player passed or played their turn in their last
   turn. When starting the game each player starts with NotPass. *)
type playerList = {
  mutable p1 : pass ref;
  mutable p2 : pass ref;
  mutable p3 : pass ref;
  mutable p4 : pass ref;
}

(* [p] is the playerList with all players set to NotPass *)
val p : playerList
val is_end : playerList -> bool

(* [pass_list p] is [v1;..;v4] where vi is the converted the values of [p] to a
   string *)
val pass_list : playerList -> string list

(* [start_round] resets all players in the playerList to have NotPass *)
val start_round : unit

(* [end_round p] resets the playerList to have NotPass if the [p] currently has
   all players set to Pass *)
val end_round : playerList -> unit

(* [player_order ()] randomly picks one of the four players in the game *)
val player_order : unit -> string
val change_to_pass : string -> unit
val number_match : number -> string
val suggested_card_type : card list -> number
val suggested_play : number -> int -> card list -> card list option

(* [bot_play n num pl] facilitates the game play of the bots by first checking
   if [pl] contains any cards with number [n]. If so, return [Some cl] where cl
   contains the cards with number [n] that the bot wants to put down. The number
   of cards the bot places down is determined by a random number between 1 and
   the number of [n] cards the bot has. Else, the bot will randomly decide
   between playing and passing, where if the bot chooses to play, return [Some
   cl] where the number of cards placed down is determined by a random number
   between [1] and [(4 - num)], where [num] is the current number of cards on
   the table. If [num >= 4], then 1 card is placed down. If the bot chooses to
   pass, return [None]. *)
val bot_play : number -> int -> card list -> card list option
