(** Determines the start and end of a round in the game *)

open Card
open Hand

(** [pass] represents the choice of each player in the game to either pass or
    play their turn *)
type pass =
  | Pass
  | NotPass

type playerList = {
  mutable p1 : pass ref;
  mutable p2 : pass ref;
  mutable p3 : pass ref;
  mutable p4 : pass ref;
}
(** [playerList] records if each player passed or played their turn in their
    last turn. When starting the game each player starts with NotPass. *)

val p : playerList
(** [p] is the playerList with all players set to NotPass *)

val bs_pass : playerList
(** [bs_pass] is the playerList with all players set to NotPass *)

val is_end : playerList -> bool
(** [is_end p] returns [true] if all players are set to Pass in [p] *)

val pass_list : playerList -> string list
(** [pass_list p] is [v1;..;v4] where vi is the converted the values of [p] to a
    string *)

val start_round : playerList -> unit
(** [start_round] resets all players in the playerList to have NotPass *)

val check_pass : playerList -> string -> bool
(** [check_pass pl p] checks to see if player [p] has passed yet. Returns [true]
    if Pass, else [false] *)

val end_round : playerList -> unit
(** [end_round p] resets the playerList to have NotPass if the [p] currently has
    all players set to Pass *)

val player_order : unit -> string
(** [player_order ()] randomly picks one of the four players in the game *)

val change_to_pass : string -> playerList -> unit
(** [change_to_pass plyer] sets the player associated with [plyer] to Pass in
    the playerList *)

val suggested_card_type : card list -> number list
(** [suggested_card_type pl] returns a list of numbers that occurs the most in
    [pl] *)

val suggested_play : number -> int -> card list -> card list option
(** [suggested_play n num pl] facilitates the game play of the human player by
    evaluating [pl] to identify if the player has any cards with number [n]. If
    so, return [Some x] where [x] represents a list of all number [n] cards in
    [pl] for the player to choose from. If [pl] does not contain cards with
    number [n], the suggestion will be to either skip and return [None] or place
    down the worst card(s) as [Some cl] where the number of cards placed down is
    determined by a random number between [1] and [(4 - num)], where [num] is
    the current number of cards on the table. If [num >= 4], then 1 card is
    placed down. *)

val bot_play : number -> int -> card list -> card list option
(** [bot_play n num pl] facilitates the game play of the bots by first checking
    if [pl] contains any cards with number [n]. If so, return [Some cl] where cl
    contains the cards with number [n] that the bot wants to put down. The
    number of cards the bot places down is determined by a random number between
    1 and the number of [n] cards the bot has. Else, the bot will randomly
    decide between playing and passing, where if the bot chooses to play, return
    [Some cl] where the number of cards placed down is determined by a random
    number between [1] and [(4 - num)], where [num] is the current number of
    cards on the table. If [num >= 4], then 1 card is placed down. If the bot
    chooses to pass, return [None]. *)
