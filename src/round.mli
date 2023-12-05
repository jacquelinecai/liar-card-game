open Card
open Hand

type pass =
  | Pass
  | NotPass

type playerList = {
  mutable p1 : pass ref;
  mutable p2 : pass ref;
  mutable p3 : pass ref;
  mutable p4 : pass ref;
}

val p : playerList
val bs_pass : playerList
val is_end : playerList -> bool
val pass_to_string : pass -> string
val pass_list : playerList -> string list
val start_round : playerList -> unit
val end_round : playerList -> unit
val randomize : unit -> pass
val player_order : unit -> string
val change_to_pass : string -> playerList -> unit
val number_match : number -> string
val suggested_card_type : card list -> number list
val suggested_play : number -> int -> card list -> card list option
val bot_play : number -> int -> card list -> card list option
