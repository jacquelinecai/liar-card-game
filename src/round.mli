open Card
open Hand

type pass =
  | Pass
  | NotPass

type playerList = {
  mutable p1 : pass;
  mutable p2 : pass;
  mutable p3 : pass;
  mutable p4 : pass;
}

(* List of ordered card numbers *)
val cList : number list
val p : playerList
val is_end : playerList -> bool
val curr_round : int ref
val card_round : unit -> string
val start_round : playerList -> playerList
val end_round : playerList -> playerList
val randomize : unit -> pass
val player_order : unit -> string
val change_to_pass : string -> unit
val number_match : number -> string
val bot_play : number -> int -> card list -> card list option
