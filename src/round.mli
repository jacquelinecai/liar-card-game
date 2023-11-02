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
val curr_round : int ref
val card_round : unit->string
val start_round : playerList -> playerList
val end_round : playerList -> playerList
val randomize : unit-> pass
val order : unit-> string
val change_to_pass:string->unit
