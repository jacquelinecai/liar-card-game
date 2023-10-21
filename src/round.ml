(* *)
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

let startRound () = { p1 = NotPass; p2 = NotPass; p3 = NotPass; p4 = NotPass }

let endRound (p : playerList) : playerList =
  if p.p1 = Pass && p.p2 = Pass && p.p3 = Pass && p.p4 = Pass then startRound ()
  else p
