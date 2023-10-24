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

let p = { p1 = NotPass; p2 = NotPass; p3 = NotPass; p4 = NotPass }
let player1 = p.p1
let player2 = p.p2
let player3 = p.p3
let player3 = p.p4

let startRound (p : playerList) : playerList =
  { p1 = NotPass; p2 = NotPass; p3 = NotPass; p4 = NotPass }

let endRound (p : playerList) : playerList =
  if p.p1 = Pass && p.p2 = Pass && p.p3 = Pass && p.p4 = Pass then startRound p
  else p

(* Randomizing the actions of the bot: *)
let randomize () =
  let x = Random.bool () in
  if x then Pass else NotPass
