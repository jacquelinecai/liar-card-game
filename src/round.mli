type pass =
  | Pass
  | NotPass

type playerList = {
  p1 : pass;
  p2 : pass;
  p3 : pass;
  p4 : pass;
}

val startRound : playerList -> playerList
val endRound : playerList -> playerList
