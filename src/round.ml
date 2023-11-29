(* *)
open Hand
open Card

type pass =
  | Pass
  | NotPass

type playerList = {
  mutable p1 : pass ref;
  mutable p2 : pass ref;
  mutable p3 : pass ref;
  mutable p4 : pass ref;
}

let pass_to_string x =
  match x with
  | Pass -> "Pass"
  | NotPass -> "NotPass"

let number_match c =
  match c with
  | Number x -> if x = 1 then "A" else string_of_int x
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"

let p =
  { p1 = ref NotPass; p2 = ref NotPass; p3 = ref NotPass; p4 = ref NotPass }

let player1 = p.p1
let player2 = p.p2
let player3 = p.p3
let player4 = p.p4

let pass_list (p : playerList) : string list =
  match p with
  | { p1; p2; p3; p4 } ->
      [
        pass_to_string !p1;
        pass_to_string !p2;
        pass_to_string !p3;
        pass_to_string !p4;
      ]

let start_round (p : playerList) : unit =
  p.p1 := NotPass;
  p.p2 := NotPass;
  p.p3 := NotPass;
  p.p4 := NotPass

let end_round (p : playerList) : unit =
  if p.p1 = ref Pass && p.p2 = ref Pass && p.p3 = ref Pass && p.p4 = ref Pass
  then start_round p
  else ()

let is_end (p : playerList) : bool =
  if p.p1 = ref Pass && p.p2 = ref Pass && p.p3 = ref Pass && p.p4 = ref Pass
  then true
  else false

let randomize () =
  let () = Random.self_init () in
  let x = Random.bool () in
  if x then Pass else NotPass

let change_to_pass plyr =
  match plyr with
  | "Player 1" -> p.p1 := Pass
  | "Player 2" -> p.p2 := Pass
  | "Player 3" -> p.p3 := Pass
  | "Player 4" -> p.p4 := Pass
  | _ -> ()

let player_order () =
  let () = Random.self_init () in
  let y = Random.int 4 in
  match y with
  | 0 -> "Player 1"
  | 1 -> "Player 2"
  | 2 -> "Player 3"
  | _ -> "Player 4"

let rec rand_seq num l acc =
  if num > 0 then (
    let () = Random.self_init () in
    let x = ref (Random.int (l - num)) in
    while List.exists (fun y -> !x = y) acc do
      x := Random.int (l - num)
    done;
    rand_seq (num - 1) l (!x :: acc))
  else acc

(** [bot_play n num pl] facilitates the game play of the bots by first checking
    if pl contains any cards with number n. If so, return [Some cl] where cl
    contains the cards with number n that the bot wants to put down. The number
    of cards the bot places down is determined by a random number between 1 and
    the number of n cards the bot has. Else, the bot will randomly decide
    between playing and passing, where if the bot chooses to play, return
    [Some cl] where the number of cards placed down is determined by a random
    number between 1 and (4 - num). If num >= 4, then 1 card is placed down. If
    the bot chooses to pass, return [None]. *)
let bot_play n num cl =
  let () = Random.self_init () in
  if containsNum n cl then
    let x = Random.int (numCards n cl 0) + 1 in
    Some (nCards n x cl [])
  else if Random.bool () then
    let amt = 4 - num in
    if amt <= 0 then
      let idx = Random.int (List.length cl) in
      Some (getRandCards [ idx ] 0 cl [])
    else
      let x = Random.int amt + 1 in
      Some (getRandCards (rand_seq x (List.length cl) []) 0 cl [])
  else None
