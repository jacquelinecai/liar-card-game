(* *)
open Hand
open Card

type pass =
  | Pass
  | NotPass

type playerList = {
  mutable p1 : pass;
  mutable p2 : pass;
  mutable p3 : pass;
  mutable p4 : pass;
}

let number_match c =
  match c with
  | Number x -> if x = 1 then "A" else string_of_int x
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"

let p = { p1 = NotPass; p2 = NotPass; p3 = NotPass; p4 = NotPass }
let player1 = p.p1
let player2 = p.p2
let player3 = p.p3
let player4 = p.p4

let cList =
  [
    Number 1;
    Number 2;
    Number 3;
    Number 4;
    Number 5;
    Number 6;
    Number 7;
    Number 8;
    Number 9;
    Number 10;
    Jack;
    Queen;
    King;
  ]

let curr_round = ref 0
let card_round () = number_match (List.nth cList !curr_round)

let increment () =
  if !curr_round = 11 then curr_round := 0 else curr_round := !curr_round + 1

let start_round (p : playerList) : playerList =
  { p1 = NotPass; p2 = NotPass; p3 = NotPass; p4 = NotPass }

let end_round (p : playerList) : playerList =
  if p.p1 = Pass && p.p2 = Pass && p.p3 = Pass && p.p4 = Pass then start_round p
  else p

let is_end (p : playerList) : bool =
  if p.p1 = Pass && p.p2 = Pass && p.p3 = Pass && p.p4 = Pass then true
  else false

let randomize () =
  let x = Random.bool () in
  if x then Pass else NotPass

let change_to_pass plyr =
  match plyr with
  | "Player 1" -> p.p1 <- Pass
  | "Player 2" -> p.p2 <- Pass
  | "Player 3" -> p.p3 <- Pass
  | "Player 4" -> p.p4 <- Pass
  | _ -> ()

let player_order () =
  let y = Random.int 4 in
  match y with
  | 0 -> "Player 1"
  | 1 -> "Player 2"
  | 2 -> "Player 3"
  | _ -> "Player 4"

let rec rand_seq num l acc =
  if num > 0 then (
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
