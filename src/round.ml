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

let bs_pass =
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

let start_round : unit =
  p.p1 := NotPass;
  p.p2 := NotPass;
  p.p3 := NotPass;
  p.p4 := NotPass

let end_round (p : playerList) : unit =
  if p.p1 = ref Pass && p.p2 = ref Pass && p.p3 = ref Pass && p.p4 = ref Pass
  then start_round
  else ()

let is_end (p : playerList) : bool =
  if p.p1 = ref Pass && p.p2 = ref Pass && p.p3 = ref Pass && p.p4 = ref Pass
  then true
  else false

let randomize () =
  let () = Random.self_init () in
  let x = Random.bool () in
  if x then Pass else NotPass

let change_to_pass plyr pList =
  match plyr with
  | "Player 1" -> pList.p1 := Pass
  | "Player 2" -> pList.p2 := Pass
  | "Player 3" -> pList.p3 := Pass
  | "Player 4" -> pList.p4 := Pass
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

(** [hash_contains hash n] returns None if [n] is not contained in [hash].
    Otherwise, return [Some num] where [num] is the associated int for number
    [n]. *)
let rec hash_contains hash n =
  match hash with
  | [] -> None
  | (n', num) :: t -> if n' = n then Some num else hash_contains t n

(** [choose_type cl acc] returns an updated tuple list (modeling a hashmap) that
    represents the current count of card number types in [pl]. *)
let rec choose_type cl acc =
  match cl with
  | [] -> acc
  | h :: t -> (
      let x = hash_contains acc (snd h) in
      match x with
      | None -> choose_type t ((snd h, 1) :: acc)
      | Some x -> choose_type t ((snd h, x + 1) :: acc))

let suggested_card_type cl =
  let sorted_counts =
    List.sort (fun (_, x) (_, x') -> Stdlib.compare x' x) (choose_type cl [])
  in
  let max_frequency =
    match sorted_counts with
    | [] -> 0
    | (_, freq) :: _ -> freq
  in
  List.filter (fun (_, freq) -> freq = max_frequency) sorted_counts
  |> List.map fst

let assign_frequency_values hand =
  let card_counts =
    List.fold_left
      (fun acc card ->
        let count =
          match List.assoc_opt card acc with
          | Some c -> c + 1
          | None -> 1
        in
        (card, count) :: acc)
      [] hand
  in

  let total_cards = float_of_int (List.length hand) in

  List.map
    (fun card ->
      let frequency_value =
        match List.assoc_opt card card_counts with
        | Some count -> 1.0 -. (float_of_int count /. total_cards)
        | None -> 1.0
      in
      (card, Some frequency_value))
    hand

let suggested_play n num cl =
  if containsNum n cl then
    let x = numCards n cl 0 in
    Some (nCards n x cl [])
  else if
    let () = Random.self_init () in
    Random.bool ()
  then
    let amt = 4 - num in
    if amt <= 0 then Some [ assign_frequency_values cl |> List.hd |> fst ]
    else
      let () = Random.self_init () in
      let x = Random.int amt + 1 in
      Some (firstNCards cl x)
  else None

let bot_play n num cl =
  let () = Random.self_init () in
  if containsNum n cl then
    let x = Random.int (numCards n cl 0) + 1 in
    Some (nCards n x cl [])
  else if
    let () = Random.self_init () in
    Random.bool ()
  then
    let amt = 4 - num in
    if amt <= 0 then
      let () = Random.self_init () in
      let idx = Random.int (List.length cl) in
      Some (getRandCards [ idx ] 0 cl [])
    else
      let () = Random.self_init () in
      let x = Random.int amt + 1 in
      Some (getRandCards (rand_seq x (List.length cl) []) 0 cl [])
  else None
