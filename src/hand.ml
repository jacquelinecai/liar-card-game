open Card

type player =
  | Player1
  | Player2
  | Player3
  | Player4

let unshuffled_deck = card_list
let shuffled_deck = shuffle unshuffled_deck

let rec assign (a : int) (b : int) (deck : card list) (acc : card list) :
    card list =
  match deck with
  | [] -> acc
  | h :: t ->
      let card_number = List.length unshuffled_deck - List.length t in
      if card_number >= a && card_number <= b then assign a b t (h :: acc)
      else assign a b t acc

let rec order (deck : card list) : card list =
  List.sort
    (fun (s1, n1) (s2, n2) ->
      if n1 = n2 then compare s1 s2
      else
        match (n1, n2) with
        | Number x, (Jack | Queen | King) -> -1
        | (Jack | Queen | King), Number x -> 1
        | _ -> compare n1 n2)
    deck

let rec deck_to_string (deck : card list) : string =
  match deck with
  | [] -> ""
  | [ x ] -> card_to_string x
  | h :: t -> card_to_string h ^ ", " ^ deck_to_string t

let player1_hand = ref (assign 1 13 shuffled_deck [] |> order)
let player2_hand = ref (assign 14 26 shuffled_deck [] |> order)
let player3_hand = ref (assign 27 39 shuffled_deck [] |> order)
let player4_hand = ref (assign 40 52 shuffled_deck [] |> order)

let rec contains (c : card) (cl : card list) : bool =
  match cl with
  | [] -> false
  | h :: t -> if h = c then true else contains c t

exception InvalidCard

let rec updateDeck (c : card) (cl : card list) (acc : card list) : card list =
  if contains c cl then
    match cl with
    | [] -> []
    | h :: t -> if h = c then acc @ t else updateDeck c t (h :: acc)
  else raise InvalidCard
