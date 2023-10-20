type player =
  | Player1
  | Player2
  | Player3
  | Player4

type card = Card.card

let unshuffled_deck = Card.card_list
let shuffled_deck = Card.shuffle Card.card_list

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
        | Card.Number x, (Card.Jack | Card.Queen | Card.King) -> -1
        | (Card.Jack | Card.Queen | Card.King), Card.Number x -> 1
        | _ -> compare n1 n2)
    deck

let rec deck_to_string (deck : card list) : string =
  match deck with
  | [] -> ""
  | [ x ] -> Card.card_to_string x
  | h :: t -> Card.card_to_string h ^ ", " ^ deck_to_string t

let player1_hand = assign 1 13 shuffled_deck [] |> order
let player2_hand = assign 14 26 shuffled_deck [] |> order
let player3_hand = assign 27 39 shuffled_deck [] |> order
let player4_hand = assign 40 52 shuffled_deck [] |> order
