type player =
  | Player1
  | Player2
  | Player3
  | Player4

type card = Card.card

let shuffled_deck = Card.shuffle Card.card_list

let rec assign (a : int) (b : int) (deck : card list) (acc : card list) :
    card list =
  match deck with
  | [] -> []
  | h :: t ->
      let card_number = List.length shuffled_deck - List.length t in
      if card_number > a && card_number < b then h :: acc else assign a b t acc

let rec order (deck : card list) : card list =
  List.sort
    (fun (s1, n1) (s2, n2) -> if n1 = n2 then compare s1 s2 else compare n1 n2)
    deck

let player1_hand = assign 0 12 shuffled_deck [] |> order
let player2_hand = assign 13 25 shuffled_deck [] |> order
let player3_hand = assign 26 38 shuffled_deck [] |> order
let player4_hand = assign 39 52 shuffled_deck [] |> order
