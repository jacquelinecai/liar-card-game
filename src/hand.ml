type player =
  | Player1
  | Player2
  | Player3
  | Player4

type card = Card.card

let unshuffled_deck = Card.card_list
let shuffled_deck = Card.shuffle unshuffled_deck

(** [assign a b deck acc] returns a new card list from index a to b of an existing card list *)
let rec assign (a : int) (b : int) (deck : card list) (acc : card list) :
    card list =
  match deck with
  | [] -> acc
  | h :: t ->
      let card_number = List.length unshuffled_deck - List.length t in
      if card_number >= a && card_number <= b then assign a b t (h :: acc)
      else assign a b t acc

(** [order deck] returns the card list in order based on number and suit *)
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

 (** [deck_to_string deck] returns a string of each card in the card list*)
let rec deck_to_string (deck : card list) : string =
  match deck with
  | [] -> ""
  | [ x ] -> Card.card_to_string x
  | h :: t -> Card.card_to_string h ^ ", " ^ deck_to_string t

(** player_hand evenly distributes a shuffled deck to each player, and orders it*)
let player1_hand = assign 1 13 shuffled_deck [] |> order
let player2_hand = assign 14 26 shuffled_deck [] |> order
let player3_hand = assign 27 39 shuffled_deck [] |> order
let player4_hand = assign 40 52 shuffled_deck [] |> order

(** [contains c cl] returns true if the card list contains the card, else returns false *)
let rec contains (c : card) (cl : card list) : bool =
  match cl with
  | [] -> false
  | h :: t -> if h = c then true else contains c t

(**[updateDeck c cl] returns the card list without the card *)
let rec updateDeck (c : card) (cl : card list) : card list =
  match cl with
  | [] -> []
  | h :: t -> if h = c then t else h :: updateDeck c t
