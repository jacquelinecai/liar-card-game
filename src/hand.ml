open Card

type player =
  | Player1
  | Player2
  | Player3
  | Player4

let unshuffled_deck = card_list
let shuffled_deck = shuffle unshuffled_deck

(** [assign a b deck acc] returns a new card list from index a to b of an
    existing card list *)
let rec assign (a : int) (b : int) (deck : card list) (acc : card list) :
    card list =
  match deck with
  | [] -> acc
  | h :: t ->
      let card_number = List.length unshuffled_deck - List.length t in
      if card_number >= a && card_number <= b then assign a b t (h :: acc)
      else assign a b t acc

(** [order deck] returns the card list in order based on number and suit *)
let order (deck : card list) : card list =
  List.sort
    (fun (s1, n1) (s2, n2) ->
      if n1 = n2 then compare s1 s2
      else
        match (n1, n2) with
        | Number x, (Jack | Queen | King) -> -1
        | (Jack | Queen | King), Number x -> 1
        | _ -> compare n1 n2)
    deck

(** [order_num nl] returns the number list in order *)
let order_num (nList : number list) : number list =
  List.sort
    (fun n1 n2 ->
      match (n1, n2) with
      | Number x, (Jack | Queen | King) -> -1
      | (Jack | Queen | King), Number x -> 1
      | _ -> compare n1 n2)
    nList

(** [deck_to_string deck] returns a string of each card in [deck] *)
let rec deck_to_string (deck : card list) : string =
  match deck with
  | [] -> ""
  | [ x ] -> card_to_string x
  | h :: t -> card_to_string h ^ ", " ^ deck_to_string t

(** [num_list_to_string nl] returns a string of each number in [nl] *)
let rec num_list_to_string (nList : number list) : string =
  match nList with
  | [] -> ""
  | [ x ] -> number_match x
  | h :: t -> number_match h ^ ", " ^ num_list_to_string t

(** player_hand evenly distributes a shuffled deck to each player and orders it *)

let player1_hand = ref (assign 1 13 shuffled_deck [] |> order)
let player2_hand = ref (assign 14 26 shuffled_deck [] |> order)
let player3_hand = ref (assign 27 39 shuffled_deck [] |> order)
let player4_hand = ref (assign 40 52 shuffled_deck [] |> order)

let player_hand_size (player : int) =
  match player with
  | 1 -> List.length !player1_hand
  | 2 -> List.length !player2_hand
  | 3 -> List.length !player3_hand
  | _ -> List.length !player4_hand

(** [containsNum n cl] returns true if cl contains the card number n, else
    returns false *)
let rec containsNum (n : number) (cl : card list) : bool =
  match cl with
  | [] -> false
  | h :: t -> if snd h = n then true else containsNum n t

(** [numCards n cl acc] returns the number of cards in cl that have a number n *)
let rec numCards (n : number) (cl : card list) (acc : int) : int =
  match cl with
  | [] -> acc
  | h :: t -> if snd h = n then numCards n t (acc + 1) else numCards n t acc

(** [nCards n amt cl acc] returns a card list with amt number of number n cards
    in cl. Requires that [amt < numCards n cl acc] *)
let rec nCards (n : number) (amt : int) (cl : card list) (acc : card list) :
    card list =
  if amt > 0 then
    match cl with
    | [] -> acc
    | h :: t ->
        if snd h = n then nCards n (amt - 1) t (h :: acc)
        else nCards n amt t acc
  else acc

(** [getRandCards lst idx deck acc] returns the cards of [deck] from the indices
    in [lst] *)
let rec getRandCards (lst : int list) (idx : int) (deck : card list)
    (acc : card list) : card list =
  if List.length lst > 0 then
    let sorted = List.sort Stdlib.compare lst in
    match deck with
    | [] -> acc
    | h :: t ->
        if idx = List.hd sorted then
          getRandCards (List.tl sorted) (idx + 1) t (h :: acc)
        else getRandCards sorted (idx + 1) t acc
  else acc

(** [firstNCards deck n] returns the first [n] cards in [deck] *)
let rec firstNCards (deck : card list) (num : int) : card list =
  if num > 0 then
    match deck with
    | [] -> failwith "no cards remaining"
    | h :: t -> h :: firstNCards t (num - 1)
  else []

exception InvalidCard

(**[updateDeck c cl []] returns the card list without the card c *)
let rec updateDeck (c : card) (cl : card list) (acc : card list) : card list =
  if contains c cl then
    match cl with
    | [] -> acc
    | h :: t -> if h = c then acc @ t else updateDeck c t (h :: acc)
  else raise InvalidCard

(**[updateDeckWithCardList clr cl] returns the card list without the cards from
   clr *)
let rec updateDeckWithCardList (clr : card list) (cl : card list) : card list =
  match clr with
  | [] -> cl
  | h :: t ->
      let newDeck = updateDeck h cl [] in
      updateDeckWithCardList t newDeck
