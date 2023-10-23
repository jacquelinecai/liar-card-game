open Random

type number =
  | Number of int
  | Jack
  | Queen
  | King

type suit =
  | Diamonds
  | Clubs
  | Hearts
  | Spades

type card = suit * number

let card_list : card list =
  [
    (Clubs, Number 1);
    (Clubs, Number 2);
    (Clubs, Number 3);
    (Clubs, Number 4);
    (Clubs, Number 5);
    (Clubs, Number 6);
    (Clubs, Number 7);
    (Clubs, Number 8);
    (Clubs, Number 9);
    (Clubs, Number 10);
    (Clubs, Jack);
    (Clubs, Queen);
    (Clubs, King);
    (Diamonds, Number 1);
    (Diamonds, Number 2);
    (Diamonds, Number 3);
    (Diamonds, Number 4);
    (Diamonds, Number 5);
    (Diamonds, Number 6);
    (Diamonds, Number 7);
    (Diamonds, Number 8);
    (Diamonds, Number 9);
    (Diamonds, Number 10);
    (Diamonds, Jack);
    (Diamonds, Queen);
    (Diamonds, King);
    (Hearts, Number 1);
    (Hearts, Number 2);
    (Hearts, Number 3);
    (Hearts, Number 4);
    (Hearts, Number 5);
    (Hearts, Number 6);
    (Hearts, Number 7);
    (Hearts, Number 8);
    (Hearts, Number 9);
    (Hearts, Number 10);
    (Hearts, Jack);
    (Hearts, Queen);
    (Hearts, King);
    (Spades, Number 1);
    (Spades, Number 2);
    (Spades, Number 3);
    (Spades, Number 4);
    (Spades, Number 5);
    (Spades, Number 6);
    (Spades, Number 7);
    (Spades, Number 8);
    (Spades, Number 9);
    (Spades, Number 10);
    (Spades, Jack);
    (Spades, Queen);
    (Spades, King);
  ]

let suit_match c =
  match c with
  | Clubs, _ -> "♣"
  | Diamonds, _ -> "♦"
  | Hearts, _ -> "♥"
  | Spades, _ -> "♠"

let number_match c =
  match c with
  | _, Number x -> if x = 1 then "A" else string_of_int x
  | _, Jack -> "J"
  | _, Queen -> "Q"
  | _, King -> "K"

let card_to_string c =
  let suit = suit_match c in
  let number = number_match c in
  number ^ " " ^ suit

(** Implementation based on the Fisher-Yates Shuffling Algorithm:
    https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle*)
let shuffle d =
  let d_arr = Array.of_list d in
  for i = 51 downto 0 do
    let j = Random.int (i + 1) in
    let temp = d_arr.(j) in
    d_arr.(j) <- d_arr.(i);
    d_arr.(i) <- temp
  done;

  Array.to_list d_arr

let string_suit_match s =
  let suit = String.sub s 1 1 in
  match suit with
  | "D" -> Some Diamonds
  | "H" -> Some Hearts
  | "C" -> Some Clubs
  | "S" -> Some Spades
  | _ -> None

let string_number_match s =
  let num = String.sub s 0 1 in
  match num with
  | "A" -> Some (Number 1)
  | "2" -> Some (Number 2)
  | "3" -> Some (Number 3)
  | "4" -> Some (Number 4)
  | "5" -> Some (Number 5)
  | "6" -> Some (Number 6)
  | "7" -> Some (Number 7)
  | "8" -> Some (Number 8)
  | "9" -> Some (Number 9)
  | "10" -> Some (Number 10)
  | "J" -> Some Jack
  | "Q" -> Some Queen
  | "K" -> Some King
  | _ -> None

let string_to_card s =
  if String.length s = 2 then
    let suit = string_suit_match s in
    let num = string_number_match s in
    match (suit, num) with
    | Some x, Some y -> Some (x, y)
    | _ -> None
  else None
