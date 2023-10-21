open Array
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

let card_to_string c =
  let suit = suit_match c in
  match c with
  | Clubs, Number x ->
      let num = if x = 1 then "A" else string_of_int x in
      num ^ " " ^ suit
  | Clubs, Jack -> "J" ^ " " ^ suit
  | Clubs, Queen -> "Q" ^ " " ^ suit
  | Clubs, King -> "K" ^ " " ^ suit
  | Diamonds, Number x ->
      let num = if x = 1 then "A" else string_of_int x in
      num ^ " " ^ suit
  | Diamonds, Jack -> "J" ^ " " ^ suit
  | Diamonds, Queen -> "Q" ^ " " ^ suit
  | Diamonds, King -> "K" ^ " " ^ suit
  | Hearts, Number x ->
      let num = if x = 1 then "A" else string_of_int x in
      num ^ " " ^ suit
  | Hearts, Jack -> "J" ^ " " ^ suit
  | Hearts, Queen -> "Q" ^ " " ^ suit
  | Hearts, King -> "K" ^ " " ^ suit
  | Spades, Number x ->
      let num = if x = 1 then "A" else string_of_int x in
      num ^ " " ^ suit
  | Spades, Jack -> "J" ^ " " ^ suit
  | Spades, Queen -> "Q" ^ " " ^ suit
  | Spades, King -> "K" ^ " " ^ suit

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
