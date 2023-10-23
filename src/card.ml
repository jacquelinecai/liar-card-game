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

let string_to_card s =
  match s with
  | "AD" -> Some (Diamonds, Number 1)
  | "AH" -> Some (Hearts, Number 1)
  | "AC" -> Some (Clubs, Number 1)
  | "AS" -> Some (Spades, Number 1)
  | "2D" -> Some (Diamonds, Number 2)
  | "2H" -> Some (Hearts, Number 2)
  | "2C" -> Some (Clubs, Number 2)
  | "2S" -> Some (Spades, Number 2)
  | "3D" -> Some (Diamonds, Number 3)
  | "3H" -> Some (Hearts, Number 3)
  | "3C" -> Some (Clubs, Number 3)
  | "3S" -> Some (Spades, Number 3)
  | "4D" -> Some (Diamonds, Number 4)
  | "4H" -> Some (Hearts, Number 4)
  | "4C" -> Some (Clubs, Number 4)
  | "4S" -> Some (Spades, Number 4)
  | "5D" -> Some (Diamonds, Number 5)
  | "5H" -> Some (Hearts, Number 5)
  | "5C" -> Some (Clubs, Number 5)
  | "5S" -> Some (Spades, Number 5)
  | "6D" -> Some (Diamonds, Number 6)
  | "6H" -> Some (Hearts, Number 6)
  | "6C" -> Some (Clubs, Number 6)
  | "6S" -> Some (Spades, Number 6)
  | "7D" -> Some (Diamonds, Number 7)
  | "7H" -> Some (Hearts, Number 7)
  | "7C" -> Some (Clubs, Number 7)
  | "7S" -> Some (Spades, Number 7)
  | "8D" -> Some (Diamonds, Number 8)
  | "8H" -> Some (Hearts, Number 8)
  | "8C" -> Some (Clubs, Number 8)
  | "8S" -> Some (Spades, Number 8)
  | "9D" -> Some (Diamonds, Number 9)
  | "9H" -> Some (Hearts, Number 9)
  | "9C" -> Some (Clubs, Number 9)
  | "9S" -> Some (Spades, Number 9)
  | "10D" -> Some (Diamonds, Number 10)
  | "10H" -> Some (Hearts, Number 10)
  | "10C" -> Some (Clubs, Number 10)
  | "10S" -> Some (Spades, Number 10)
  | "JD" -> Some (Diamonds, Jack)
  | "JH" -> Some (Hearts, Jack)
  | "JC" -> Some (Clubs, Jack)
  | "JS" -> Some (Spades, Jack)
  | "QD" -> Some (Diamonds, Queen)
  | "QH" -> Some (Hearts, Queen)
  | "QC" -> Some (Clubs, Queen)
  | "QS" -> Some (Spades, Queen)
  | "KD" -> Some (Diamonds, King)
  | "KH" -> Some (Hearts, King)
  | "KC" -> Some (Clubs, King)
  | "KS" -> Some (Spades, King)
  | _ -> None
