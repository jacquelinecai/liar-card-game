open Array
open Random

type number =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King

type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

type card = suit * number

let card_list : card list =
  [
    (Clubs, Ace);
    (Clubs, Two);
    (Clubs, Three);
    (Clubs, Four);
    (Clubs, Five);
    (Clubs, Six);
    (Clubs, Seven);
    (Clubs, Eight);
    (Clubs, Nine);
    (Clubs, Ten);
    (Clubs, Jack);
    (Clubs, Queen);
    (Clubs, King);
    (Diamonds, Ace);
    (Diamonds, Two);
    (Diamonds, Three);
    (Diamonds, Four);
    (Diamonds, Five);
    (Diamonds, Six);
    (Diamonds, Seven);
    (Diamonds, Eight);
    (Diamonds, Nine);
    (Diamonds, Ten);
    (Diamonds, Jack);
    (Diamonds, Queen);
    (Diamonds, King);
    (Hearts, Ace);
    (Hearts, Two);
    (Hearts, Three);
    (Hearts, Four);
    (Hearts, Five);
    (Hearts, Six);
    (Hearts, Seven);
    (Hearts, Eight);
    (Hearts, Nine);
    (Hearts, Ten);
    (Hearts, Jack);
    (Hearts, Queen);
    (Hearts, King);
    (Spades, Ace);
    (Spades, Two);
    (Spades, Three);
    (Spades, Four);
    (Spades, Five);
    (Spades, Six);
    (Spades, Seven);
    (Spades, Eight);
    (Spades, Nine);
    (Spades, Ten);
    (Spades, Jack);
    (Spades, Queen);
    (Spades, King);
  ]

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
