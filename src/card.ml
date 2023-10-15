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
  | Diamonds
  | Clubs
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

let fileValue (c : card) : string =
  match c with
  | Clubs, Ace -> "card-images/ace_of_clubs.png"
  | Clubs, Two -> "card-images/2_of_clubs.png"
  | Clubs, Three -> "card-images/3_of_clubs.png"
  | Clubs, Four -> "card-images/4_of_clubs.png"
  | Clubs, Five -> "card-images/5_of_clubs.png"
  | Clubs, Six -> "card-images/6_of_clubs.png"
  | Clubs, Seven -> "card-images/7_of_clubs.png"
  | Clubs, Eight -> "card-images/8_of_clubs.png"
  | Clubs, Nine -> "card-images/9_of_clubs.png"
  | Clubs, Ten -> "card-images/10_of_clubs.png"
  | Clubs, Jack -> "card-images/jack_of_clubs.png"
  | Clubs, Queen -> "card-images/queen_of_clubs.png"
  | Clubs, King -> "card-images/king_of_clubs.png"
  | Diamonds, Ace -> "card-images/ace_of_diamonds.png"
  | Diamonds, Two -> "card-images/2_of_diamonds.png"
  | Diamonds, Three -> "card-images/3_of_diamonds.png"
  | Diamonds, Four -> "card-images/4_of_diamonds.png"
  | Diamonds, Five -> "card-images/5_of_diamonds.png"
  | Diamonds, Six -> "card-images/6_of_diamonds.png"
  | Diamonds, Seven -> "card-images/7_of_diamonds.png"
  | Diamonds, Eight -> "card-images/8_of_diamonds.png"
  | Diamonds, Nine -> "card-images/9_of_diamonds.png"
  | Diamonds, Ten -> "card-images/10_of_diamonds.png"
  | Diamonds, Jack -> "card-images/jack_of_diamonds.png"
  | Diamonds, Queen -> "card-images/queen_of_diamonds.png"
  | Diamonds, King -> "card-images/king_of_diamonds.png"
  | Hearts, Ace -> "card-images/ace_of_hearts.png"
  | Hearts, Two -> "card-images/2_of_hearts.png"
  | Hearts, Three -> "card-images/3_of_hearts.png"
  | Hearts, Four -> "card-images/4_of_hearts.png"
  | Hearts, Five -> "card-images/5_of_hearts.png"
  | Hearts, Six -> "card-images/6_of_hearts.png"
  | Hearts, Seven -> "card-images/7_of_hearts.png"
  | Hearts, Eight -> "card-images/8_of_hearts.png"
  | Hearts, Nine -> "card-images/9_of_hearts.png"
  | Hearts, Ten -> "card-images/10_of_hearts.png"
  | Hearts, Jack -> "card-images/jack_of_hearts.png"
  | Hearts, Queen -> "card-images/queen_of_hearts.png"
  | Hearts, King -> "card-images/king_of_hearts.png"
  | Spades, Ace -> "card-images/ace_of_spades.png"
  | Spades, Two -> "card-images/2_of_spades.png"
  | Spades, Three -> "card-images/3_of_spades.png"
  | Spades, Four -> "card-images/4_of_spades.png"
  | Spades, Five -> "card-images/5_of_spades.png"
  | Spades, Six -> "card-images/6_of_spades.png"
  | Spades, Seven -> "card-images/7_of_spades.png"
  | Spades, Eight -> "card-images/8_of_spades.png"
  | Spades, Nine -> "card-images/9_of_spades.png"
  | Spades, Ten -> "card-images/10_of_spades.png"
  | Spades, Jack -> "card-images/jack_of_spades.png"
  | Spades, Queen -> "card-images/queen_of_spades.png"
  | Spades, King -> "card-images/king_of_spades.png"

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
