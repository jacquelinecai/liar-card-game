(* [number] represents the card number in a deck of cards*)
type number =
  | Number of int
  | Jack
  | Queen
  | King

(* [suit] represents the suit in a deck of cards*)
type suit =
  | Diamonds
  | Clubs
  | Hearts
  | Spades

(* [card] contains both the suit and number *)
type card = suit * number

exception Invalid

val card_list : card list
(* [card_list] contains all [cards] that are in a deck, excluding the Joker
   cards *)

val suit_match : suit -> string
(* [suit_match c] matches c to the suit's string representation *)

val number_match : number -> string
(* [number_match c] matches c to the number's string representation *)

val card_to_string : card -> string
(* [card_to_string c] matches c to the card's string representation *)

val shuffle : card list -> card list
(* Implementation based on the Fisher-Yates Shuffling Algorithm:
   https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle *)

val string_to_card : string -> card option
val stringlist_to_card_list : string list -> card option list
val cardlist_to_string : card list -> string

val contains : card -> card list -> bool
(** [contains c cl] returns true if the card list contains the card, else
    returns false *)

val valid : card option list -> card list -> bool
(** [valid cl yourCards] returns true if the card list has all valid cards and
    all the cards from cl are in yourCards, else returns false *)

val toCardList : card option list -> card list
(** [toCardList cl] returns the card option list into a card list. Precondition:
    all the cards must be valid *)
