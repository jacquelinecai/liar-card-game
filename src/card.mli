(** Determines the card setup for the game *)

(** [number] represents the card number in a deck of cards*)
type number =
  | Number of int
  | Jack
  | Queen
  | King

(** [suit] represents the suit in a deck of cards*)
type suit =
  | Diamonds
  | Clubs
  | Hearts
  | Spades

type card = suit * number
(** [card] contains both the suit and number *)

exception InvalidCard
(** [InvalidCard] is raised if a card is invalid *)

val card_list : card list
(** [card_list] contains all [cards] that are present in a standard 52-card deck *)

val suit_match : suit -> string
(** [suit_match s] returns the string representation of suit [s] *)

val number_match : number -> string
(** [number_match n] returns the string representation of number [n] *)

val card_to_string : card -> string
(** [card_to_string c] returns the string representation of card [c] *)

val shuffle : card list -> card list
(** [shuffle cl] shuffles the deck using the Fisher-Yates Shuffling Algorithm:
    https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle *)

val string_to_card : string -> card option
(** [string_to_card s] returns [Some c] if [s] can be represented as a valid
    card [c]. Otherwise, return [None] *)

val stringlist_to_card_list : string list -> card option list
(** [stringlist_to_card_list lst] returns a list of card representations of the
    elements of [lst] *)

val cardlist_to_string : card list -> string
(** [cardlist_to_string cl] returns the string representation of cards in [cl] *)

val contains : card -> card list -> bool
(** [contains c cl] returns [true] if [cl] contains the card, else returns
    [false] *)

val valid : card option list -> card list -> bool
(** [valid cl yourCards] returns [true] if the card list has all valid cards and
    all the cards from [cl] are in [yourCards], else returns [false] *)

val toCardList : card option list -> card list
(** [toCardList cl] returns the card option list into a card list. Precondition:
    all the cards must be valid *)
