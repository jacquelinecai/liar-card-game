open Card

type player =
  | Player1
  | Player2
  | Player3
  | Player4

val shuffled_deck : card list

val unshuffled_deck : card list
(**unshuffled_deck used mainly for testing*)

val assign : int -> int -> card list -> card list -> card list
(** Returns a new card list from an existing card list. *)

val order : card list -> card list
(** Returns the card list in order based on number and suit. *)

val order_num : number list -> number list
(** Returns a number list in order. *)

val deck_to_string : card list -> string
(** Returns a string of each card in a deck. *)

val num_list_to_string : number list -> string
(** Returns a string of each number in a number list. *)

val player1_hand : card list ref
(** Player 1's cards *)

val player2_hand : card list ref
(** Player 2's cards *)

val player3_hand : card list ref
(** Player 3's cards *)

val player4_hand : card list ref
(** Player 4's cards *)

val player_hand_size : int -> int
(** Returns the size of a player's card list. *)

val containsNum : number -> card list -> bool
(** Returns true if the card list contains the card number, else returns false. *)

val numCards : number -> card list -> int -> int
(** Returns the number of cards in a card list that has a particular number. *)

val nCards : number -> int -> card list -> card list -> card list
val getRandCards : int list -> int -> card list -> card list -> card list
val firstNCards : card list -> int -> card list

exception InvalidCard

val updateDeck : card -> card list -> card list -> card list
val updateDeckWithCardList : card list -> card list -> card list
