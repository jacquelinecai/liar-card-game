(** Determines the cards in a player's hands *)

open Card

(** [player] represents each of the 4 players in the game *)
type player =
  | Player1
  | Player2
  | Player3
  | Player4

val shuffled_deck : card list
(** [shuffled_deck] is the shuffled deck using the shuffling algorithm *)

val unshuffled_deck : card list
(** [unshuffled_deck] used mainly for testing*)

val assign : int -> int -> card list -> card list -> card list
(** [assign a b deck acc] returns a new card list from index a to b of [deck] *)

val order : card list -> card list
(** [order deck] returns [deck] in order based on number and suit *)

val order_num : number list -> number list
(** [order_num nl] returns [nl] in order where [nl] represents a number list *)

val deck_to_string : card list -> string
(** [deck_to_string deck] returns a string of each card in [deck] *)

val num_list_to_string : number list -> string
(** [num_list_to_string nl] returns a string of each number in [nl] *)

val player1_hand : card list ref
(** [player1_hand] represents player 1's cards *)

val player2_hand : card list ref
(** [player2_hand] represents player 2's cards *)

val player3_hand : card list ref
(** [player3_hand] represents player 3's cards *)

val player4_hand : card list ref
(** [player4_hand] represents player 4's cards *)

val player_hand_size : int -> int
(** [player_hand_size n] returns the size of player [n]'s card list *)

val containsNum : number -> card list -> bool
(** [containsNum n cl] returns [true] if [cl] contains the card number [n], else
    returns [false], where [c1] represents a card list *)

val numCards : number -> card list -> int -> int
(** [numCards n cl acc] returns the number of cards in [cl] that have a number
    [n] *)

val nCards : number -> int -> card list -> card list -> card list
(** [nCards n amt cl acc] returns a card list with [amt] number of number [n]
    cards in [cl]. Requires that [amt < numCards n cl acc] *)

val getRandCards : int list -> int -> card list -> card list -> card list
(** [getRandCards lst idx deck acc] returns the cards of [deck] from the indices
    in [lst] *)

val firstNCards : card list -> int -> card list
(** [firstNCards deck n] returns the first n cards in [deck] *)

val updateDeck : card -> card list -> card list -> card list
(**[updateDeck c cl acc] returns [cl] without the card [c] *)

val updateDeckWithCardList : card list -> card list -> card list
(**[updateDeckWithCardList clr cl] returns [cl] without the cards from [clr],
   where [c1r] and [cl] are card lists *)
