(** Determines the condition in which the game ends *)

open Card

val card_status :
  card list -> card list -> card list -> card list -> int * int * int * int
(** [card_status p1 p2 p3 p4] checks the number of cards left in each player's
    hands *)

exception InvalidCardAmount
(** [InvalidCardAmount] is raised if multiple players have zero cards *)

val check_winner : int * int * int * int -> int
(** [check_winner cs] checks if any one player has run out of cards and returns
    the winner of the player number [1, 2, 3, 4] *)
