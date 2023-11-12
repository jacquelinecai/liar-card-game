open Card

val card_status :
  card list -> card list -> card list -> card list -> int * int * int * int

exception InvalidCardAmount

val check_winner : int * int * int * int -> int
