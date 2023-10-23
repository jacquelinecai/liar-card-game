type player =
  | Player1
  | Player2
  | Player3
  | Player4

type card = Card.card

val shuffled_deck : card list

val unshuffled_deck : card list
(**unshuffled_deck used mainly for testing*)

val assign : int -> int -> card list -> card list -> card list
val order : card list -> card list
val deck_to_string : card list -> string
val player1_hand : card list ref
val player2_hand : card list ref
val player3_hand : card list ref
val player4_hand : card list ref
val contains : card -> card list -> bool
val updateDeck : card -> card list -> card list -> card list
