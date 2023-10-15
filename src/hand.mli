type player =
  | Player1
  | Player2
  | Player3
  | Player4

type card = Card.card

val shuffled_deck : card list

val unshuffled_deck : card list
(**unshuffled_deck used mainly for testing*)

val player1_hand : card list
val player2_hand : card list
val player3_hand : card list
val player4_hand : card list
