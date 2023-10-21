type number =
  | Number of int
  | Jack
  | Queen
  | King

type suit =
  | Diamonds
  | Clubs
  | Hearts
  | Spades

type card = suit * number

val card_list : card list
val card_to_string : card -> string
val shuffle : card list -> card list
val string_to_card : string -> card option
