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
val stringlist_to_card_list : string list -> card option list
val cardlist_to_string : card list -> string

val valid : card option list -> bool
val toCardList: card option list -> card list
