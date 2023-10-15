type number =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King

type suit =
  | Diamonds
  | Clubs
  | Hearts
  | Spades

type card = suit * number

val fileValue : card -> string
val card_list : card list
val shuffle : card list -> card list
