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
  | Clubs
  | Diamonds
  | Hearts
  | Spades

type card = suit * number
