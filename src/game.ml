(** [card_status p1 p2 p3 p4] checks the number of cards left in each player's
    hands *)
let card_status p1 p2 p3 p4 =
  (List.length p1, List.length p2, List.length p3, List.length p4)

exception InvalidCardAmount

(** [check_invalid x y z n] checks if there are multiple players with no cards.
    If so, InvalidCardAmount is raised, else the player number is returned *)
let check_invalid x y z n =
  if x = 0 || y = 0 || z = 0 then raise InvalidCardAmount else n

(** [check_winner cs] checks if any one player has run out of cards and returns
    the winner of the player number (1,2,3,4) *)
let check_winner c =
  match c with
  | 0, x, y, z -> check_invalid x y z 1
  | x, 0, y, z -> check_invalid x y z 2
  | x, y, 0, z -> check_invalid x y z 3
  | x, y, z, 0 -> check_invalid x y z 4
  | _ -> 0
