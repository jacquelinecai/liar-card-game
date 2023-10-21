open Liargame.Hand
open Liargame.Card

let () =
  print_endline "\n\nWelcome to BS.\n";

  let y = ref false in
  while not !y do
    print_endline "Press s to start the game: ";
    let x = read_line () in
    if x = "s" then y := true
  done
;;

print_endline ("Here are the cards: " ^ deck_to_string player1_hand)

let round = ref 0
let card_type = ref King

let () =
  print_endline
    "\n\n\
     Choose a card type you claim to have: possible options include: \n\
    \    Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, \
     Queen, \n\
    \    or King.\n\
    \    \n\
    \   \n";
  let y = ref false in
  let z = ref King in
  while not !y do
    print_endline "Press s to start the game: ";
    let x = String.lowercase_ascii (read_line ()) in
    if x = "ace" then y := (true) z := Ace
    else if x = "two" then y := true
    else if x = "three" then y := true
    else if x = "four" then y := true
    else if x = "five" then y := true
    else if x = "six" then y := true
    else if x = "seven" then y := true
    else if x = "eight" then y := true
    else if x = "nine" then y := true
    else if x = "ten" then y := true
    else if x = "jack" then y := true
    else if x = "queen" then y := true
    else if x = "king" then y := true
  done
