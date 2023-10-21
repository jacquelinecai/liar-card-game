open Liargame.Hand
open Liargame.Card

let start () =
  print_endline "\n\nWelcome to BS.\n";

  let y = ref false in
  while not !y do
    print_endline "Press s to start the game: ";
    let x = read_line () in
    if x = "s" then y := true
  done;

  print_endline ("Here are your cards: " ^ deck_to_string !player1_hand)

let exit () =
  print_endline "Exit Prompt";
  let x = read_line () in
  if x = "exit" then
    print_endline
      "\n\
       You have successfully quit the Liar Card game. Thank you for playing! \
       Exiting the session...\n";
  Stdlib.exit 0

let round = ref 0
let card_type = ref None
let card = ref None

let match_card_type c =
  match c with
  | None -> None
  | Some s -> s

let choose_card_type () =
  let c = ref None in
  while !c = None do
    print_endline
      "Choose a card type you are claiming to have: possible options include: \n\
      \    Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, \
       Queen, or King";
    let x = String.lowercase_ascii (read_line ()) in
    if x = "ace" then c := Some (ref (Number 1))
    else if x = "two" then c := Some (ref (Number 2))
    else if x = "three" then c := Some (ref (Number 3))
    else if x = "four" then c := Some (ref (Number 4))
    else if x = "five" then c := Some (ref (Number 5))
    else if x = "six" then c := Some (ref (Number 6))
    else if x = "seven" then c := Some (ref (Number 7))
    else if x = "eight" then c := Some (ref (Number 8))
    else if x = "nine" then c := Some (ref (Number 9))
    else if x = "ten" then c := Some (ref (Number 10))
    else if x = "jack" then c := Some (ref Jack)
    else if x = "queen" then c := Some (ref Queen)
    else if x = "king" then c := Some (ref King)
    else print_endline "That is not a possible card type."
  done;
  card_type := !c

let choose_cards () =
  print_endline
    ("\n\n\
      How many cards would you like to put down? \n\
     \ \n\
     \    Here are your current cards: "
    ^ deck_to_string !player1_hand);
  let y = ref None in
  while !y = None do
    let x = String.lowercase_ascii (read_line ()) in
    if x = "1" then
      let () = print_endline "What is the card you would like to place?" in
      let c = read_line () |> String.uppercase_ascii |> string_to_card in
      y := c
    else if x = "2" then y := None
    else if x = "3" then y := None
    else if x = "4" then y := None
  done

let () =
  start ();
  choose_card_type ();
  print_endline "Great. This round everyone will be claiming to have ";
  choose_cards ();
  exit ()
