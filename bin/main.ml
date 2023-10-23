open Liargame.Hand
open Liargame.Card

let start () =
  print_endline "\n\nWelcome to the Liar Card Game.\n";

  let y = ref false in
  while not !y do
    print_endline "Press \"s\" to start the game: ";
    let x = read_line () in
    if x = "s" then y := true
  done;

  print_endline ("\nHere are your cards: " ^ deck_to_string !player1_hand)

let exit () =
  let quit = ref None in
  while !quit = None do
    print_endline "Please type \"exit\" to exit the game.";
    let x = read_line () in
    if x = "exit" then quit := Some 1;
    print_endline
      "\n\
       You have successfully quit the Liar Card game. Thank you for playing! \
       Exiting the session...\n"
  done;
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
      "\n\
       Choose a card type you are claiming to have. Possible options include: \n\
      \    Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, \
       Queen, King";
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
      How many cards would you like to put down? You may place down up to 4 \
      cards. \n\
     \ \n\
     \    Here are your current cards: "
    ^ deck_to_string !player1_hand);
  let y = ref None in
  while !y = None do
    let x = String.lowercase_ascii (read_line ()) in
    if x = "1" then
      let () =
        print_endline
          "What is the card you would like to place?\n\
           Please type it in the format \"NumberSuit\" \n\
           where \"Number\" can take values \"A, 2, 3, 4, 5, 6, 7, 8, 9, 10, \
           J, Q, K\" \n\
           and \"Suit\" can take values \"D, C, H, S\"."
      in
      let c = read_line () |> String.uppercase_ascii |> string_to_card in
      y := c
    else if x = "2" then
      let () =
        print_endline
          "What are the two cards you would like to place?\n\
           Please type it in the format \"NumberSuit\" \n\
           where \"Number\" can take values \"A, 2, 3, 4, 5, 6, 7, 8, 9, 10, \
           J, Q, K\" \n\
           and \"Suit\" can take values \"D, C, H, S\" and separate each card \
           by spaces."
      in
      let c = read_line () |> String.uppercase_ascii |> string_to_card in
      y := c
    else if x = "3" then
      let () =
        print_endline
          "What are the three cards you would like to place?\n\
           Please type it in the format \"NumberSuit\" \n\
           where \"Number\" can take values \"A, 2, 3, 4, 5, 6, 7, 8, 9, 10, \
           J, Q, K\" \n\
           and \"Suit\" can take values \"D, C, H, S\" and separate each card \
           by spaces."
      in
      let c = read_line () |> String.uppercase_ascii |> string_to_card in
      y := c
    else if x = "4" then
      let () =
        print_endline
          "What are the four cards you would like to place?\n\
           Please type it in the format \"NumberSuit\" \n\
           where \"Number\" can take values \"A, 2, 3, 4, 5, 6, 7, 8, 9, 10, \
           J, Q, K\" \n\
           and \"Suit\" can take values \"D, C, H, S\" and separate each card \
           by spaces."
      in
      let c = read_line () |> String.uppercase_ascii |> string_to_card in
      y := c
    else if x = "0" then y := Some (Diamonds, Number 1)
  done

let () =
  start ();
  choose_card_type ();
  choose_cards ();
  exit ()
