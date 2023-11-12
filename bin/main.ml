open Liargame.Hand
open Liargame.Card
open Liargame.Round
open Liargame.Game

let start () =
  print_endline "\n\nWelcome to the Liar Card Game!\n";

  let y = ref false in
  while not !y do
    print_endline "Press \"s\" to start the game: ";
    let x = read_line () in
    if x = "s" then y := true
  done;

  print_endline ("\nIn this game you will be Player 1. Here are your cards: " ^ deck_to_string !player1_hand)

let exit () =
  let quit = ref None in
  while !quit = None do
    print_endline "\nPlease type \"exit\" to exit the game.";
    let x = read_line () in
    if x = "exit" then (
      quit := Some 1;
      print_endline
        "\n\
         You have successfully quit the Liar Card game. Thank you for playing! \
         Exiting the session...\n")
  done;
  Stdlib.exit 0

let winner () =
  let status =
    card_status !player1_hand !player2_hand !player3_hand !player4_hand
  in
  let win = check_winner status in
  match win with
  | 0 -> print_string ""
  | _ ->
      print_endline
        ("\nPlayer " ^ string_of_int win
       ^ " has gotten rid of their cards and wins the game!")

let round = ref 0
let card_type = ref None
let card = ref None
let curr_player = ref (order())
let current_round () = print_endline ("For this round the card will be " ^ (card_round ()))
let player_order () = print_endline ((!curr_player)^ "'s turn.")
let match_card_type c =
  match c with
  | None -> None
  | Some s -> s


  let num_cards_prompt () =
    print_endline
      ("\n\n\
        How many cards would you like to put down? You may place down up to 4 \
        cards. \n(Note: for MS2, we're only supporting one card at this time.) \n\
       \ \n\
       \    Here are your current cards: "
      ^ deck_to_string !player1_hand)
  
  
  let choose_cards () =
    num_cards_prompt ();
    let y = ref None in
    while !y = None do
      let x = String.lowercase_ascii (read_line ()) in
      if x = "1" then (
        let z = ref None in
        while !z = None do
          let () =
            print_endline
              "\n\
               What is the card you would like to place?\n\
               Please type it in the format \"NumberSuit\" \n\
               where \"Number\" can take values \"A, 2, 3, 4, 5, 6, 7, 8, 9, 10, \
               J, Q, K\" \n\
               and \"Suit\" can take values \"D, C, H, S\"."
          in
          let c = read_line () |> String.uppercase_ascii |> string_to_card in
          if c <> None then z := Some 1;
          y := c
        done;
        print_endline
          ("You have chosen to place down "
          ^ (Option.get !y |> card_to_string)
          ^ " and you claimed to place down one "
          ^ String.sub (card_to_string (Diamonds, Option.get !card_type)) 0 1))
      else if x = "2" then (
        let z = ref None in
        while !z = None do
          let () =
            print_endline
              "\n\
               What are the two cards you would like to place?\n\
               Please type it in the format \"NumberSuit\" \n\
               where \"Number\" can take values \"A, 2, 3, 4, 5, 6, 7, 8, 9, 10, \
               J, Q, K\" \n\
               and \"Suit\" can take values \"D, C, H, S\" and separate each \
               card by spaces.\n\
               (Note: for MS2, we're only supporting one card at this time)"
          in
          let c = read_line () |> String.uppercase_ascii |> string_to_card in
          if c <> None then z := Some 1;
          y := c
        done;
        print_endline
          ("You have chosen to place down "
          ^ (Option.get !y |> card_to_string)
          ^ " and you claimed to place down one "
          ^ String.sub (card_to_string (Diamonds, Option.get !card_type)) 0 1))
      else if x = "3" then (
        let z = ref None in
        while !z = None do
          let () =
            print_endline
              "\n\
               What are the three cards you would like to place?\n\
               Please type it in the format \"NumberSuit\" \n\
               where \"Number\" can take values \"A, 2, 3, 4, 5, 6, 7, 8, 9, 10, \
               J, Q, K\" \n\
               and \"Suit\" can take values \"D, C, H, S\" and separate each \
               card by spaces.\n\
               (Note: for MS2, we're only supporting one card at this time)"
          in
          let c = read_line () |> String.uppercase_ascii |> string_to_card in
          if c <> None then z := Some 1;
          y := c
        done;
        print_endline
          ("You have chosen to place down "
          ^ (Option.get !y |> card_to_string)
          ^ " and you claimed to place down one "
          ^ String.sub (card_to_string (Diamonds, Option.get !card_type)) 0 1))
      else if x = "4" then (
        let z = ref None in
        while !z = None do
          let () =
            print_endline
              "\n\
               What are the four cards you would like to place?\n\
               Please type it in the format \"NumberSuit\" \n\
               where \"Number\" can take values \"A, 2, 3, 4, 5, 6, 7, 8, 9, 10, \
               J, Q, K\" \n\
               and \"Suit\" can take values \"D, C, H, S\" and separate each \
               card by spaces.\n\
               (Note: for MS2, we're only supporting one card at this time)"
          in
          let c = read_line () |> String.uppercase_ascii |> string_to_card in
          if c <> None then z := Some 1;
          y := c
        done;
        print_endline
          ("You have chosen to place down "
          ^ (Option.get !y |> card_to_string)
          ^ " and you claimed to place down one "
          ^ String.sub (card_to_string (Diamonds, Option.get !card_type)) 0 1))
      else if x = "0" then y := Some (Diamonds, Number 1)
      else num_cards_prompt ()
    done;
    card := !y;
    print_endline "test";
    try
      player1_hand := updateDeck (Option.get !card) !player1_hand [];
      print_endline ("\nHere are your cards: " ^ deck_to_string !player1_hand)
    with InvalidCard -> print_endline "You do not have that card!"
  
let next_player () = match !curr_player with 
    |"Player 1"->"Player 2"
    |"Player 2" -> "Player 3"
    |"Player 3"->"Player 4"
    |"Player 4"->"Player 1"

let choose_card_type () =
  let c = ref None in
  while !c = None do
    print_endline
      "\n\
       Choose a card type you are claiming to have. Possible options include: \n\
      \    Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, \
       Queen, King";
    let x = String.lowercase_ascii (read_line ()) in
    if x = "ace" then c := Some (Number 1)
    else if x = "two" then c := Some (Number 2)
    else if x = "three" then c := Some (Number 3)
    else if x = "four" then c := Some (Number 4)
    else if x = "five" then c := Some (Number 5)
    else if x = "six" then c := Some (Number 6)
    else if x = "seven" then c := Some (Number 7)
    else if x = "eight" then c := Some (Number 8)
    else if x = "nine" then c := Some (Number 9)
    else if x = "ten" then c := Some (Number 10)
    else if x = "jack" then c := Some Jack
    else if x = "queen" then c := Some Queen
    else if x = "king" then c := Some King
    else print_endline "That is not a possible card type."
  done;
  let () = choose_cards () in
  card_type := !c 
  let pass_or_play () = let a = ref "" in while !a =""  do 
    print_endline ("You can choose to pass or play a card. Type 'pass' or 'play' to continue."); 
    let x = String.lowercase_ascii (read_line ()) in
    if x = "pass" then a:="pass"
    else if x = "play" then a:="play"
    else print_endline "Please try again. Type 'pass' or 'play' to continue."
  done;
  match !a with 
  |"play"->(choose_card_type ())
  |_->let () = (change_to_pass (!curr_player)) in let ()= curr_player:=(next_player ()) in player_order()

let () =
  start ();
  current_round ();
  player_order ();
  pass_or_play ();
  winner ();
  exit ()
