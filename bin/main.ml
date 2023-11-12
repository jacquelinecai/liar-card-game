open Liargame.Card
open Liargame.Game
open Liargame.Table
open Liargame.Hand
open Liargame.Round

let start () =
  let y = ref false in
  while not !y do
    print_endline "\nPress \"s\" to start the game: ";
    print_string "> ";
    let x = read_line () in
    if x = "s" then y := true
  done;

  print_endline
    ("\nIn this game you will be Player 1. Here are your cards: \n"
     ^ deck_to_string !player1_hand)

let exit () =
  let quit = ref None in
  while !quit = None do
    print_endline "\nPlease type \"exit\" to exit the game.";
    print_string "> ";
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
       ^ " has gotten rid of their cards and wins the game!");
    exit ()

let round = ref 0
let card_type = ref None
let card = ref None
let curr_player = ref (player_order ())
let table = empty_table

let current_round () =
  print_endline ("For this round the card will be " ^ card_round ())

let player_order () = print_endline (!curr_player ^ "'s turn.")

let num_cards_prompt () =
  print_endline
    ("\n\n\
      How many cards would you like to put down? You may place down up to 4 \
      cards. \n\
      (Note: for MS2, we're only supporting one card at this time.) \n\
     \ \n\
     \    Here are your current cards: "
     ^ deck_to_string !player1_hand);
  print_string "> "

let choose_cards () =
  let () = print_endline ("\n
        What cards would you like to place? \n\
                          \ \n\ Example: 4D-4C
        Here are your current cards: "
                          ^ deck_to_string !player1_hand) in
  let y = ref None in
  while !y = None do
    let x = String.uppercase_ascii (read_line ()) in
    let cards_placed = ((String.split_on_char ('-') x) |> stringlist_to_card_list) in
    if (valid cards_placed !player1_hand) then y := Some cards_placed 
    else print_endline ("One or more of your cards are not valid. Try again.")
  done;
  let amt = (List.length (Option.get !y)) in
  let () = print_endline ("You have chosen to place down " ^ string_of_int amt ^ " cards: "^
                          (Option.get !y |> toCardList |> cardlist_to_string )^ " and you have claimed to place down "
                          ^ string_of_int amt ^ " "^ String.sub (card_to_string (Diamonds, Option.get !card_type)) 0 1) in 
  adding_cards_to_table table (Option.get !y |> toCardList);
  player1_hand := updateDeckWithCardList (Option.get !y |> toCardList) !player1_hand;
  print_endline
    ("\nHere are your current cards: " ^ (order !player1_hand |> deck_to_string))


let next_player () =
  match !curr_player with
  | "Player 1" -> "Player 2"
  | "Player 2" -> "Player 3"
  | "Player 3" -> "Player 4"
  | "Player 4" -> "Player 1"
  | _ -> ""

let choose_card_type () =
  let c = ref None in
  while !c = None do
    print_endline
      "\n\
       Choose a card type you are claiming to have. Possible options include: \n\
      \    A, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K";
    print_string "> ";
    let x = String.lowercase_ascii (read_line ()) in
    try c := Some (Number (int_of_string x))
    with Failure s ->
      if x = "a" then c := Some (Number 1)
      else if x = "j" then c := Some Jack
      else if x = "q" then c := Some Queen
      else if x = "k" then c := Some King
      else print_endline "That is not a possible card type."
  done;
  card_type := !c;
  current_round ();
  choose_cards ()

let pass_or_play st =
  let a = ref "" in
  while !a = "" do
    print_endline
      "You can choose to pass or play a card. Type 'pass' or 'play' to \
       continue.";
    print_string "> ";
    let x = String.lowercase_ascii (read_line ()) in
    if x = "pass" then a := "pass"
    else if x = "play" then a := "play"
    else print_endline "Please try again. Type 'pass' or 'play' to continue."
  done;
  match !a with
  | "play" -> choose_card_type ()
  | _ ->
    let () = change_to_pass !curr_player in
    let () = curr_player := next_player () in
    player_order ()

(* let callout () = print_endline "Do you want to call BS? Please input yes or
   no." let response = read_line () |> String.lowercase_ascii in if response =
   "yes" then *)

let rec main_prompt st = pass_or_play st |> main_prompt

let main () =
  print_endline "\n\nWelcome to the Liar Card Game!\n";
  print_endline
    "You will be playing against 3 other bots. Here's the rules for this game: ";
  print_endline
    "1) You start off the game. At the start of each round, the selected \
     player will choose the card type they claim to place down";
  print_endline
    "2) Each player will have the option of passing the round or placing down \
     up to 4 cards";
  print_endline
    "3) If at any point during the game, you believe that the other players \
     have lied in their card placement, instantiate the BS callout. If you're \
     correct in your assumption, that player will collect all the cards on the \
     table. If you're incorrect in your assumption, you must collect all the \
     cards on the table.";
  print_endline
    "4) Each round ends when all players decide to pass or someone has \
     collected all the cards on the table. If all players choose to pass, the \
     current cards on the table will be discarded.";
  print_endline
    "5) Each subsequent round starts with the next player if everyone passes \
     or the player who was correct in the BS callout.";
  print_endline
    "6) Continue battling your way through the liar game and the player who \
     gets rid of their cards first wins!";
  start ();
  main_prompt ()

let () =
  start ();
  player_order ();
  pass_or_play ();
  (* main (); *)
  winner ();
  exit ()
