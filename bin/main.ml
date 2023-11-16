open Liargame.Card
open Liargame.Game
open Liargame.Table
open Liargame.Hand
open Liargame.Round

let main_player =
  let y = Random.int 4 in
  print_endline (string_of_int y);
  match y with
  | 0 -> "Player 1"
  | 1 -> "Player 2"
  | 2 -> "Player 3"
  | _ -> "Player 4"

let start () =
  let y = ref false in
  while not !y do
    print_endline "\nPress \"s\" to start the game: ";
    print_string "> ";
    let x = read_line () in
    if x = "s" then y := true
  done;
  print_endline
    ("\nIn this game you will be " ^ main_player ^ ". Here are your cards: "
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
let curr_player = ref "Player 1"
let bs_curr_player = ref "Player 2"
let table = empty_table

let current_round () =
  print_endline
    ("For this round the card will be "
    ^ (Option.get !card_type |> number_match))

let player_order () = print_endline (!curr_player ^ "'s turn.")
let bs_player_callout () = print_endline (!bs_curr_player ^ "'s callout turn")

let num_cards_prompt () =
  print_endline
    ("\n\n\
      How many cards would you like to put down? You may place down up to 4 \
      cards.\n\
      Here are your current cards: "
    ^ deck_to_string !player1_hand);
  print_string "> "

let choose_cards () =
  let () =
    print_endline
      ("\n\n\
       \        What cards would you like to place? \n\
       \ \n\
       \ Example: 4D-4C\n\
       \        Here are your current cards: "
      ^ deck_to_string !player1_hand)
  in
  let y = ref None in
  while !y = None do
    let x = String.uppercase_ascii (read_line ()) in
    let cards_placed = String.split_on_char '-' x |> stringlist_to_card_list in
    if valid cards_placed !player1_hand then y := Some cards_placed
    else print_endline "One or more of your cards are not valid. Try again."
  done;
  let amt = List.length (Option.get !y) in
  let () =
    print_endline
      ("You have chosen to place down " ^ string_of_int amt ^ " cards: "
      ^ (Option.get !y |> toCardList |> cardlist_to_string)
      ^ " and you have claimed to place down " ^ string_of_int amt ^ " "
      ^ String.sub (card_to_string (Diamonds, Option.get !card_type)) 0 1)
  in
  adding_cards_to_table table (Option.get !y |> toCardList);
  player1_hand :=
    updateDeckWithCardList (Option.get !y |> toCardList) !player1_hand;
  print_endline
    ("\nHere are your current cards: " ^ (order !player1_hand |> deck_to_string))

let next_player () =
  match !curr_player with
  | "Player 1" -> "Player 2"
  | "Player 2" -> "Player 3"
  | "Player 3" -> "Player 4"
  | "Player 4" -> "Player 1"
  | _ -> ""

let next_bs_player =
  if !bs_curr_player = !curr_player then "Done"
  else
    match !bs_curr_player with
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

let bot_cards () =
  let curr_player_cards =
    match !curr_player with
    | "Player 2" -> player2_hand
    | "Player 3" -> player3_hand
    | "Player 4" -> player4_hand
    | _ -> failwith "Impossible"
  in
  let length = List.length !curr_player_cards in
  let index = Random.int length in
  List.nth !curr_player_cards index

let bot_actions () =
  let play =
    match !curr_player with
    | "Player 2" ->
        bot_play (Option.get !card_type) (table_size table) !player2_hand
    | "Player 3" ->
        bot_play (Option.get !card_type) (table_size table) !player3_hand
    | "Player 4" ->
        bot_play (Option.get !card_type) (table_size table) !player4_hand
    | _ -> None
  in
  match play with
  | None -> change_to_pass !curr_player
  | Some x ->
      (match !curr_player with
      | "Player 2" ->
          player2_hand := updateDeckWithCardList x !player2_hand;
          adding_cards_to_table table x
      | "Player 3" ->
          player3_hand := updateDeckWithCardList x !player3_hand;
          adding_cards_to_table table x
      | "Player 4" ->
          player4_hand := updateDeckWithCardList x !player4_hand;
          adding_cards_to_table table x
      | _ -> ());
      print_endline
        (!curr_player ^ " claimed they placed down "
        ^ (List.length x |> string_of_int)
        ^ " "
        ^ number_match (Option.get !card_type))

let check_round () =
  if is_end p then
    match !curr_player with
    | "Player 1" -> choose_card_type ()
    | _ -> card_type := Some (snd (bot_cards ()))

let pass_chosen () =
  let () = change_to_pass !curr_player in
  let () = curr_player := next_player () in
  let () = player_order () in
  check_round ()

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
      let () =
        curr_player := next_player ();
        bs_curr_player := next_bs_player
      in
      player_order ();
      if !card_type = None then card_type := Some (snd (bot_cards ()));
      bot_actions ()

let match_player_with_hand player =
  match player with
  | "Player 1" -> player1_hand
  | "Player 2" -> player2_hand
  | "Player 3" -> player3_hand
  | "Player 4" -> player4_hand
  | _ -> failwith "will not happen"

let callout () =
  while !bs_curr_player <> "Done" && next_bs_player <> "Done" do
    if !bs_curr_player = "Player 1" then begin
      print_endline "Do you want to call BS? Please input yes or no.";
      let response = read_line () |> String.lowercase_ascii in
      (if response = "yes" then
         let table_list = peek_at_table table in
         match table_list with
         | (suit, number) :: t ->
             if number = Option.get !card_type then (
               print_endline
                 ("Here are the cards in the table: "
                 ^ deck_to_string (peek_at_table table |> order)
                 ^ ". The player was not lying.");
               match_player_with_hand !bs_curr_player
               := table_list @ !(match_player_with_hand !bs_curr_player))
             else (
               print_endline
                 ("Here are the cards in the table: "
                 ^ deck_to_string (peek_at_table table |> order)
                 ^ ". The player was lying.");
               match_player_with_hand !curr_player
               := table_list @ !(match_player_with_hand !curr_player))
         | [] -> failwith "should not happen"
       else if next_bs_player = "Done" then
         let () = curr_player := next_player () in
         player_order ()
       else
         let () = bs_curr_player := next_bs_player in
         bs_player_callout ());
      print_endline ("\nHere are your cards: " ^ deck_to_string !player1_hand)
    end
    else
      let bot_bs = Random.bool () in
      if bot_bs then begin
        print_endline (!bs_curr_player ^ " calls BS.");
        let table_list = peek_at_table table in
        match table_list with
        | (suit, number) :: t ->
            if number = Option.get !card_type then (
              print_endline
                ("Here are the cards in the table: "
                ^ deck_to_string (peek_at_table table |> order)
                ^ ". The player was not lying.");
              let () =
                match_player_with_hand !bs_curr_player
                := table_list @ !(match_player_with_hand !bs_curr_player)
              in
              bs_curr_player := "Done")
            else (
              print_endline
                ("Here are the cards in the table: "
                ^ deck_to_string (peek_at_table table |> order)
                ^ ". The player was lying.");
              let () =
                match_player_with_hand !curr_player
                := table_list @ !(match_player_with_hand !curr_player)
              in
              bs_curr_player := "Done")
        | [] -> failwith "should not happen"
      end
      else
        let () = bs_curr_player := next_bs_player in
        bs_player_callout ()
  done;
  print_endline !bs_curr_player

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
     gets rid of their cards first wins!"
(* start (); main_prompt () *)

let () =
  main ();
  start ();
  player_order ();
  pass_or_play ();
  callout ();
  winner ();
  exit ()
