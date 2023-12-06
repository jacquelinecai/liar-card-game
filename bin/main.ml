open Liargame.Card
open Liargame.Game
open Liargame.Table
open Liargame.Hand
open Liargame.Round

let rec escape () =
  let quit = ref None in
  while !quit = None do
    print_endline
      "This will close your game. All progress will be lost. Do you wish to \
       continue quitting (y/n)";
    print_string "> ";
    let x = read_line () in
    match x with
    | "y" ->
        quit := Some 1;
        print_endline
          "\n\
           You have successfully quit the Liar Card game. Thank you for joining";
        Stdlib.exit 0
    | "n" -> quit := Some 1
    | _ ->
        print_endline "Please enter 'y' or 'n'.";
        escape ()
  done;
  ()

let main_player =
  let () = Random.self_init () in
  let y = Random.int 4 in
  print_endline (string_of_int y);
  match y with
  | 0 -> "Player 1"
  | 1 -> "Player 2"
  | 2 -> "Player 3"
  | _ -> "Player 4"

let main = main_player

let main_player_cards =
  match main with
  | "Player 1" -> player1_hand
  | "Player 2" -> player2_hand
  | "Player 3" -> player3_hand
  | "Player 4" -> player4_hand
  | _ -> failwith "impossible"

let start () =
  let y = ref false in
  while not !y do
    print_endline
      "\nPress \"s\" to start the game or \"e\" to escape the game: ";
    print_string "> ";
    let x = read_line () in
    if x = "s" then y := true else if x = "e" then escape ()
  done;
  print_endline
    ("\nIn this game you will be " ^ main ^ ". Here are your cards: "
    ^ (!main_player_cards |> order |> deck_to_string))

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

let winner_status = ref false

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
      winner_status := true;
      exit ()

let round = ref 0
let card_type = ref None
let curr_player = ref "Player 1"
let bs_curr_player = ref "Player 2"
let table = empty_table

let current_round () =
  print_endline
    ("For this round the card will be "
    ^ (Option.get !card_type |> number_match))

let player_order () = print_endline (!curr_player ^ "'s turn.")
let bs_player_callout () = print_endline (!bs_curr_player ^ "'s callout turn")
let bs_pass_curr_player p = change_to_pass p bs_pass

let choose_cards () =
  let () =
    print_endline
      ("\n\n\
        What cards would you like to place? \n\
       \ Example: 4D-4C\n\
        Current cards: "
      ^ deck_to_string (order !main_player_cards));
    print_endline
      ("Suggested play: "
      ^
      let suggested =
        suggested_play (Option.get !card_type) (table_size table)
          !main_player_cards
      in
      match suggested with
      | None -> failwith "cannot pass"
      | Some x -> order x |> deck_to_string);
    print_string "> "
  in
  let y = ref None in
  while !y = None do
    let x = String.uppercase_ascii (read_line ()) in
    let cards_placed = String.split_on_char '-' x |> stringlist_to_card_list in
    if valid cards_placed !main_player_cards then y := Some cards_placed
    else print_endline "One or more of your cards are not valid. Try again."
  done;
  let amt = List.length (Option.get !y) in
  let () =
    print_endline
      ("You have chosen to place down " ^ string_of_int amt ^ " cards: "
      ^ (Option.get !y |> toCardList |> cardlist_to_string)
      ^ " and you have claimed to place down " ^ string_of_int amt ^ " "
      ^ number_match (Option.get !card_type))
  in
  adding_cards_to_table table (Option.get !y |> toCardList);
  main_player_cards :=
    updateDeckWithCardList (Option.get !y |> toCardList) !main_player_cards;
  print_endline
    ("\nHere are your current cards: "
    ^ (order !main_player_cards |> deck_to_string))

let next_player () =
  match !curr_player with
  | "Player 1" -> "Player 2"
  | "Player 2" -> "Player 3"
  | "Player 3" -> "Player 4"
  | "Player 4" -> "Player 1"
  | _ -> failwith "impossible"

let next_bs_player () =
  if !bs_curr_player = !curr_player then "Done"
  else
    match !bs_curr_player with
    | "Player 1" -> "Player 2"
    | "Player 2" -> "Player 3"
    | "Player 3" -> "Player 4"
    | "Player 4" -> "Player 1"
    | _ -> failwith "impossible"

let set_bs_player cp =
  match cp with
  | "Player 1" -> "Player 2"
  | "Player 2" -> "Player 3"
  | "Player 3" -> "Player 4"
  | "Player 4" -> "Player 1"
  | _ -> failwith "impossible"

let choose_card_type () =
  let c = card_type in
  while !c = None do
    print_endline
      "\n\
       Choose a card type you are claiming to have. Possible options include: \n\
      \    A, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K";
    print_endline
      ("Suggested card type: "
      ^
      let suggested = suggested_card_type !main_player_cards in
      suggested |> order_num |> num_list_to_string);
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
    | "Player 1" -> player1_hand
    | "Player 2" -> player2_hand
    | "Player 3" -> player3_hand
    | "Player 4" -> player4_hand
    | _ -> failwith "Impossible"
  in
  let length = List.length !curr_player_cards in
  let index = Random.int length in
  List.nth !curr_player_cards index

let check_round () =
  let x = !curr_player in
  if is_end p then
    match x with
    | main -> choose_card_type ()
    | _ -> card_type := Some (snd (bot_cards ()))
  else ()

let pass_chosen () =
  let () = change_to_pass !curr_player p in
  let () = curr_player := next_player () in
  let () = player_order () in
  check_round ()

let bot_actions () =
  let play =
    match !curr_player with
    | "Player 1" ->
        bot_play (Option.get !card_type) (table_size table) !player1_hand
    | "Player 2" ->
        bot_play (Option.get !card_type) (table_size table) !player2_hand
    | "Player 3" ->
        bot_play (Option.get !card_type) (table_size table) !player3_hand
    | "Player 4" ->
        bot_play (Option.get !card_type) (table_size table) !player4_hand
    | _ -> None
  in
  match play with
  | None ->
      pass_chosen ();
      print_endline (!curr_player ^ " decides to pass.")
  | Some x ->
      (match !curr_player with
      | "Player 1" ->
          player1_hand := updateDeckWithCardList x !player1_hand;
          adding_cards_to_table table x
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
        ^ number_match (Option.get !card_type));
      bs_curr_player := set_bs_player !curr_player

let pass_or_play () =
  start_round bs_pass;
  bs_pass_curr_player !curr_player;
  if !card_type = None then start_round p;
  if !curr_player = main then begin
    let a = ref "" in
    while !a = "" do
      print_endline
        "You can choose to pass or play a card. Type 'pass' or 'play' to \
         continue. (Type 'e' to escape)";
      print_endline
        ("Suggested play: "
        ^
        let suggested =
          if !card_type <> None then
            suggested_play (Option.get !card_type) (table_size table)
              !main_player_cards
          else Some []
        in
        match suggested with
        | None -> "pass"
        | Some x -> "play");
      print_string "> ";
      let x = String.lowercase_ascii (read_line ()) in
      if x = "pass" then a := "pass"
      else if x = "play" then a := "play"
      else if x = "e" then escape ()
      else print_endline "Please try again. Type 'pass' or 'play' to continue."
    done;
    match !a with
    | "play" -> choose_card_type ()
    | _ ->
        let () = change_to_pass !curr_player p in
        let () =
          curr_player := next_player ();
          bs_curr_player := next_bs_player ()
        in
        player_order ();
        if !card_type = None then card_type := Some (snd (bot_cards ()));
        bot_actions ()
  end
  else (
    if !card_type = None then card_type := Some (snd (bot_cards ()));
    bot_actions ())

let match_player_with_hand player =
  match player with
  | "Player 1" -> player1_hand
  | "Player 2" -> player2_hand
  | "Player 3" -> player3_hand
  | "Player 4" -> player4_hand
  | _ -> failwith "impossible"

let callout () =
  if check_pass p !curr_player then ()
  else
    while !bs_curr_player <> "Done" && next_bs_player () <> "Done" do
      if !bs_curr_player = main then begin
        print_endline
          "Do you want to call BS? Please input yes or no. (Or \"e\" to escape \
           the game)";
        print_string "> ";
        let response = read_line () |> String.lowercase_ascii in
        if response = "yes" then (
          let table_list = peek_at_table table in
          (match table_list with
          | (suit, number) :: t ->
              if number = Option.get !card_type then (
                print_endline
                  ("Here are the cards in the table: "
                  ^ deck_to_string (peek_at_table table |> order)
                  ^ ". " ^ !curr_player ^ " was not lying.");
                match_player_with_hand !bs_curr_player
                := table_list @ !(match_player_with_hand !bs_curr_player)
                   |> order;
                bs_curr_player := set_bs_player !curr_player)
              else (
                print_endline
                  ("Here are the cards in the table: "
                  ^ deck_to_string (peek_at_table table |> order)
                  ^ ". " ^ !curr_player ^ " was lying.");
                match_player_with_hand !curr_player
                := table_list @ !(match_player_with_hand !curr_player) |> order;
                curr_player := !bs_curr_player;
                bs_curr_player := set_bs_player !curr_player);
              table.table_cards <- [];
              bs_curr_player := "Done"
          | [] -> failwith "should not happen");
          print_endline
            ("\nCurrent cards: " ^ (order !main_player_cards |> deck_to_string)))
        else if response = "e" then escape ()
        else if next_bs_player () = "Done" then (
          change_to_pass !bs_curr_player bs_pass;
          let () = curr_player := next_player () in
          player_order ())
        else (
          change_to_pass !bs_curr_player bs_pass;
          bs_curr_player := next_bs_player ())
      end
      else (
        bs_player_callout ();
        let () = Random.self_init () in
        if Random.int 8 < 1 then begin
          print_endline (!bs_curr_player ^ " calls BS.");
          let table_list = peek_at_table table in
          match table_list with
          | (suit, number) :: t ->
              if number = Option.get !card_type then (
                print_endline
                  ("Here are the cards on the table: "
                  ^ deck_to_string (peek_at_table table |> order)
                  ^ ". " ^ !curr_player ^ " was not lying.");
                match_player_with_hand !bs_curr_player
                := table_list @ !(match_player_with_hand !bs_curr_player)
                   |> order)
              else (
                print_endline
                  ("Here are the cards on the table: "
                  ^ deck_to_string (peek_at_table table |> order)
                  ^ ". " ^ !curr_player ^ " was lying.");
                match_player_with_hand !curr_player
                := table_list @ !(match_player_with_hand !curr_player) |> order;
                curr_player := !bs_curr_player);
              table.table_cards <- [];
              bs_curr_player := "Done"
          | [] -> failwith "should not happen"
        end
        else (
          print_endline (!bs_curr_player ^ " does not call BS.");
          change_to_pass !bs_curr_player bs_pass;
          bs_curr_player := next_bs_player ()))
    done;
  print_endline
    ("\nCurrent cards: " ^ (order !main_player_cards |> deck_to_string));
  if is_end bs_pass then curr_player := next_player () else card_type := None;
  bs_curr_player := set_bs_player !curr_player

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
  print_endline
    "7) Lastly, if at any point you wish to escape the game press the \"e\' to \
     leave";
  let () = Random.self_init () in
  let s = shuffle card_list in
  player1_hand := assign 1 13 s [] |> order;
  player2_hand := assign 14 26 s [] |> order;
  player3_hand := assign 27 39 s [] |> order;
  player4_hand := assign 40 52 s [] |> order;
  start_round p;
  start_round bs_pass;
  bs_pass_curr_player !curr_player

let () =
  main ();
  start ();
  while !winner_status = false do
    player_order ();
    pass_or_play ();
    callout ();
    winner ()
  done
