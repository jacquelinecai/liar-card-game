(** Test Plan: We strive to create tests to ensure the functionality and
    usability of our single-player Liar Card Game in the terminal environment.

    Our unit test cases ensure that the functions of the game are running as
    expected. We mainly used OUnit tests to test functions in our Card, Hand,
    Game, Round, and Table modules. We used black box testing when evaluating
    the game from an external perspective and tested basic cases of card
    situations. We verified that game functions were running expectedly
    according to their specifications. Furthermore, we used glass box testing to
    assess the internal logic and code structure. We tested edge cases such as
    empty lists of cards, empty tables, and other unexpected cases. All of our
    functions in our Card, Hand, Game, Round and Table modules were tested using
    OUnit tests. Both glass box testing and black box testing were used to
    create test cases. Some functions could not be tested directly, but instead
    were developed through pipelining through other functions contained in the
    module. These approaches helped to make sure that our documentation was
    correct and to find edge cases for functions in our modules. We ensured the
    correct handling of card piles and their distribution based on the outcomes
    of the game. However, certain functionalities, like calling BS, were tested
    manually by either playing the game since this functionality requires user
    input.

    Due to the fact that the cards are always distributed randomly, we could
    only use manual test cases in the terminal to analyze the actual stimulation
    of the game in real scenarios. Player roles were also distributed randomly,
    which we tested through multiple plays of the game. All of our
    functionalities in our /bin/main.ml file were tested manually by playing the
    game in the terminal due to this reason, and also since some of our
    functionalities require user input. We manually tested the information
    printed before and after each round and/or move. We manually tested cases of
    randomized and unexpected user inputs throughout the game.

    Our program behaves exactly as intended through our OUnit and manual testing
    cases. *)

open OUnit2
open Liargame
open Card
open Hand
open Game
open Table
open Round

(** From A2: [pp_string s] pretty-prints card [c]. *)
let pp_string c = "\"" ^ c ^ "\""

(** From A2: [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let suit_to_string (s : suit) : string =
  match s with
  | Clubs -> "Clubs"
  | Diamonds -> "Diamonds"
  | Hearts -> "Hearts"
  | Spades -> "Spades"

let number_to_string (s : number) : string =
  match s with
  | Number x -> if x = 1 then "Ace" else string_of_int x
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"

let rec card_to_string_list (cards : card list) : string list =
  match cards with
  | [] -> []
  | (s, n) :: t ->
      (number_to_string n ^ " of " ^ suit_to_string s) :: card_to_string_list t

let deck1 =
  let () = Random.self_init () in
  shuffle unshuffled_deck

let deck2 =
  let () = Random.self_init () in
  shuffle unshuffled_deck

let card_tests =
  [
    ( "shuffling deck produces different output each time" >:: fun _ ->
      assert_equal ~printer:string_of_bool false (deck1 = deck2) );
    ( "shuffling deck produces permutation of input deck" >:: fun _ ->
      assert_equal ~printer:string_of_bool true
        (order deck1 = order unshuffled_deck) );
    ( "shuffling deck doesn't change input deck" >:: fun _ ->
      assert_equal ~printer:string_of_bool false (deck1 = unshuffled_deck) );
    ( "contains test on a card in the deck" >:: fun _ ->
      assert_equal ~printer:string_of_bool true
        (contains (Hearts, King) (assign 39 45 unshuffled_deck [])) );
    ( "contains test on first card in the deck" >:: fun _ ->
      assert_equal ~printer:string_of_bool true
        (contains (Clubs, Number 1) (assign 1 5 unshuffled_deck [])) );
    ( "contains test on a card not in the deck" >:: fun _ ->
      assert_equal ~printer:string_of_bool false
        (contains (Spades, Number 10) (assign 1 5 unshuffled_deck [])) );
    ( "string to card list1" >:: fun _ ->
      assert_equal
        [ Some (Clubs, King); Some (Diamonds, Number 5) ]
        (String.split_on_char '-' "KC-5D" |> stringlist_to_card_list) );
    ( "string to card list2" >:: fun _ ->
      assert_equal
        [ Some (Clubs, Number 7); Some (Diamonds, Number 7) ]
        (String.split_on_char '-' "7C-7D" |> stringlist_to_card_list) );
    ( "string to card list3" >:: fun _ ->
      assert_equal
        [ (Clubs, Number 7); (Diamonds, Number 7) ]
        (String.split_on_char '-' "7C-7D"
        |> stringlist_to_card_list |> toCardList) );
    ( "string to card random list4" >:: fun _ ->
      assert_equal
        [
          (Clubs, Number 7);
          (Diamonds, Number 7);
          (Diamonds, Number 5);
          (Diamonds, Number 8);
          (Spades, Number 9);
        ]
        (String.split_on_char '-' (String.uppercase_ascii "7C-7D-5D-8d-9s")
        |> stringlist_to_card_list |> toCardList) );
    ( "string to card random list5" >:: fun _ ->
      assert_equal
        [
          (Clubs, Jack);
          (Diamonds, Queen);
          (Diamonds, King);
          (Diamonds, Number 1);
          (Spades, Number 2);
        ]
        (String.split_on_char '-' (String.uppercase_ascii "JC-QD-KD-aD-2s")
        |> stringlist_to_card_list |> toCardList) );
    ( "string to card random list6" >:: fun _ ->
      assert_equal
        [
          (Hearts, Jack);
          (Hearts, Queen);
          (Hearts, King);
          (Spades, Number 1);
          (Spades, Number 2);
          (Spades, Number 10);
          (Spades, Number 4);
          (Spades, Number 5);
        ]
        (String.split_on_char '-'
           (String.uppercase_ascii "JH-QH-KH-AS-2s-10S-4S-5S")
        |> stringlist_to_card_list |> toCardList) );
    ( "string to card list1" >:: fun _ ->
      assert_raises InvalidCard (fun () ->
          String.split_on_char '-' (String.uppercase_ascii "p")
          |> stringlist_to_card_list |> toCardList) );
    ( "string to card list1" >:: fun _ ->
      assert_raises InvalidCard (fun () ->
          String.split_on_char '-' (String.uppercase_ascii "JC-QD-KD-aD-2z")
          |> stringlist_to_card_list |> toCardList) );
    ( "string to card list1" >:: fun _ ->
      assert_raises InvalidCard (fun () ->
          String.split_on_char '-' (String.uppercase_ascii "1d-2d-7d-11D")
          |> stringlist_to_card_list |> toCardList) );
    ( "valid test on not valid list" >:: fun _ ->
      assert_equal false (valid [ None ] unshuffled_deck) );
    ( "valid test on not valid list" >:: fun _ ->
      assert_equal false
        (valid
           [ Some (Clubs, Number 1); Some (Hearts, King); None ]
           unshuffled_deck) );
    ( "valid test on not valid list" >:: fun _ ->
      assert_equal true
        (valid [ Some (Clubs, Number 1); Some (Hearts, King) ] unshuffled_deck)
    );
    ( "valid test on cards not in list" >:: fun _ ->
      assert_equal false
        (valid
           [ Some (Clubs, Number 1); Some (Hearts, King) ]
           [ (Clubs, Number 2); (Hearts, Number 3) ]) );
    ( "card list to string" >:: fun _ ->
      assert_equal ~printer:pp_string "K ♣, 5 ♦"
        ([ (Clubs, King); (Diamonds, Number 5) ] |> cardlist_to_string) );
    ( "card list to string ordered" >:: fun _ ->
      assert_equal ~printer:pp_string "5 ♦, K ♣"
        ([ (Clubs, King); (Diamonds, Number 5) ] |> order |> cardlist_to_string)
    );
    ( "initial card list to string" >:: fun _ ->
      assert_equal ~printer:pp_string
        "A ♣, 2 ♣, 3 ♣, 4 ♣, 5 ♣, 6 ♣, 7 ♣, 8 ♣, 9 ♣, 10 ♣, J ♣, Q ♣, K ♣"
        (assign 1 13 unshuffled_deck [] |> order |> cardlist_to_string) );
  ]

let hand_tests =
  [
    ( "ordering cards test on five cards" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "Ace of Diamonds"; "5 of Diamonds"; "5 of Spades"; "King of Clubs" ]
        ([
           (Clubs, King);
           (Diamonds, Number 1);
           (Spades, Number 5);
           (Diamonds, Number 5);
         ]
        |> order |> card_to_string_list) );
    ( "ordering cards test on cards of same number" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "6 of Diamonds"; "6 of Clubs"; "6 of Hearts"; "6 of Spades" ]
        ([
           (Clubs, Number 6);
           (Spades, Number 6);
           (Diamonds, Number 6);
           (Hearts, Number 6);
         ]
        |> order |> card_to_string_list) );
    ( "assigning unordered deck of cards" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "Ace of Clubs"; "2 of Clubs"; "3 of Clubs" ]
        (assign 1 3 unshuffled_deck [] |> order |> card_to_string_list) );
    ( "assigning unordered deck of cards" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [
          "Ace of Spades";
          "2 of Spades";
          "3 of Spades";
          "4 of Spades";
          "5 of Spades";
          "6 of Spades";
          "King of Hearts";
        ]
        (assign 39 45 unshuffled_deck [] |> order |> card_to_string_list) );
    ( "containsNum test on empty card list" >:: fun _ ->
      assert_equal false (containsNum King []) );
    ( "containsNum test on existing number" >:: fun _ ->
      assert_equal true (containsNum King (assign 39 45 unshuffled_deck [])) );
    ( "containsNum test on non-existing number" >:: fun _ ->
      assert_equal false
        (containsNum (Number 8) (assign 39 45 unshuffled_deck [])) );
    ( "numCards test on empty card list" >:: fun _ ->
      assert_equal 0 (numCards King [] 0) );
    ( "numCards test on entire deck" >:: fun _ ->
      assert_equal 4 (numCards King unshuffled_deck 0) );
    ( "numCards test on first group in deck" >:: fun _ ->
      assert_equal 2 (numCards (Number 1) (assign 1 15 unshuffled_deck []) 0) );
    ( "nCards test on 0 amount of cards" >:: fun _ ->
      assert_equal [] (nCards King 0 unshuffled_deck []) );
    ( "nCards test on entire deck" >:: fun _ ->
      assert_equal
        [ (Diamonds, King); (Clubs, King); (Hearts, King); (Spades, King) ]
        (nCards King 4 unshuffled_deck [] |> order) );
    ( "nCards test on partial amount of entire deck" >:: fun _ ->
      assert_equal
        [ (Diamonds, King); (Clubs, King) ]
        (nCards King 2 unshuffled_deck [] |> order) );
    ( "getRandCards test on empty indices list" >:: fun _ ->
      assert_equal [] (getRandCards [] 0 unshuffled_deck []) );
    ( "getRandCards test on non-empty indices list over entire deck" >:: fun _ ->
      assert_equal
        [ (Clubs, Number 5); (Clubs, Jack); (Hearts, King); (Spades, King) ]
        (getRandCards [ 4; 10; 38; 51 ] 0 unshuffled_deck [] |> order) );
    ( "getRandCards test on partial deck" >:: fun _ ->
      assert_equal
        [ (Spades, Number 1); (Hearts, Number 9) ]
        (getRandCards [ 2; 7 ] 0
           (assign 33 45 unshuffled_deck [] |> List.rev)
           []
        |> order) );
    ( "updateDeck test on a card in the deck" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [
          "Ace of Spades";
          "2 of Spades";
          "3 of Spades";
          "4 of Spades";
          "5 of Spades";
          "6 of Spades";
        ]
        (updateDeck (Hearts, King) (assign 39 45 unshuffled_deck []) []
        |> order |> card_to_string_list) );
    ( "updateDeck test on first card in the deck" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "2 of Clubs"; "3 of Clubs"; "4 of Clubs"; "5 of Clubs" ]
        (updateDeck (Clubs, Number 1) (assign 1 5 unshuffled_deck []) []
        |> order |> card_to_string_list) );
    ( "updateDeck test on card in the middle of the deck" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "Ace of Clubs"; "2 of Clubs"; "4 of Clubs"; "5 of Clubs" ]
        (updateDeck (Clubs, Number 3) (assign 1 5 unshuffled_deck []) []
        |> order |> card_to_string_list) );
    ( "updateDeck test on a card not in the deck" >:: fun _ ->
      assert_raises InvalidCard (fun () ->
          updateDeck (Spades, Number 10) (assign 1 5 unshuffled_deck []) []) );
    ( "updateDeckWithCardList test on first card in the deck" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "2 of Clubs"; "3 of Clubs"; "4 of Clubs"; "5 of Clubs" ]
        (updateDeckWithCardList
           [ (Clubs, Number 1) ]
           (assign 1 5 unshuffled_deck [])
        |> order |> card_to_string_list) );
    ( "updateDeckWithCardList test on five card in a list of five cards"
    >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        (updateDeckWithCardList
           [
             (Clubs, Number 1);
             (Clubs, Number 2);
             (Clubs, Number 3);
             (Clubs, Number 4);
             (Clubs, Number 5);
           ]
           (assign 1 5 unshuffled_deck [])
        |> order |> card_to_string_list) );
    ( "updateDeckWithCardList test on first three card in the deck" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "4 of Clubs"; "5 of Clubs" ]
        (updateDeckWithCardList
           [ (Clubs, Number 1); (Clubs, Number 2); (Clubs, Number 3) ]
           (assign 1 5 unshuffled_deck [])
        |> order |> card_to_string_list) );
    ( "updateDeckWithCardList test on empty list" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [
          "Ace of Clubs"; "2 of Clubs"; "3 of Clubs"; "4 of Clubs"; "5 of Clubs";
        ]
        (updateDeckWithCardList [] (assign 1 5 unshuffled_deck [])
        |> order |> card_to_string_list) );
    ( "updateDeckWithCardList test on all cards" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        (updateDeckWithCardList unshuffled_deck unshuffled_deck
        |> order |> card_to_string_list) );
    ( "updateDeckWithCardList test on empty list" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "Ace of Clubs" ]
        (updateDeckWithCardList [] (assign 1 1 unshuffled_deck [])
        |> order |> card_to_string_list) );
    ( "updateDeck test on a card in the deck" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [
          "Ace of Spades";
          "2 of Spades";
          "3 of Spades";
          "4 of Spades";
          "5 of Spades";
          "6 of Spades";
        ]
        (updateDeck (Hearts, King) (assign 39 45 unshuffled_deck []) []
        |> order |> card_to_string_list) );
    ( "updateDeck test on a card in the deck" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [
          "2 of Spades";
          "3 of Spades";
          "4 of Spades";
          "5 of Spades";
          "6 of Spades";
        ]
        (updateDeck (Spades, Number 1) (assign 40 45 unshuffled_deck []) []
        |> order |> card_to_string_list) );
    ( "updateDeck test on a card in the deck" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [
          "2 of Clubs";
          "3 of Clubs";
          "4 of Clubs";
          "5 of Clubs";
          "7 of Clubs";
          "8 of Clubs";
        ]
        (updateDeck (Clubs, Number 6)
           (updateDeck (Clubs, Number 1) (assign 1 8 unshuffled_deck []) [])
           []
        |> order |> card_to_string_list) );
    ( "updateDeck test on first card in the deck" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "2 of Clubs"; "3 of Clubs"; "4 of Clubs"; "5 of Clubs" ]
        (updateDeck (Clubs, Number 1) (assign 1 5 unshuffled_deck []) []
        |> order |> card_to_string_list) );
    ( "updateDeck test on a card not in the deck" >:: fun _ ->
      assert_raises InvalidCard (fun () ->
          updateDeck (Spades, Number 10) (assign 1 5 unshuffled_deck []) []) );
    ( "updateDeck test on a card not in the deck" >:: fun _ ->
      assert_raises InvalidCard (fun () ->
          updateDeck (Spades, Number 1) (assign 1 5 unshuffled_deck []) []) );
    ( "Checking to see if player 1 has 13 cards at first" >:: fun _ ->
      assert_equal 13 (player_hand_size 1) );
    ( "Checking to see if player 2 has 13 cards at first" >:: fun _ ->
      assert_equal 13 (player_hand_size 2) );
    ( "Checking to see if player 3 has 13 cards at first" >:: fun _ ->
      assert_equal 13 (player_hand_size 3) );
    ( "Checking to see if player 4 has 13 cards at first" >:: fun _ ->
      assert_equal 13 (player_hand_size 4) );
  ]

let game_tests =
  [
    ( "card status on 4 empty card lists" >:: fun _ ->
      assert_equal (0, 0, 0, 0) (card_status [] [] [] []) );
    ( "card status on initial player hands" >:: fun _ ->
      assert_equal (13, 13, 13, 13)
        (card_status !player1_hand !player2_hand !player3_hand !player4_hand) );
    ( "card status during middle of game" >:: fun _ ->
      assert_equal (4, 6, 14, 2)
        (card_status
           (assign 1 4 unshuffled_deck [])
           (assign 5 10 unshuffled_deck [])
           ((Diamonds, Number 10) :: !player3_hand)
           [ (Diamonds, King); (Hearts, Number 1) ]) );
    ( "check winner on first player" >:: fun _ ->
      assert_equal 1
        (card_status [] !player2_hand !player3_hand !player4_hand
        |> check_winner) );
    ( "check winner on middle player" >:: fun _ ->
      assert_equal 3
        (card_status !player1_hand !player2_hand [] !player4_hand
        |> check_winner) );
    ( "check winner with some empty hands" >:: fun _ ->
      assert_raises InvalidCardAmount (fun () ->
          card_status !player1_hand [] [] !player4_hand |> check_winner) );
    ( "check winner with all empty hands" >:: fun _ ->
      assert_raises InvalidCardAmount (fun () ->
          card_status [] [] [] [] |> check_winner) );
  ]

(** Have to make multiple tables due to the fact that we are mutating each table
    in our test cases.*)
let empty_table = { table_cards = []; discard_pile = [] }

let table1 = { table_cards = []; discard_pile = [] }
let add_one_card = adding_cards_to_table table1 [ (Diamonds, Jack) ]
let table2 = { table_cards = []; discard_pile = [] }

let add_two_cards =
  adding_cards_to_table table2 [ (Spades, Queen); (Hearts, Number 3) ]

let table3 = { table_cards = []; discard_pile = [] }

let add_three_cards =
  adding_cards_to_table table3
    [ (Spades, Queen); (Hearts, Number 3); (Clubs, Number 5) ]

let option_table4 = { table_cards = []; discard_pile = [] }

let add_card_option =
  adding_card_to_table option_table4 (Some (Hearts, Number 9))

let add_card_option_none = adding_card_to_table option_table4 None
let table5 = { table_cards = []; discard_pile = [] }

let modify_table =
  modify_table_cards table5
    [ (Clubs, Number 1); (Spades, Queen); (Diamonds, Number 10) ]

let table6 = { table_cards = []; discard_pile = [] }
let table7 = { table_cards = []; discard_pile = [] }
let table8 = { table_cards = []; discard_pile = [] }
let card1 : card = (Spades, Number 1)
let card2 : card = (Hearts, King)
let card3 : card = (Diamonds, Number 2)

let table_tests =
  [
    ( "empty table table cards" >:: fun _ ->
      assert_equal [] empty_table.table_cards );
    ( "empty table discard pile" >:: fun _ ->
      assert_equal [] empty_table.discard_pile );
    ( "modify table by 3" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "Ace of Clubs"; "Queen of Spades"; "10 of Diamonds" ]
        (peek_at_table table5 |> card_to_string_list) );
    ( "do not modify table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "Ace of Clubs"; "Queen of Spades"; "10 of Diamonds" ]
        (modify_table_cards table5 [];
         peek_at_table table5 |> card_to_string_list) );
    ( "adding one card to the table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "Jack of Diamonds" ]
        (peek_at_table table1 |> card_to_string_list) );
    ( "adding no cards to the table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "Jack of Diamonds" ]
        (adding_cards_to_table table1 [];
         peek_at_table table1 |> card_to_string_list) );
    ( "adding two cards to the table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "3 of Hearts"; "Queen of Spades" ]
        (peek_at_table table2 |> card_to_string_list) );
    ( "adding three cards to the table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "5 of Clubs"; "3 of Hearts"; "Queen of Spades" ]
        (peek_at_table table3 |> card_to_string_list) );
    ( "adding card option to the table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "9 of Hearts" ]
        (peek_at_table option_table4 |> card_to_string_list) );
    ( "adding none card option to the table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "9 of Hearts" ]
        (peek_at_table option_table4 |> card_to_string_list) );
    ("table size of 0" >:: fun _ -> assert_equal 0 (table_size empty_table));
    ("table size of 1" >:: fun _ -> assert_equal 1 (table_size table1));
    ("table size of 2" >:: fun _ -> assert_equal 2 (table_size table2));
    ("table size of 3" >:: fun _ -> assert_equal 3 (table_size table3));
    ( "discard a pile of size 0" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        (discard_cards empty_table;
         peek_at_discard_pile empty_table |> card_to_string_list) );
    ( "discard a pile of size 0 checking table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        (discard_cards empty_table;
         peek_at_table empty_table |> card_to_string_list) );
    ( "discard a pile of size 1" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "Jack of Diamonds" ]
        (discard_cards table1;
         peek_at_discard_pile table1 |> card_to_string_list) );
    ( "discard a pile of size 1 checking table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        (discard_cards table1;
         peek_at_table table1 |> card_to_string_list) );
    ( "discard a pile of size 1 checking table size" >:: fun _ ->
      assert_equal 0
        (discard_cards table1;
         table_size table1) );
    ( "discard a pile of size 2" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "3 of Hearts"; "Queen of Spades" ]
        (discard_cards table2;
         peek_at_discard_pile table2 |> card_to_string_list) );
    ( "discard a pile of size 2 checking table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        (discard_cards table2;
         peek_at_table table2 |> card_to_string_list) );
    ( "discard a pile of size 2 checking table size" >:: fun _ ->
      assert_equal 0
        (discard_cards table2;
         table_size table2) );
    ( "discard a pile of size 3" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "5 of Clubs"; "3 of Hearts"; "Queen of Spades" ]
        (discard_cards table3;
         peek_at_discard_pile table3 |> card_to_string_list) );
    ( "discard a pile of size 3 checking table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        (discard_cards table3;
         peek_at_table table3 |> card_to_string_list) );
    ( "discard a pile of size 3 checking table size" >:: fun _ ->
      assert_equal 0
        (discard_cards table3;
         table_size table3) );
    ( "Combining multiple operations: adding_cards_to_table and discard_cards \
       and peek_at_table and peek_at_discard_pile"
    >:: fun _ ->
      adding_cards_to_table table6 [ card1; card2; card3 ];
      discard_cards table6;
      assert (peek_at_table table6 = []);
      assert (peek_at_discard_pile table6 = [ card3; card2; card1 ]) );
    ( "Combining multiple operations: modifying and discard_cards and \
       peek_at_table and peek_at_discard_pile"
    >:: fun _ ->
      modify_table_cards table7 [ card1; card2 ];
      discard_cards table7;
      assert (peek_at_table table7 = []);
      assert (peek_at_discard_pile table7 = [ card1; card2 ]) );
    ( "Complex sequence of operations" >:: fun _ ->
      adding_cards_to_table table8 [ card1 ];
      modify_table_cards table8 [ card2; card3 ];
      discard_cards table8;
      adding_cards_to_table table8 [ card1; card2 ];
      modify_table_cards table8 [ card3 ];
      discard_cards table8;
      assert (peek_at_table table8 = []);
      assert (
        peek_at_discard_pile table8
        = [ card3; card2; card1; card2; card3; card1 ]) );
  ]

let all_pass x =
  if x then
    let () = change_to_pass "Player 3" p in
    let () = change_to_pass "Player 1" p in
    let () = change_to_pass "Player 2" p in
    change_to_pass "Player 4" p
  else ()

let round_tests =
  [
    ( "testing number_match with a number" >:: fun _ ->
      assert_equal ~printer:pp_string "10" (number_match (Number 10)) );
    ( "testing number_match with Jack" >:: fun _ ->
      assert_equal ~printer:pp_string "J" (number_match Jack) );
    ( "testing number_match with Ace" >:: fun _ ->
      assert_equal ~printer:pp_string "A" (number_match (Number 1)) );
    ( "testing pass_list" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "NotPass"; "NotPass"; "NotPass"; "NotPass" ]
        (pass_list p) );
    ( "testing change_to_pass with Player 2" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "NotPass"; "Pass"; "NotPass"; "NotPass" ]
        (let () = change_to_pass "Player 2" p in
         pass_list p) );
    ( "testing change_to_pass when all players pass" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "Pass"; "Pass"; "Pass"; "Pass" ]
        (let () = all_pass true in
         pass_list p) );
    ( "testing is_end when all players pass" >:: fun _ ->
      assert_equal ~printer:string_of_bool true
        (let () = all_pass true in
         is_end p) );
    ( "testing is_end when no players pass" >:: fun _ ->
      assert_equal ~printer:string_of_bool false
        (let () = start_round p in
         is_end p) );
    ( "testing end_round when all players pass" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "NotPass"; "NotPass"; "NotPass"; "NotPass" ]
        (let () = all_pass true in
         let () = end_round p in
         pass_list p) );
    ( "suggested_card_play with one number of greatest frequency" >:: fun _ ->
      assert_equal ~printer:(pp_list number_match) [ Number 1 ]
        (suggested_card_type (assign 1 14 unshuffled_deck [])) );
    ( "suggested_card_play with many numbers of same frequency" >:: fun _ ->
      assert_equal ~printer:(pp_list number_match)
        [ Number 1; Number 2; Number 3; Number 4; Number 5; King ]
        (suggested_card_type (assign 13 31 unshuffled_deck []) |> order_num) );
    ( "suggested_card_play on entire deck" >:: fun _ ->
      assert_equal ~printer:(pp_list number_match)
        [
          Number 1;
          Number 2;
          Number 3;
          Number 4;
          Number 5;
          Number 6;
          Number 7;
          Number 8;
          Number 9;
          Number 10;
          Jack;
          Queen;
          King;
        ]
        (suggested_card_type unshuffled_deck |> order_num) );
    ( "suggested_play with card list that contains number n" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "Ace of Clubs"; "Ace of Diamonds" ]
        (suggested_play (Number 1) 0 (assign 1 14 unshuffled_deck [])
        |> Option.get |> card_to_string_list) );
    ( "bot_play returns card list with correct range of cards" >:: fun _ ->
      assert_equal ~printer:string_of_bool true
        (bot_play (Number 5) 4 unshuffled_deck |> Option.get |> List.length <= 4)
    );
  ]

let suite =
  "test suite for Liar Card Game"
  >::: List.flatten
         [ card_tests; hand_tests; game_tests; table_tests; round_tests ]

let () = run_test_tt_main suite
