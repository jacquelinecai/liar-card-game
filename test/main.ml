open OUnit2
open Liargame
open Card
open Hand
open Game
open Table

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

module Card_test = Card

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
    ( "contains test on a card in the deck" >:: fun _ ->
      assert_equal true
        (contains (Hearts, King) (assign 39 45 unshuffled_deck [])) );
    ( "contains test on first card in the deck" >:: fun _ ->
      assert_equal true
        (contains (Clubs, Number 1) (assign 1 5 unshuffled_deck [])) );
    ( "contains test on a card not in the deck" >:: fun _ ->
      assert_equal false
        (contains (Spades, Number 10) (assign 1 5 unshuffled_deck [])) );
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
    ( "updateDeckWithCardList test on empty list" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "Ace of Clubs" ]
        (updateDeckWithCardList [] (assign 1 1 unshuffled_deck [])
        |> order |> card_to_string_list) );
    ( "card list to string" >:: fun _ ->
      assert_equal ~printer:pp_string "K ♣, 5 ♦"
        ([ (Clubs, King); (Diamonds, Number 5) ] |> cardlist_to_string) );
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
    ( "string to card list1" >:: fun _ ->
      assert_raises Invalid (fun () ->
          String.split_on_char '-' (String.uppercase_ascii "p")
          |> stringlist_to_card_list |> toCardList) );
    ( "string to card list1" >:: fun _ ->
      assert_raises Invalid (fun () ->
          String.split_on_char '-' (String.uppercase_ascii "JC-QD-KD-aD-2z")
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
    ( "contains test on a card in the deck" >:: fun _ ->
      assert_equal true
        (contains (Hearts, King) (assign 39 45 unshuffled_deck [])) );
    ( "contains test on first card in the deck" >:: fun _ ->
      assert_equal true
        (contains (Clubs, Number 1) (assign 1 5 unshuffled_deck [])) );
    ( "contains test on a card not in the deck" >:: fun _ ->
      assert_equal false
        (contains (Spades, Number 10) (assign 1 5 unshuffled_deck [])) );
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
    ( "updateDeck test on a card not in the deck" >:: fun _ ->
      assert_raises InvalidCard (fun () ->
          updateDeck (Spades, Number 10) (assign 1 5 unshuffled_deck []) []) );
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
    ( "check winner with all empty hands" >:: fun _ ->
      assert_raises InvalidCardAmount (fun () ->
          card_status [] [] [] [] |> check_winner) );
  ]

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

let table_tests =
  [
    ( "empty table table cards" >:: fun _ ->
      assert_equal [] empty_table.table_cards );
    ( "empty table discard pile" >:: fun _ ->
      assert_equal [] empty_table.discard_pile );
    ( "adding one card to the table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "Jack of Diamonds" ]
        (peek_at_table table1 |> card_to_string_list) );
    ( "adding two cards to the table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "3 of Hearts"; "Queen of Spades" ]
        (peek_at_table table2 |> card_to_string_list) );
    ( "adding three cards to the table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "5 of Clubs"; "3 of Hearts"; "Queen of Spades" ]
        (peek_at_table table3 |> card_to_string_list) );
    ("table size of 1" >:: fun _ -> assert_equal 1 (table_size table1));
    ("table size of 2" >:: fun _ -> assert_equal 2 (table_size table2));
    ("table size of 3" >:: fun _ -> assert_equal 3 (table_size table3));
    ( "discard a pile of size 1" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "Jack of Diamonds" ]
        (discard_cards table1;
         peek_at_discard_pile table1 |> card_to_string_list) );
    ( "discard a pile of size 1 checking table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        (discard_cards table1;
         peek_at_table table1 |> card_to_string_list) );
    ( "discard a pile of size 2" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "3 of Hearts"; "Queen of Spades" ]
        (discard_cards table2;
         peek_at_discard_pile table2 |> card_to_string_list) );
    ( "discard a pile of size 2 checking table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        (discard_cards table2;
         peek_at_table table2 |> card_to_string_list) );
    ( "discard a pile of size 3" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        [ "5 of Clubs"; "3 of Hearts"; "Queen of Spades" ]
        (discard_cards table3;
         peek_at_discard_pile table3 |> card_to_string_list) );
    ( "discard a pile of size 3 checking table" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        (discard_cards table3;
         peek_at_table table3 |> card_to_string_list) );
  ]

let suite =
  "test suite for Liar Card Game"
  >::: List.flatten [ hand_tests; game_tests; table_tests ]

let () = run_test_tt_main suite
