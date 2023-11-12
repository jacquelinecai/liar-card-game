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
              |> Hand.order |> card_to_string_list) );
    ( "ordering cards test on cards of same number" >:: fun _ ->
          assert_equal ~printer:(pp_list pp_string)
            [ "6 of Diamonds"; "6 of Clubs"; "6 of Hearts"; "6 of Spades" ]
            ([
              (Clubs, Number 6);
              (Spades, Number 6);
              (Diamonds, Number 6);
              (Hearts, Number 6);
            ]
              |> Hand.order |> card_to_string_list) );
    ( "assigning unordered deck of cards" >:: fun _ ->
          assert_equal ~printer:(pp_list pp_string)
            [ "Ace of Clubs"; "2 of Clubs"; "3 of Clubs" ]
            (assign 1 3 Hand.unshuffled_deck [] |> Hand.order |> card_to_string_list)
    );
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
            (assign 39 45 Hand.unshuffled_deck []
             |> Hand.order |> card_to_string_list) );
    ( "contains test on a card in the deck" >:: fun _ ->
          assert_equal true
            (contains (Hearts, King) (assign 39 45 Hand.unshuffled_deck [])) );
    ( "contains test on first card in the deck" >:: fun _ ->
          assert_equal true
            (contains (Clubs, Number 1) (assign 1 5 Hand.unshuffled_deck [])) );
    ( "contains test on a card not in the deck" >:: fun _ ->
          assert_equal false
            (contains (Spades, Number 10) (assign 1 5 Hand.unshuffled_deck [])) );
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
            (updateDeck (Hearts, King) (assign 39 45 Hand.unshuffled_deck []) []
             |> Hand.order |> card_to_string_list) );
    ( "updateDeck test on first card in the deck" >:: fun _ ->
          assert_equal ~printer:(pp_list pp_string)
            [ "2 of Clubs"; "3 of Clubs"; "4 of Clubs"; "5 of Clubs" ]
            (updateDeck (Clubs, Number 1) (assign 1 5 Hand.unshuffled_deck []) []
             |> Hand.order |> card_to_string_list) );
    ( "updateDeck test on card in the middle of the deck" >:: fun _ ->
          assert_equal ~printer:(pp_list pp_string)
            [ "Ace of Clubs"; "2 of Clubs"; "4 of Clubs"; "5 of Clubs" ]
            (updateDeck (Clubs, Number 3) (assign 1 5 Hand.unshuffled_deck []) []
             |> Hand.order |> card_to_string_list) );
    ( "updateDeck test on a card not in the deck" >:: fun _ ->
          assert_raises InvalidCard (fun () ->
              updateDeck (Spades, Number 10) (assign 1 5 Hand.unshuffled_deck []) [])
    );
    ( "updateDeckWithCardList test on first card in the deck" >:: fun _ ->
          assert_equal ~printer:(pp_list pp_string)
            [ "2 of Clubs"; "3 of Clubs"; "4 of Clubs"; "5 of Clubs" ]
            (updateDeckWithCardList [(Clubs, Number 1)] (assign 1 5 Hand.unshuffled_deck [])
             |> Hand.order |> card_to_string_list) );
    ( "updateDeckWithCardList test on five card in a list of five cards" >:: fun _ ->
          assert_equal ~printer:(pp_list pp_string)
            [ ]
            (updateDeckWithCardList [(Clubs, Number 1);(Clubs, Number 2);
                                     (Clubs, Number 3);(Clubs, Number 4);(Clubs, Number 5);]
               (assign 1 5 Hand.unshuffled_deck [])
             |> Hand.order |> card_to_string_list) );
    ("card list to string" >:: fun _ -> assert_equal ~printer:(pp_string)
         "K ♣, 5 ♦" ([(Clubs, King);(Diamonds, Number 5);] |> cardlist_to_string));
    ("string to card list" >:: fun _ -> assert_equal
         [(Clubs, King);(Diamonds, Number 5);]
         ((String.split_on_char ('-') "KC-5D") |> stringlist_to_card_list));
    ("string to card list" >:: fun _ -> assert_equal
         [(Diamonds, Number 7);(Clubs, Number 7);]
         ((String.split_on_char ('-') "7C-7D") |> stringlist_to_card_list |> order));

  ]

let game_tests =
  [
    ( "card status on 4 empty card lists" >:: fun _ ->
          assert_equal (0, 0, 0, 0) (card_status [] [] [] []) );
    ( "card status on initially player hands" >:: fun _ ->
          assert_equal (13, 13, 13, 13)
            (card_status !player1_hand !player2_hand !player3_hand !player4_hand) );
    ( "check winner on first player" >:: fun _ ->
          assert_equal 1
            (card_status [] !player2_hand !player3_hand !player4_hand
             |> check_winner) );
    ( "check winner with all empty hands" >:: fun _ ->
          assert_raises InvalidCardAmount (fun () ->
              card_status [] [] [] [] |> check_winner) );
  ]

let suite =
  "test suite for Liar Card Game" >::: List.flatten [ hand_tests; game_tests ]

let table1 = { table_cards = []; discard_pile = [] }
let add_one_card = modify_table_cards table1 [ (Diamonds, Jack) ]
let table2 = { table_cards = []; discard_pile = [] }

let add_two_cards =
  modify_table_cards table2 [ (Spades, Queen); (Hearts, Number 3) ]

let table3 = { table_cards = []; discard_pile = [] }

let add_three_cards =
  modify_table_cards table3
    [ (Spades, Queen); (Hearts, Number 3); (Clubs, Number 5) ]

let table_tests =
  [
    ( "adding one card to the table" >:: fun _ ->
          assert_equal ~printer:(pp_list pp_string) [ "Jack of Diamonds" ]
            (peek_at_table table1 |> card_to_string_list) );
    ( "adding two cards to the table" >:: fun _ ->
          assert_equal ~printer:(pp_list pp_string)
            [ "Queen of Spades"; "3 of Hearts" ]
            (peek_at_table table2 |> card_to_string_list) );
    ( "adding three cards to the table" >:: fun _ ->
          assert_equal ~printer:(pp_list pp_string)
            [ "Queen of Spades"; "3 of Hearts"; "5 of Clubs" ]
            (peek_at_table table3 |> card_to_string_list) );
  ]

let suite =
  "test suite for Liar Card Game" >::: List.flatten [ hand_tests; table_tests ]

let () = run_test_tt_main suite
