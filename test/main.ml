open OUnit2
open Liargame
open Card
open Hand

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
  ]

let suite = "test suite for Liar Card Game" >::: List.flatten [ hand_tests ]
let () = run_test_tt_main suite
