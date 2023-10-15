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
  | Ace -> "Ace"
  | Two -> "Two"
  | Three -> "Three"
  | Four -> "Four"
  | Five -> "Five"
  | Six -> "Six"
  | Seven -> "Seven"
  | Eight -> "Eight"
  | Nine -> "Nine"
  | Ten -> "Ten"
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"

let rec card_to_string_list (cards : card list) : string list =
  match cards with
  | [] -> []
  | (s, n) :: t ->
      (number_to_string n ^ " of " ^ suit_to_string s) :: card_to_string_list t

let cmp_demo =
  [
    ( "ordering cards test on five cards" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        ([ (Clubs, King); (Diamonds, Ace); (Spades, Five); (Diamonds, Five) ]
        |> Hand.order |> card_to_string_list)
        [
          "Ace of Diamonds";
          "Five of Diamonds";
          "Five of Spades";
          "King of Clubs";
        ] );
    ( "ordering cards test on cards of same number" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string)
        ([ (Clubs, Six); (Spades, Six); (Diamonds, Six); (Hearts, Six) ]
        |> Hand.order |> card_to_string_list)
        [ "Six of Diamonds"; "Six of Clubs"; "Six of Hearts"; "Six of Spades" ]
    );
  ]

let suite = "test suite for Liar Card Game" >::: List.flatten [ cmp_demo ]
let () = run_test_tt_main suite
