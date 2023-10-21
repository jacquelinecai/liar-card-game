(* let () = print_endline "\n\nWelcome to BS.\n"; print_endline "Press s to
   start the game: "; let x = read_line () in match x with | "s" ->
   print_endline "Here are the cards:" | _ -> print_endline "Please try again.
   Press s to start the game: " *)
(* open Bogue open Card module W = Widget module L = Layout *)

(* Take in a card list and display it *)
open Bogue
module W = Widget
module L = Layout

let player =
  let width = 5 in
  let height = 5 in
  let player_image =
    W.image ~w:width ~h:height "card-images/player_image.png"
  in
  L.resident ~w:width ~h:height ~x:10 ~y:10 player_image

let display imageString =
  (* let width = 15 in let height = 20 in let image = Widget.image ~w:width
     ~h:height imageString in

     let layout = Layout.flat_of_w [ image ] in let size = L.set_size layout
     (700, 700) in *)
  let layout = player in
  let size = L.set_size layout (700, 700) in
  let board1 = Bogue.of_layout layout in
  Bogue.run board1

(* let size = L.set_size layout (700, 700) in Bogue.of_layout layout *)

(* let display imageString = let width = 2 in let height = 3 in let image =
   Widget.image ~w:width ~h:height imageString in Layout.flat_of_w [ image ] *)

let button =
  let button_bg = Style.Solid (181, 214, 214, 255) in
  let button_bg_over = Some (Style.Solid (181, 214, 214, 255)) in
  let button_bg_pressed = Style.Solid (230, 149, 151, 255) in
  let action (x : bool) = display "card-images/ace_of_clubs.png" in
  W.button ~action ~bg_on:button_bg_pressed ~bg_over:button_bg_over
    ~bg_off:button_bg ~border_radius:10 "Start"

let home_screen =
  let welcome = W.label ~size:60 "Welcome to BS" in
  let start_label = W.label ~size:40 "Press Start to Begin" in
  let buttonlayout = L.flat_of_w [ button ] in
  let button_size = L.set_size buttonlayout (30, 20) in
  L.tower ~sep:10 ~align:Draw.Center
    [ L.resident ~w:60 welcome; L.resident ~w:60 start_label; buttonlayout ]

(* let display_start = (* let tower = L.tower ~sep:10 ~align:Draw.Center [
   L.resident ~w:60 welcome; L.resident ~w:60 start_label; buttonlayout ] in *)
   let layout = if Button.is_pressed (W.get_button button) then display
   "card-images/ace_of_clubs.png" else home_screen in let size = L.set_size
   layout (700, 700) in let background = L.set_background layout (Some
   (L.color_bg (255, 218, 224, 255))) in Bogue.run (Bogue.of_layout layout) *)

let display_start =
  let start_screen = home_screen in
  let size = L.set_size start_screen (700, 700) in
  let background =
    L.set_background start_screen (Some (L.color_bg (255, 218, 224, 255)))
  in
  Bogue.run (Bogue.of_layout start_screen)

let main () =
  (* if Button.is_pressed (W.get_button button) then display
     "card-images/ace_of_clubs.png" else display_start *)
  display_start

(* let on_press button _ev = let b = W.get_button button in if Button.is_pressed
   b then (W.label "Hi") else (W.label "Start game") in

   let c = W.connect ~priority:W.Join button on_press *)
(* let () = main (); Bogue.quit () *)
