open Bogue
open Card
module W = Widget
module L = Layout

let display imageString =
  let width = 15 in
  let height = 20 in
  let image = Widget.image ~w:width ~h:height imageString in

  let layout = Layout.flat_of_w [ image ] in
  let size = L.set_size layout (700, 700) in
  let board1 = Bogue.of_layout layout in
  Bogue.run board1

let button =
  let button_bg = Style.Solid (181, 214, 214, 255) in
  let button_bg_over = Some (Style.Solid (181, 214, 214, 255)) in
  let button_bg_pressed = Style.Solid (230, 149, 151, 255) in
  let action (x : bool) = display "card-images/ace_of_clubs.png" in
  W.button ~action ~bg_on:button_bg_pressed ~bg_over:button_bg_over
    ~bg_off:button_bg ~border_radius:10 "Start"

let display_start =
  let welcome = W.label ~size:60 "Welcome to BS" in
  let start_label = W.label ~size:40 "Press Start to Begin" in
  let buttonlayout = L.flat_of_w [ button ] in
  let button_size = L.set_size buttonlayout (30, 20) in
  let tower =
    L.tower ~sep:10 ~align:Draw.Center
      [ L.resident ~w:60 welcome; L.resident ~w:60 start_label; buttonlayout ]
  in
  let size = L.set_size tower (700, 700) in
  let background =
    L.set_background tower (Some (L.color_bg (255, 218, 224, 255)))
  in
  Bogue.run (Bogue.of_layout tower)

let main () =
  (* if Button.is_pressed (W.get_button button) then display
     "card-images/ace_of_clubs.png" else display_start *)
  display_start

(* let on_press button _ev = let b = W.get_button button in if Button.is_pressed
   b then (W.label "Hi") else (W.label "Start game") in

   let c = W.connect ~priority:W.Join button on_press *)

let () =
  main ();
  Bogue.quit ()
