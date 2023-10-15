open Bogue
open Card
module W = Widget
module L = Layout

let main () =
  let welcome = W.label ~size:60 "Welcome to BS" in
  let button =
    let button_bg = Style.Solid (181, 214, 214, 255) in
    let button_bg_over = Some (Style.Solid (181, 214, 214, 255)) in
    let button_bg_pressed = Style.Solid (230, 149, 151, 255) in
    W.button ~bg_on:button_bg_pressed ~bg_over:button_bg_over ~bg_off:button_bg
      ~border_radius:10 "Start"
  in
  let buttonlayout = L.flat_of_w [ button ] in
  let button_size = L.set_size buttonlayout (30, 20) in
  let tower =
    L.tower ~sep:10 ~align:Draw.Center
      [ L.resident ~w:60 welcome; buttonlayout ]
  in
  let size = L.set_size tower (700, 700) in
  let background =
    L.set_background tower (Some (L.color_bg (255, 218, 224, 255)))
  in

  (* let on_press button _ev = let b = W.get_button button in if
     Button.is_pressed b then (W.label "Hi") else (W.label "Start game") in

     let c = W.connect ~priority:W.Join button on_press *)
  let board = Bogue.of_layout tower in
  Bogue.run board

let () =
  main ();
  Bogue.quit ()
