open Bogue
open Card
module W = Widget
module L = Layout

let main () =
  let button = W.button ~border_radius:10 "Start" in
  let buttonlayout = L.flat_of_w [ button ] in
  let tower = L.tower ~sep:10 ~align:Draw.Center [ buttonlayout ] in
  let size = L.set_size tower (500, 500) in
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
