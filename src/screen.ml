open Bogue
open Card
module W = Widget
module L = Layout

let main () =
  let b = W.check_box () in
  let l = W.label "Hello world" in
  let layout = L.flat_of_w [ b; l ] in
  let size = L.set_size layout (500, 500) in
  let background =
    L.set_background layout (Some (L.color_bg (255, 218, 224, 255)))
  in
  let board = Bogue.of_layout layout in
  Bogue.run board

let () =
  main ();
  Bogue.quit ()
