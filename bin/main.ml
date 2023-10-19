let () =
  print_endline "\n\nWelcome to BS.\n";
  print_endline "Press s to start the game: ";
  let x = read_line () in
  match x with
  | "s" -> print_endline "Here are the cards:"
  | _ -> print_endline "Please try again. Press s to start the game: "
