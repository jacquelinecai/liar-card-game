let () =
  print_endline "\n\nWelcome to BS.\n";

  let y = ref false in
  while not !y do
    print_endline "Press s to start the game: ";
    let x = read_line () in
    if x = "s" then y := true
  done
;;

print_endline "Here are the cards:"
