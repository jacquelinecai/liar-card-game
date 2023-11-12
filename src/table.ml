type card = Card.card

type table = {
  mutable table_cards : card list;
  mutable discard_pile : card list;
}

let empty_table : table = { table_cards = []; discard_pile = [] }

let modify_table_cards (t : table) (c : card list) =
  t.table_cards <- c @ t.table_cards

let adding_card_to_table (t : table) (c : card option) =
  let list =
    match c with
    | None -> []
    | Some value -> [ value ]
  in
  modify_table_cards t list

let rec adding_cards_to_table (t:table) (cl: card list) = 
  match cl with
  |[] -> adding_card_to_table t None
  |h::tl -> let () = adding_card_to_table t (Some h) in adding_cards_to_table t tl

let peek_at_table (t : table) : card list = t.table_cards

let discard_cards (t : table) =
  t.discard_pile <- t.table_cards @ t.discard_pile;
  t.table_cards <- []
