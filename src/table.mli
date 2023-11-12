type card = Card.card

type table = {
  mutable table_cards : card list;
  mutable discard_pile : card list;
}

val empty_table : table
val modify_table_cards : table -> card list -> unit
val adding_card_to_table : table -> card option -> unit
val peek_at_table : table -> card list
val discard_cards : table -> unit
