(** Determines the cards on the table *)

type card = Card.card
(** [card] represents a card *)

type table = {
  mutable table_cards : card list;
  mutable discard_pile : card list;
}
(** [table] represents the table in the game *)

val empty_table : table
(** [empty_table] represents the empty table *)

val modify_table_cards : table -> card list -> unit
(** [modify_table_cards table [c1; c2; c3]] modifies the table by adding cards
    to the front of the table in the order [c1; c2; c3] *)

val adding_card_to_table : table -> card option -> unit
(** [adding_card_to_table table (Some c)] adds a singular card onto [table] *)

val adding_cards_to_table : table -> card list -> unit
(** [adding_cards_to_table table [c1; c2; c3]] adds cards to the front of
    [table] in the order [c3; c2; c1] *)

val peek_at_table : table -> card list
(** [peek_at_table table] returns the list of cards on [table] at the current
    moment *)

val peek_at_discard_pile : table -> card list
(** [peek_at_discard_pile table] returns the list of discarded cards on [table]
    at the current moment *)

val table_size : table -> int
(** [table_size table] returns the size of [table] *)

val discard_cards : table -> unit
(** [discard_cards table] adds cards to the discarded pile after a round of the
    game *)
