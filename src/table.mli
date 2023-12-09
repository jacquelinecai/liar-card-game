type card = Card.card
(** Type representing a card. *)

type table = {
  mutable table_cards : card list;
  mutable discard_pile : card list;
}
(** Type representing the table in the game. *)

val empty_table : table
(** Represents the empty table. *)

val modify_table_cards : table -> card list -> unit
(** modify_table_cards table [c1; c2; c3] modifies the table by adding cards to
    the front of the table in the order [c1; c2; c3]*)

val adding_card_to_table : table -> card option -> unit
(** Adds a singular card onto the table. *)

val adding_cards_to_table : table -> card list -> unit
(** adding_cards_to_table table [c1; c2; c3] adds cards to the front of the
    table in the order [c3; c2; c1] *)

val peek_at_table : table -> card list
(** Returns the list of cards on the table at the current moment. *)

val peek_at_discard_pile : table -> card list
(** Returns the list of discarded cards on the table at the current moment. *)

val table_size : table -> int
(** Returns the size of a table. *)

val discard_cards : table -> unit
(** Adds cards to the discarded pile after a round of the game. *)
