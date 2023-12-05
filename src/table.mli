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
(** Modifies the cards on the table by adding cards to the beginning of the
    table. *)

val adding_card_to_table : table -> card option -> unit
(** Adds a singular card onto the table. *)

val adding_cards_to_table : table -> card list -> unit
(** Adds list of cards onto the end of the table. *)

val peek_at_table : table -> card list
(** Returns the list of cards on the table at the current moment. *)

val peek_at_discard_pile : table -> card list
(** Returns the list of discarded cards on the table at the current moment. *)

val table_size : table -> int
(** Returns the size of a table. *)

val discard_cards : table -> unit
(** Adds cards to the discarded pile after a round of the game. *)
