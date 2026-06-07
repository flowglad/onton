(* @archlint.module interface
   @archlint.domain tui-input *)

include module type of Tui_input_decision

(** Bounded input history with up/down browsing.

    Stores previously entered lines in a circular buffer. Duplicate consecutive
    entries and empty/whitespace-only lines are silently ignored. *)
module History : sig
  type t

  type newer_result =
    | At_fresh
    | Entry of string
        (** Result of {!newer}. [At_fresh] means the browse position is at the
            fresh-input slot (either already there or just moved back).
            [Entry s] is a history line. *)

  val create : ?capacity:int -> unit -> t
  (** Create a history buffer. Default capacity is 50. *)

  val push : t -> string -> unit
  (** Record a line. Empty lines are ignored (browse position unchanged);
      consecutive duplicates reset the browse position but are not stored. *)

  val older : t -> string option
  (** Move toward older entries. Returns [None] at the oldest. *)

  val newer : t -> newer_result
  (** Move toward newer entries. Returns [At_fresh] when at the fresh-input
      position (whether already there or just arrived). *)

  val reset_browse : t -> unit
  (** Reset browse position to the bottom (fresh-input position). *)

  val is_browsing : t -> bool
  (** [true] when the browse cursor is not at the bottom. *)
end

(** Cursor-aware single-line editing buffer.

    Tracks a cursor position within the text and supports insert, delete, and
    movement operations at the cursor. *)
module Edit_buffer : sig
  type t

  val create : unit -> t
  val contents : t -> string
  val cursor : t -> int
  val length : t -> int
  val clear : t -> unit
  val set : t -> string -> unit
  val insert_char : t -> char -> unit
  val insert_string : t -> string -> unit
  val delete_before : t -> unit
  val delete_at : t -> unit
  val move_left : t -> unit
  val move_right : t -> unit
  val move_home : t -> unit
  val move_end : t -> unit
  val kill_to_end : t -> string
  val kill_to_start : t -> string
end
