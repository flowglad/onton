(** Pure keyboard input → TUI command translation.

    Maps {!Term.Key.t} values to semantic TUI commands. Contains no I/O — the
    reading of keys from stdin is the caller's responsibility. *)

type command =
  | Quit
  | Refresh
  | Help
  | Move_up
  | Move_down
  | Page_up
  | Page_down
  | Select
  | Back
  | Noop
  | Timeline
  | Send_message of Types.Patch_id.t * string
  | Add_pr of Types.Pr_number.t
[@@deriving show, eq]

val of_key : Term.Key.t -> command
(** Translate a key press into a TUI command. *)

val apply_move : count:int -> selected:int -> command -> int
(** Apply a navigation command to the selected index.

    Returns the new selected index, clamped to [[0, count-1]]. Non-navigation
    commands clamp [selected] to the valid range (guarding against asynchronous
    count shrinkage). Returns [0] when [count <= 0]. *)

val parse_line : string -> command option
(** Parse a text-mode input line into a command.

    Supported formats:
    - ["N> message"] — send a human message to patch N
    - ["+123"] — register ad-hoc PR for the currently selected patch *)

(** Bounded input history with up/down browsing.

    Stores previously entered lines in a circular buffer. Duplicate consecutive
    entries and empty/whitespace-only lines are silently ignored. *)
module History : sig
  type t

  val create : ?capacity:int -> unit -> t
  (** Create a history buffer. Default capacity is 100. *)

  val push : t -> string -> unit
  (** Record a line. Resets browse position to the bottom. Empty lines and
      consecutive duplicates are ignored. *)

  val older : t -> string option
  (** Move toward older entries. Returns [None] at the oldest. *)

  val newer : t -> string option
  (** Move toward newer entries. Returns [None] when back at the fresh-input
      position (bottom of history). *)

  val reset_browse : t -> unit
  (** Reset browse position to the bottom (fresh-input position). *)

  val is_browsing : t -> bool
  (** [true] when the browse cursor is not at the bottom. *)
end
