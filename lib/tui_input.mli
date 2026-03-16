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

(** Pure input history with zipper-based navigation.

    Maintains a bounded list of previously entered lines. Navigating up/down
    moves through history while preserving the in-progress draft. *)
module History : sig
  type t [@@deriving show, eq]

  val create : ?max_size:int -> unit -> t
  (** Create an empty history. [max_size] defaults to 100. *)

  val add : string -> t -> t
  (** Record a submitted line. Empty strings and consecutive duplicates are
      ignored. Oldest entries are dropped when [max_size] is exceeded. *)

  val up : current:string -> t -> string * t
  (** Move to the previous (older) entry. Returns the recalled line and updated
      history. On the first [up], [current] is saved as the draft. Returns
      [current] unchanged if already at the oldest entry. *)

  val down : current:string -> t -> string * t
  (** Move to the next (newer) entry. Returns the draft when past the newest
      entry. *)

  val reset : t -> t
  (** Flatten navigation state, keeping all entries but clearing the position
      cursor. Call this after submitting a line. *)

  val entries : t -> string list
  (** All entries in chronological order (oldest first). *)
end
