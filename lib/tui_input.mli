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
  | Send_message of string
  | Add_pr of Types.Pr_number.t
  | Add_worktree of string
  | Remove_patch
[@@deriving show, eq]

(** Input mode for the TUI prompt. *)
type input_mode =
  | Normal
  | Prompt_pr  (** Buffer holds digits for PR number *)
  | Prompt_worktree  (** Buffer holds path string *)
  | Prompt_message  (** Buffer holds message text, detail view only *)
  | Prompt_broadcast  (** Buffer holds message to send to all active patches *)
[@@deriving show, eq]

val prompt_prefix : input_mode -> string
(** Returns the prompt prefix for each mode (e.g. ["PR #: "], ["> "]). *)

val of_key : Term.Key.t -> command
(** Translate a key press into a TUI command. *)

val apply_move : count:int -> selected:int -> command -> int
(** Apply a navigation command to the selected index.

    Returns the new selected index, clamped to [[0, count-1]]. Non-navigation
    commands clamp [selected] to the valid range (guarding against asynchronous
    count shrinkage). Returns [0] when [count <= 0]. *)

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
