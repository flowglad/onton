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
