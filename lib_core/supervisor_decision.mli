(* @archlint.module interface
   @archlint.domain supervisor *)

(** Pure lifecycle decisions for long-lived supervisor fibers. *)

type termination = Returned | Cancelled | Quit | Raised of string
[@@deriving show, eq]

type fatal_reason = Returned_unexpectedly | Raised_unexpectedly of string
[@@deriving show, eq]

type decision =
  | Normal_quit
  | Propagate_cancel
  | Fatal of { name : string; reason : fatal_reason }
[@@deriving show, eq]

type cleanup_state = Cleanup_pending | Cleanup_done [@@deriving show, eq]
type exit_decision = Defer_exit_until_cleanup | Exit_now [@@deriving show, eq]

val classify : name:string -> quit_is_normal:bool -> termination -> decision
(** Classify how a supervised long-lived fiber ended. [Returned] is fatal
    because these fibers are expected to run until cancellation or intentional
    TUI quit. *)

val message : name:string -> fatal_reason -> string
(** User-facing fatal supervisor message. *)

val exit_after_fatal : cleanup_state -> exit_decision
(** Fatal supervisor exits are delayed until TUI cleanup has run. *)
