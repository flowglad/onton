(* @archlint.module interface
   @archlint.domain supervisor *)

(** Pure lifecycle decisions for supervisor fibers. *)

type termination = Returned | Cancelled | Quit | Raised of string
[@@deriving show, eq]

type return_policy = Return_is_fatal | Return_is_normal [@@deriving show, eq]

type fatal_reason = Returned_unexpectedly | Raised_unexpectedly of string
[@@deriving show, eq]

type decision =
  | Normal_return
  | Normal_quit
  | Propagate_cancel
  | Fatal of { name : string; reason : fatal_reason }
[@@deriving show, eq]

type cleanup_state = Cleanup_pending | Cleanup_done [@@deriving show, eq]
type exit_decision = Defer_exit_until_cleanup | Exit_now [@@deriving show, eq]

val classify :
  name:string ->
  quit_is_normal:bool ->
  return_policy:return_policy ->
  termination ->
  decision
(** Classify how a supervised long-lived fiber ended. [Returned] is fatal for
    fibers expected to run until cancellation or intentional TUI quit, and
    normal for one-shot startup fibers. *)

val message : name:string -> fatal_reason -> string
(** User-facing fatal supervisor message. *)

val exit_after_fatal : cleanup_state -> exit_decision
(** Fatal supervisor exits are delayed until TUI cleanup has run. *)
