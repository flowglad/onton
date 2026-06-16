(* @archlint.module core
   @archlint.domain supervisor *)

open Base

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

let classify ~name ~quit_is_normal termination =
  match termination with
  | Cancelled -> Propagate_cancel
  | Quit when quit_is_normal -> Normal_quit
  | Quit ->
      Fatal
        { name; reason = Raised_unexpectedly "TUI quit escaped non-TUI fiber" }
  | Returned -> Fatal { name; reason = Returned_unexpectedly }
  | Raised detail -> Fatal { name; reason = Raised_unexpectedly detail }

let message ~name reason =
  let detail =
    match reason with
    | Returned_unexpectedly -> "returned unexpectedly"
    | Raised_unexpectedly detail -> detail
  in
  Printf.sprintf "Fatal supervisor fiber error (%s) — %s" name detail

let exit_after_fatal = function
  | Cleanup_pending -> Defer_exit_until_cleanup
  | Cleanup_done -> Exit_now

let%test_module "supervisor decision" =
  (module struct
    let%test "normal return is fatal" =
      match
        classify ~name:"poller" ~quit_is_normal:false (Returned : termination)
      with
      | Fatal { reason; _ } -> equal_fatal_reason reason Returned_unexpectedly
      | Normal_quit | Propagate_cancel -> false

    let%test "cancel is propagated" =
      equal_decision
        (classify ~name:"poller" ~quit_is_normal:false Cancelled)
        Propagate_cancel
  end)
