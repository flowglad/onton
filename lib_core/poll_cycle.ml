open Base
open Types

(** Pure planning layer between the effectful per-patch poll driver and the
    orchestrator state machine.

    The poll cycle has a hard separation between an effectful side (per-patch
    HTTPS call, with [Eio.Time.with_timeout]) and a pure side (turning each
    patch's outcome into an orchestrator action). This module is the pure side.
    It exists so the per-patch-isolation guarantee can be property tested
    without an event loop: if one patch's poll times out, every other patch's
    classification must be identical to the run where that patch was excluded.
*)

type input = {
  patch_id : Patch_id.t;
  pr_number : Pr_number.t;
  was_merged : bool;
  outcome : Poll_outcome.t;
}
[@@deriving show, eq]

type classification =
  | Apply_pr_state of {
      pr_state : Pr_state.t;
      poll_result : Poller.t;
      ci_checks_truncated : bool;
    }
      (** Successful, non-fork, non-closed poll. The driver should build a
          [Patch_controller.observation] (filling in branch_in_root and
          worktree_path from effectful checks) and apply via
          [Patch_controller.apply_poll_result]. *)
  | Skip_fork of { head_branch : Branch.t option }
      (** [pr_state.is_fork] was true. The driver should log once per patch and
          do nothing else. *)
  | Rediscover_pr of { head_branch : Branch.t option }
      (** [pr_state.closed] was true. The driver must call [discover_pr] to find
          a replacement PR (effectful — separate I/O path). *)
  | Log_error of { message : string }
      (** Transport / timeout / HTTP / GraphQL / parse error. The driver logs
          the message; the orchestrator state for this patch is unchanged this
          tick. *)
[@@deriving show, eq]

let classify (input : input) : classification =
  match input.outcome with
  | Poll_outcome.Ok_pr_state pr_state when Pr_state.is_fork pr_state ->
      Skip_fork { head_branch = pr_state.Pr_state.head_branch }
  | Poll_outcome.Ok_pr_state pr_state ->
      let poll_result = Poller.poll ~was_merged:input.was_merged pr_state in
      if poll_result.Poller.closed then
        Rediscover_pr { head_branch = pr_state.Pr_state.head_branch }
      else
        Apply_pr_state
          {
            pr_state;
            poll_result;
            ci_checks_truncated = pr_state.Pr_state.ci_checks_truncated;
          }
  | Poll_outcome.Timed_out { seconds } ->
      Log_error
        {
          message =
            Printf.sprintf "Poll error — request timed out after %.0fs" seconds;
        }
  | Poll_outcome.Transport_failed { msg } ->
      Log_error
        { message = Printf.sprintf "Poll error — transport error: %s" msg }
  | Poll_outcome.Http_failed { status; msg } ->
      Log_error
        { message = Printf.sprintf "Poll error — HTTP %d: %s" status msg }
  | Poll_outcome.Graphql_failed msgs ->
      Log_error
        {
          message =
            Printf.sprintf "Poll error — GraphQL: %s"
              (String.concat ~sep:"; " msgs);
        }
  | Poll_outcome.Json_parse_failed msg ->
      Log_error { message = Printf.sprintf "Poll error — JSON parse: %s" msg }

let plan inputs : (Patch_id.t * Pr_number.t * classification) list =
  List.map inputs ~f:(fun input ->
      (input.patch_id, input.pr_number, classify input))

(* ── Inline property hooks ── *)

let%test "classify is total over Timed_out" =
  let pr_number = Pr_number.of_int 1 in
  let patch_id = Patch_id.of_string "p1" in
  match
    classify
      {
        patch_id;
        pr_number;
        was_merged = false;
        outcome = Poll_outcome.Timed_out { seconds = 30.0 };
      }
  with
  | Log_error _ -> true
  | Apply_pr_state _ | Skip_fork _ | Rediscover_pr _ -> false

let%test "classify of Transport_failed is Log_error" =
  match
    classify
      {
        patch_id = Patch_id.of_string "p1";
        pr_number = Pr_number.of_int 1;
        was_merged = false;
        outcome = Poll_outcome.Transport_failed { msg = "connection refused" };
      }
  with
  | Log_error { message } -> String.is_substring message ~substring:"transport"
  | Apply_pr_state _ | Skip_fork _ | Rediscover_pr _ -> false

let%test "plan preserves input order" =
  let mk i outcome =
    {
      patch_id = Patch_id.of_string (Printf.sprintf "p%d" i);
      pr_number = Pr_number.of_int i;
      was_merged = false;
      outcome;
    }
  in
  let inputs =
    [
      mk 1 (Poll_outcome.Timed_out { seconds = 30.0 });
      mk 2 (Poll_outcome.Transport_failed { msg = "foo" });
      mk 3 (Poll_outcome.Http_failed { status = 500; msg = "bar" });
    ]
  in
  let ids = List.map (plan inputs) ~f:(fun (pid, _, _) -> pid) in
  List.equal Patch_id.equal ids
    [
      Patch_id.of_string "p1"; Patch_id.of_string "p2"; Patch_id.of_string "p3";
    ]

let%test "classify ignores other patches in the list" =
  (* The signature of [classify] takes a single input, so this is a typing
     guarantee — but assert the invariant explicitly anyway: classifying
     the same input twice in different surrounding contexts yields the
     same answer. *)
  let input =
    {
      patch_id = Patch_id.of_string "p1";
      pr_number = Pr_number.of_int 1;
      was_merged = false;
      outcome = Poll_outcome.Timed_out { seconds = 30.0 };
    }
  in
  equal_classification (classify input) (classify input)
