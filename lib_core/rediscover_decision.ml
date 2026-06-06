(* @archlint.module core
   @archlint.domain rediscover-decision *)

open Base
open Types

(** Pure decision layer between [StartupReconciler.discover_pr]'s effectful PR
    lookup and the orchestrator state change. See the [.mli] for the contract.
    The classification depends only on [(in_gameplan, result)] — the [patch_id]
    and [pr_number] flow through for the effectful caller's logs but never
    affect the decision. *)

type replacement = {
  new_pr : Pr_number.t;
  base_branch : Branch.t;
  merged : bool;
}
[@@deriving show, eq]

type input = {
  patch_id : Patch_id.t;
  pr_number : Pr_number.t;
  in_gameplan : bool;
  result : (replacement option, string) Result.t;
}

type classification =
  | Switch_to_pr of replacement
  | Clear_pr_for_recreate
  | Mark_pr_missing
  | Log_error of { message : string }
[@@deriving show, eq]

let classify (input : input) : classification =
  match input.result with
  | Ok (Some r) -> Switch_to_pr r
  | Ok None ->
      if input.in_gameplan then Clear_pr_for_recreate else Mark_pr_missing
  | Error message -> Log_error { message }

type log_decision = Log_emit | Log_skip [@@deriving show, eq]

let classify_vanish_log cls ~already_logged =
  match cls with
  | Mark_pr_missing -> if already_logged then Log_skip else Log_emit
  | Switch_to_pr _ | Clear_pr_for_recreate | Log_error _ -> Log_skip

(* ── Inline tests: one per row of the decision table ── *)

let mk_pid = Patch_id.of_string
let mk_pr = Pr_number.of_int
let mk_branch = Branch.of_string

let sample_replacement =
  { new_pr = mk_pr 42; base_branch = mk_branch "main"; merged = false }

let%test "Ok (Some r) + in_gameplan=true -> Switch_to_pr" =
  match
    classify
      {
        patch_id = mk_pid "p1";
        pr_number = mk_pr 1;
        in_gameplan = true;
        result = Ok (Some sample_replacement);
      }
  with
  | Switch_to_pr r -> equal_replacement r sample_replacement
  | Clear_pr_for_recreate | Mark_pr_missing | Log_error _ -> false

let%test "Ok (Some r) + in_gameplan=false -> Switch_to_pr (ad-hoc adoption)" =
  match
    classify
      {
        patch_id = mk_pid "p1";
        pr_number = mk_pr 1;
        in_gameplan = false;
        result = Ok (Some sample_replacement);
      }
  with
  | Switch_to_pr r -> equal_replacement r sample_replacement
  | Clear_pr_for_recreate | Mark_pr_missing | Log_error _ -> false

let%test "Ok None + in_gameplan=true -> Clear_pr_for_recreate" =
  match
    classify
      {
        patch_id = mk_pid "p1";
        pr_number = mk_pr 1;
        in_gameplan = true;
        result = Ok None;
      }
  with
  | Clear_pr_for_recreate -> true
  | Switch_to_pr _ | Mark_pr_missing | Log_error _ -> false

let%test "Ok None + in_gameplan=false -> Mark_pr_missing (bug-prevention)" =
  match
    classify
      {
        patch_id = mk_pid "p1";
        pr_number = mk_pr 1;
        in_gameplan = false;
        result = Ok None;
      }
  with
  | Mark_pr_missing -> true
  | Switch_to_pr _ | Clear_pr_for_recreate | Log_error _ -> false

let%test "Error _ + in_gameplan=true -> Log_error" =
  match
    classify
      {
        patch_id = mk_pid "p1";
        pr_number = mk_pr 1;
        in_gameplan = true;
        result = Error "boom";
      }
  with
  | Log_error { message } -> String.equal message "boom"
  | Switch_to_pr _ | Clear_pr_for_recreate | Mark_pr_missing -> false

let%test "Error _ + in_gameplan=false -> Log_error" =
  match
    classify
      {
        patch_id = mk_pid "p1";
        pr_number = mk_pr 1;
        in_gameplan = false;
        result = Error "boom";
      }
  with
  | Log_error { message } -> String.equal message "boom"
  | Switch_to_pr _ | Clear_pr_for_recreate | Mark_pr_missing -> false

let%test "classify is deterministic" =
  let input =
    {
      patch_id = mk_pid "p1";
      pr_number = mk_pr 1;
      in_gameplan = false;
      result = Ok None;
    }
  in
  equal_classification (classify input) (classify input)

let%test "classify_vanish_log: Mark_pr_missing first time -> Log_emit" =
  equal_log_decision
    (classify_vanish_log Mark_pr_missing ~already_logged:false)
    Log_emit

let%test "classify_vanish_log: Mark_pr_missing already logged -> Log_skip" =
  equal_log_decision
    (classify_vanish_log Mark_pr_missing ~already_logged:true)
    Log_skip

let%test "classify_vanish_log: Switch_to_pr never emits" =
  equal_log_decision
    (classify_vanish_log (Switch_to_pr sample_replacement) ~already_logged:false)
    Log_skip

let%test "classify_vanish_log: Clear_pr_for_recreate never emits" =
  equal_log_decision
    (classify_vanish_log Clear_pr_for_recreate ~already_logged:false)
    Log_skip

let%test "classify_vanish_log: Log_error never emits" =
  equal_log_decision
    (classify_vanish_log (Log_error { message = "x" }) ~already_logged:false)
    Log_skip
