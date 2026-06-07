(* @archlint.module test
   @archlint.domain patch-agent *)

(** Regression tests for [Tui.human_intervention_reason].

    The misleading-banner bug: a patch pushed into needs-intervention by a high
    CI-failure count rendered "runner: pushed after session" — the most recent
    (and innocuous) activity-log line — instead of the actionable reason. The
    fix makes the banner derive from the agent's own failure counters via
    [Patch_agent.intervention_reason]. These tests pin that: the human reason is
    present exactly when the agent needs intervention, and a CI-stuck agent
    reports the CI failure, never a push event. *)

open Onton
open Onton_core
open Onton_core.Types

let agent () =
  Patch_agent.create ~branch:(Branch.of_string "b")
    (Patch_id.of_string "patch-1")

let rec apply n f x = if n <= 0 then x else apply (n - 1) f (f x)

let contains s sub =
  let s = String.lowercase_ascii s and sub = String.lowercase_ascii sub in
  let n = String.length s and m = String.length sub in
  let rec go i =
    i + m <= n && (String.equal (String.sub s i m) sub || go (i + 1))
  in
  m = 0 || go 0

let () =
  (* A healthy agent needs no intervention and has no banner reason. *)
  let a = agent () in
  assert (not (Patch_agent.needs_intervention a));
  assert (Option.is_none (Tui.human_intervention_reason a));

  (* Three CI failures is the documented threshold for intervention. *)
  let stuck = apply 3 Patch_agent.increment_ci_failure_count a in
  assert (Patch_agent.needs_intervention stuck);
  (match Tui.human_intervention_reason stuck with
  | None -> assert false
  | Some msg ->
      (* The reason names the CI failure and its count... *)
      assert (contains msg "ci");
      assert (contains msg "3");
      (* ...and never the innocuous push event that triggered the bug. *)
      assert (not (contains msg "pushed")));

  (* The human reason is present exactly when intervention is needed: the two
     stay in lockstep with the authoritative predicate, so the banner can't
     show a reason for a healthy patch (or hide one for a stuck patch). *)
  assert (
    Bool.equal
      (Patch_agent.needs_intervention stuck)
      (Option.is_some (Tui.human_intervention_reason stuck)));
  assert (
    Bool.equal
      (Patch_agent.needs_intervention a)
      (Option.is_some (Tui.human_intervention_reason a)));

  print_endline "PASS: human_intervention_reason surfaces the actionable reason"
