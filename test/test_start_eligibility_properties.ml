(* @archlint.module test
   @archlint.domain start-eligibility *)

open Base
open Onton_core

(** Property tests for {!Start_eligibility}.

    Freshness here is dependency-scoped: the gate asks whether the base patch's
    local branch has been rebased onto its structurally-correct base
    ([base_structurally_fresh]), NOT whether it is current with the latest
    [origin/main]. An unrelated advance of main never defers a [Start].

    Properties:

    - {b SEP-1 Totality}: [decide] never raises on any input.
    - {b SEP-2 Determinism}: same inputs → same output.
    - {b SEP-3 Pre-emption order}: each arm in the documented order is reachable
      and only fires when no earlier arm matches.
    - {b SEP-4 [Allow] ⇒ fresh}: every [Allow] is justified by at least one of:
      base is main, base patch is merged, or (not busy-rebasing, no unresolved
      conflict, and the base is structurally fresh).
    - {b SEP-5 [Defer] ⇒ not fresh}: every [Defer] reflects a real freshness gap
      (base patch busy rebasing, base resolving a conflict, or base not
      structurally fresh) on a non-main, unmerged base.
    - {b SEP-6 Label bounds}: [short_label] is non-empty, ≤ 32 chars, lowercase
      snake_case.
    - {b SEP-7 Variant reachability}: every constructor of {!decision} and
      {!defer_reason} is reachable from at least one input. *)

module Gen = QCheck2.Gen
module Test = QCheck2.Test
module SE = Start_eligibility

(* ---------- Generators ---------- *)

let gen_branch = Gen.string_size ~gen:Gen.printable (Gen.int_range 1 30)

type inputs = {
  base_is_main : bool;
  base_branch : string;
  base_patch_merged : bool;
  base_patch_busy_rebasing : bool;
  base_patch_has_conflict : bool;
  base_structurally_fresh : bool;
  base_contains_merged_siblings : bool;
}

let gen_inputs : inputs Gen.t =
  let open Gen in
  let* base_is_main = bool in
  let* base_branch = gen_branch in
  let* base_patch_merged = bool in
  let* base_patch_busy_rebasing = bool in
  let* base_patch_has_conflict = bool in
  let* base_structurally_fresh = bool in
  let* base_contains_merged_siblings = bool in
  return
    {
      base_is_main;
      base_branch;
      base_patch_merged;
      base_patch_busy_rebasing;
      base_patch_has_conflict;
      base_structurally_fresh;
      base_contains_merged_siblings;
    }

let call i =
  SE.decide ~base_is_main:i.base_is_main ~base_branch:i.base_branch
    ~base_patch_merged:i.base_patch_merged
    ~base_patch_busy_rebasing:i.base_patch_busy_rebasing
    ~base_patch_has_conflict:i.base_patch_has_conflict
    ~base_structurally_fresh:i.base_structurally_fresh
    ~base_contains_merged_siblings:i.base_contains_merged_siblings

(* ---------- Properties ---------- *)

let prop_totality =
  Test.make ~count:500 ~name:"SEP-1: decide is total" gen_inputs (fun i ->
      try
        let _ : SE.decision = call i in
        true
      with _ -> false)

let prop_determinism =
  Test.make ~count:500 ~name:"SEP-2: same inputs → same output" gen_inputs
    (fun i -> SE.equal_decision (call i) (call i))

let is_allow = function SE.Allow -> true | SE.Defer _ -> false

let is_defer_busy = function
  | SE.Defer (SE.Base_patch_busy_with_rebase _) -> true
  | SE.Allow
  | SE.Defer (SE.Base_resolving_conflict _)
  | SE.Defer (SE.Base_not_fresh_for_cut _)
  | SE.Defer (SE.Base_missing_merged_sibling _) ->
      false

let is_defer_conflict = function
  | SE.Defer (SE.Base_resolving_conflict _) -> true
  | SE.Allow
  | SE.Defer (SE.Base_patch_busy_with_rebase _)
  | SE.Defer (SE.Base_not_fresh_for_cut _)
  | SE.Defer (SE.Base_missing_merged_sibling _) ->
      false

let is_defer_stale = function
  | SE.Defer (SE.Base_not_fresh_for_cut _) -> true
  | SE.Allow
  | SE.Defer (SE.Base_patch_busy_with_rebase _)
  | SE.Defer (SE.Base_resolving_conflict _)
  | SE.Defer (SE.Base_missing_merged_sibling _) ->
      false

let is_defer_missing_sibling = function
  | SE.Defer (SE.Base_missing_merged_sibling _) -> true
  | SE.Allow
  | SE.Defer (SE.Base_patch_busy_with_rebase _)
  | SE.Defer (SE.Base_resolving_conflict _)
  | SE.Defer (SE.Base_not_fresh_for_cut _) ->
      false

let prop_preempt_base_is_main =
  Test.make ~count:200 ~name:"SEP-3a: base_is_main pre-empts everything → Allow"
    Gen.(
      let* i = gen_inputs in
      return { i with base_is_main = true })
    (fun i -> is_allow (call i))

let prop_preempt_base_merged =
  Test.make ~count:200
    ~name:"SEP-3b: base_patch_merged pre-empts (after base_is_main) → Allow"
    Gen.(
      let* i = gen_inputs in
      return { i with base_is_main = false; base_patch_merged = true })
    (fun i -> is_allow (call i))

let prop_preempt_busy =
  Test.make ~count:200
    ~name:"SEP-3c: busy_rebasing → Defer Base_patch_busy_with_rebase"
    Gen.(
      let* i = gen_inputs in
      return
        {
          i with
          base_is_main = false;
          base_patch_merged = false;
          base_patch_busy_rebasing = true;
        })
    (fun i -> is_defer_busy (call i))

let prop_defer_when_conflicted =
  Test.make ~count:300
    ~name:
      "SEP-3c': has_conflict, not busy, non-main, unmerged → Defer \
       Base_resolving_conflict (pre-empts freshness/sibling arms — the \
       conflicted-rebase window of PR #3811)"
    Gen.(
      (* [base_structurally_fresh] and [base_contains_merged_siblings] are left
         random: a conflicted base must defer even when it reads structurally
         fresh (same-name freshen rebase) and the launching patch has no merged
         siblings — exactly the configuration that let patch 5 cut from a
         mid-conflicted-rebase patch 4. *)
      let* i = gen_inputs in
      return
        {
          i with
          base_is_main = false;
          base_patch_merged = false;
          base_patch_busy_rebasing = false;
          base_patch_has_conflict = true;
        })
    (fun i -> is_defer_conflict (call i))

let prop_defer_when_not_fresh =
  Test.make ~count:300
    ~name:
      "SEP-3d: not fresh, not busy, no conflict, non-main, unmerged → Defer \
       Base_not_fresh_for_cut"
    Gen.(
      let* base_branch = gen_branch in
      return
        {
          base_is_main = false;
          base_branch;
          base_patch_merged = false;
          base_patch_busy_rebasing = false;
          base_patch_has_conflict = false;
          base_structurally_fresh = false;
          base_contains_merged_siblings = true;
        })
    (fun i -> is_defer_stale (call i))

let prop_allow_when_fresh =
  Test.make ~count:300
    ~name:
      "SEP-3e: structurally fresh, not busy, no conflict, non-main, unmerged → \
       Allow"
    Gen.(
      let* base_branch = gen_branch in
      return
        {
          base_is_main = false;
          base_branch;
          base_patch_merged = false;
          base_patch_busy_rebasing = false;
          base_patch_has_conflict = false;
          base_structurally_fresh = true;
          base_contains_merged_siblings = true;
        })
    (fun i -> is_allow (call i))

let prop_defer_when_missing_sibling =
  Test.make ~count:300
    ~name:
      "SEP-3f: structurally fresh but missing merged sibling → Defer \
       Base_missing_merged_sibling"
    Gen.(
      let* base_branch = gen_branch in
      return
        {
          base_is_main = false;
          base_branch;
          base_patch_merged = false;
          base_patch_busy_rebasing = false;
          base_patch_has_conflict = false;
          base_structurally_fresh = true;
          base_contains_merged_siblings = false;
        })
    (fun i -> is_defer_missing_sibling (call i))

let prop_stale_preempts_missing_sibling =
  Test.make ~count:300
    ~name:
      "SEP-3g: not structurally fresh pre-empts the sibling gate → \
       Base_not_fresh_for_cut"
    Gen.(
      let* base_branch = gen_branch in
      return
        {
          base_is_main = false;
          base_branch;
          base_patch_merged = false;
          base_patch_busy_rebasing = false;
          base_patch_has_conflict = false;
          base_structurally_fresh = false;
          base_contains_merged_siblings = false;
        })
    (fun i -> is_defer_stale (call i))

let prop_busy_merged_main_preempt_missing_sibling =
  Test.make ~count:400
    ~name:
      "SEP-3h: base_is_main / merged / busy / conflicted each pre-empt the \
       sibling gate"
    Gen.(
      (* missing sibling, but an earlier arm also holds; the earlier arm wins *)
      let* which = int_range 0 3 in
      let* base_branch = gen_branch in
      let base =
        {
          base_is_main = false;
          base_branch;
          base_patch_merged = false;
          base_patch_busy_rebasing = false;
          base_patch_has_conflict = false;
          base_structurally_fresh = true;
          base_contains_merged_siblings = false;
        }
      in
      return
        (match which with
        | 0 -> { base with base_is_main = true }
        | 1 -> { base with base_patch_merged = true }
        | 2 -> { base with base_patch_busy_rebasing = true }
        | _ -> { base with base_patch_has_conflict = true }))
    (fun i -> not (is_defer_missing_sibling (call i)))

let prop_allow_when_contains_siblings =
  Test.make ~count:300
    ~name:"SEP-3i: fresh and contains siblings, non-main, unmerged → Allow"
    Gen.(
      let* base_branch = gen_branch in
      return
        {
          base_is_main = false;
          base_branch;
          base_patch_merged = false;
          base_patch_busy_rebasing = false;
          base_patch_has_conflict = false;
          base_structurally_fresh = true;
          base_contains_merged_siblings = true;
        })
    (fun i -> is_allow (call i))

let prop_allow_implies_fresh =
  Test.make ~count:500 ~name:"SEP-4: Allow ⇒ fresh" gen_inputs (fun i ->
      match call i with
      | SE.Defer _ -> true
      | SE.Allow ->
          i.base_is_main || i.base_patch_merged
          || (not i.base_patch_busy_rebasing)
             && (not i.base_patch_has_conflict)
             && i.base_structurally_fresh && i.base_contains_merged_siblings)

let prop_defer_implies_not_fresh =
  Test.make ~count:500 ~name:"SEP-5: Defer ⇒ not fresh" gen_inputs (fun i ->
      match call i with
      | SE.Allow -> true
      | SE.Defer _ ->
          (not i.base_is_main) && (not i.base_patch_merged)
          && (i.base_patch_busy_rebasing || i.base_patch_has_conflict
             || (not i.base_structurally_fresh)
             || not i.base_contains_merged_siblings))

let label_re = Re.compile (Re.Pcre.re "^[a-z][a-z0-9_]*$")

let prop_label_bounds =
  Test.make ~count:500
    ~name:"SEP-6: short_label is non-empty, ≤32 chars, snake_case" gen_inputs
    (fun i ->
      let lbl = SE.short_label (call i) in
      String.length lbl > 0 && String.length lbl <= 32 && Re.execp label_re lbl)

let prop_variants_reachable =
  Test.make ~count:1
    ~name:"SEP-7: every decision/defer_reason variant reachable" (Gen.return ())
    (fun () ->
      let allow_main =
        SE.decide ~base_is_main:true ~base_branch:"main"
          ~base_patch_merged:false ~base_patch_busy_rebasing:false
          ~base_patch_has_conflict:false ~base_structurally_fresh:false
          ~base_contains_merged_siblings:true
      in
      let allow_merged =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:true
          ~base_patch_busy_rebasing:false ~base_patch_has_conflict:false
          ~base_structurally_fresh:false ~base_contains_merged_siblings:true
      in
      let allow_fresh =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:false
          ~base_patch_busy_rebasing:false ~base_patch_has_conflict:false
          ~base_structurally_fresh:true ~base_contains_merged_siblings:true
      in
      let defer_busy =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:false
          ~base_patch_busy_rebasing:true ~base_patch_has_conflict:false
          ~base_structurally_fresh:true ~base_contains_merged_siblings:true
      in
      let defer_conflict =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:false
          ~base_patch_busy_rebasing:false ~base_patch_has_conflict:true
          ~base_structurally_fresh:true ~base_contains_merged_siblings:true
      in
      let defer_stale =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:false
          ~base_patch_busy_rebasing:false ~base_patch_has_conflict:false
          ~base_structurally_fresh:false ~base_contains_merged_siblings:true
      in
      let defer_missing_sibling =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:false
          ~base_patch_busy_rebasing:false ~base_patch_has_conflict:false
          ~base_structurally_fresh:true ~base_contains_merged_siblings:false
      in
      is_allow allow_main && is_allow allow_merged && is_allow allow_fresh
      && is_defer_busy defer_busy
      && is_defer_conflict defer_conflict
      && is_defer_stale defer_stale
      && is_defer_missing_sibling defer_missing_sibling)

let () =
  let runner = QCheck_base_runner.run_tests_main in
  ignore
    (runner
       [
         prop_totality;
         prop_determinism;
         prop_preempt_base_is_main;
         prop_preempt_base_merged;
         prop_preempt_busy;
         prop_defer_when_conflicted;
         prop_defer_when_not_fresh;
         prop_allow_when_fresh;
         prop_defer_when_missing_sibling;
         prop_stale_preempts_missing_sibling;
         prop_busy_merged_main_preempt_missing_sibling;
         prop_allow_when_contains_siblings;
         prop_allow_implies_fresh;
         prop_defer_implies_not_fresh;
         prop_label_bounds;
         prop_variants_reachable;
       ])
