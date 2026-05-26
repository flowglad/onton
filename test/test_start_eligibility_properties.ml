open Base
open Onton_core

(** Property tests for {!Start_eligibility}.

    Properties:

    - {b SEP-1 Totality}: [decide] never raises on any input.
    - {b SEP-2 Determinism}: same inputs → same output.
    - {b SEP-3 Pre-emption order}: each arm in the documented order is reachable
      and only fires when no earlier arm matches.
    - {b SEP-4 [Allow] ⇒ fresh}: every [Allow] is justified by at least one of:
      base is main, base patch is merged, or the recorded rebased-onto sha
      matches the known main sha.
    - {b SEP-5 [Defer] ⇒ not fresh}: every [Defer] reflects a real freshness gap
      (no main sha, busy rebase, or sha mismatch).
    - {b SEP-6 Label bounds}: [short_label] is non-empty, ≤ 32 chars, lowercase
      snake_case.
    - {b SEP-7 Variant reachability}: every constructor of {!decision} and
      {!defer_reason} is reachable from at least one input. *)

module Gen = QCheck2.Gen
module Test = QCheck2.Test
module SE = Start_eligibility

(* ---------- Generators ---------- *)

let gen_sha = Gen.string_size ~gen:Gen.printable (Gen.int_range 0 40)
let gen_branch = Gen.string_size ~gen:Gen.printable (Gen.int_range 1 30)

type inputs = {
  base_is_main : bool;
  base_branch : string;
  base_patch_merged : bool;
  base_patch_rebased_onto_sha : string option;
  base_patch_busy_rebasing : bool;
  main_sha : string option;
}

let gen_inputs : inputs Gen.t =
  let open Gen in
  let* base_is_main = bool in
  let* base_branch = gen_branch in
  let* base_patch_merged = bool in
  let* base_patch_rebased_onto_sha = option gen_sha in
  let* base_patch_busy_rebasing = bool in
  let* main_sha = option gen_sha in
  return
    {
      base_is_main;
      base_branch;
      base_patch_merged;
      base_patch_rebased_onto_sha;
      base_patch_busy_rebasing;
      main_sha;
    }

(* Pairs where [base_patch_rebased_onto_sha] and [main_sha] match with
   non-trivial probability — the [Allow]-via-sha-equality arm wouldn't get
   exercised by independent generators. *)
let gen_inputs_matching : inputs Gen.t =
  let open Gen in
  let* shared = gen_sha in
  let* base_branch = gen_branch in
  let* base_patch_busy_rebasing = bool in
  return
    {
      base_is_main = false;
      base_branch;
      base_patch_merged = false;
      base_patch_rebased_onto_sha = Some shared;
      base_patch_busy_rebasing;
      main_sha = Some shared;
    }

let call i =
  SE.decide ~base_is_main:i.base_is_main ~base_branch:i.base_branch
    ~base_patch_merged:i.base_patch_merged
    ~base_patch_rebased_onto_sha:i.base_patch_rebased_onto_sha
    ~base_patch_busy_rebasing:i.base_patch_busy_rebasing ~main_sha:i.main_sha

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

let is_defer_main_unknown = function
  | SE.Defer SE.Main_sha_unknown -> true
  | SE.Allow
  | SE.Defer
      ( SE.Base_patch_busy_with_rebase _
      | SE.Base_not_rebased_since_main_advanced _ ) ->
      false

let is_defer_busy = function
  | SE.Defer (SE.Base_patch_busy_with_rebase _) -> true
  | SE.Allow
  | SE.Defer (SE.Main_sha_unknown | SE.Base_not_rebased_since_main_advanced _)
    ->
      false

let is_defer_stale = function
  | SE.Defer (SE.Base_not_rebased_since_main_advanced _) -> true
  | SE.Allow | SE.Defer (SE.Main_sha_unknown | SE.Base_patch_busy_with_rebase _)
    ->
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

let prop_preempt_main_unknown =
  Test.make ~count:200
    ~name:
      "SEP-3c: main_sha=None (and no earlier match) → Defer Main_sha_unknown"
    Gen.(
      let* i = gen_inputs in
      return
        {
          i with
          base_is_main = false;
          base_patch_merged = false;
          main_sha = None;
        })
    (fun i -> is_defer_main_unknown (call i))

let prop_preempt_busy =
  Test.make ~count:200
    ~name:
      "SEP-3d: busy_rebasing (with main_sha known) → Defer \
       Base_patch_busy_with_rebase"
    Gen.(
      let* i = gen_inputs in
      let* sha = gen_sha in
      return
        {
          i with
          base_is_main = false;
          base_patch_merged = false;
          main_sha = Some sha;
          base_patch_busy_rebasing = true;
        })
    (fun i -> is_defer_busy (call i))

let prop_allow_on_sha_equality =
  Test.make ~count:200
    ~name:"SEP-3e: rebased_onto_sha = main_sha (and no earlier match) → Allow"
    gen_inputs_matching (fun i ->
      (* [gen_inputs_matching] sets base_is_main=false,
         base_patch_merged=false, both shas Some &amp; equal. busy_rebasing
         varies. *)
      if i.base_patch_busy_rebasing then is_defer_busy (call i)
      else is_allow (call i))

let prop_defer_stale_when_mismatch =
  Test.make ~count:300
    ~name:
      "SEP-3f: main_sha known, not busy, rebased_onto_sha != main_sha → Defer \
       Base_not_rebased_since_main_advanced"
    Gen.(
      let* main_sha = gen_sha in
      let* base_branch = gen_branch in
      (* Choose rebased_onto_sha to be either None or different from main_sha. *)
      let* rebased =
        oneof
          [
            return None;
            map
              (fun s -> Some (main_sha ^ s ^ "_diff"))
              (string_size ~gen:printable (int_range 1 5));
          ]
      in
      return
        {
          base_is_main = false;
          base_branch;
          base_patch_merged = false;
          base_patch_rebased_onto_sha = rebased;
          base_patch_busy_rebasing = false;
          main_sha = Some main_sha;
        })
    (fun i -> is_defer_stale (call i))

let prop_allow_implies_fresh =
  Test.make ~count:500 ~name:"SEP-4: Allow ⇒ fresh" gen_inputs (fun i ->
      match call i with
      | SE.Defer _ -> true
      | SE.Allow -> (
          i.base_is_main || i.base_patch_merged
          ||
          match (i.base_patch_rebased_onto_sha, i.main_sha) with
          | Some s, Some m -> String.equal s m
          | _ -> false))

let prop_defer_implies_not_fresh =
  Test.make ~count:500 ~name:"SEP-5: Defer ⇒ not fresh" gen_inputs (fun i ->
      match call i with
      | SE.Allow -> true
      | SE.Defer _ ->
          (not i.base_is_main) && (not i.base_patch_merged)
          && (Option.is_none i.main_sha || i.base_patch_busy_rebasing
             ||
             match (i.base_patch_rebased_onto_sha, i.main_sha) with
             | Some s, Some m -> not (String.equal s m)
             | None, _ -> true
             | _, None -> true))

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
          ~base_patch_merged:false ~base_patch_rebased_onto_sha:None
          ~base_patch_busy_rebasing:false ~main_sha:None
      in
      let allow_merged =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:true
          ~base_patch_rebased_onto_sha:None ~base_patch_busy_rebasing:false
          ~main_sha:None
      in
      let allow_sha_eq =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:false
          ~base_patch_rebased_onto_sha:(Some "abc")
          ~base_patch_busy_rebasing:false ~main_sha:(Some "abc")
      in
      let defer_unknown =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:false
          ~base_patch_rebased_onto_sha:(Some "abc")
          ~base_patch_busy_rebasing:false ~main_sha:None
      in
      let defer_busy =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:false
          ~base_patch_rebased_onto_sha:(Some "abc")
          ~base_patch_busy_rebasing:true ~main_sha:(Some "abc")
      in
      let defer_stale =
        SE.decide ~base_is_main:false ~base_branch:"b" ~base_patch_merged:false
          ~base_patch_rebased_onto_sha:(Some "old")
          ~base_patch_busy_rebasing:false ~main_sha:(Some "new")
      in
      is_allow allow_main && is_allow allow_merged && is_allow allow_sha_eq
      && is_defer_main_unknown defer_unknown
      && is_defer_busy defer_busy && is_defer_stale defer_stale)

let () =
  let runner = QCheck_base_runner.run_tests_main in
  ignore
    (runner
       [
         prop_totality;
         prop_determinism;
         prop_preempt_base_is_main;
         prop_preempt_base_merged;
         prop_preempt_main_unknown;
         prop_preempt_busy;
         prop_allow_on_sha_equality;
         prop_defer_stale_when_mismatch;
         prop_allow_implies_fresh;
         prop_defer_implies_not_fresh;
         prop_label_bounds;
         prop_variants_reachable;
       ])
