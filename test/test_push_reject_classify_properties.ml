(* @archlint.module test
   @archlint.domain push-reject-classify *)

open Base
open Onton_core

(** Property tests for {!Push_reject_classify}.

    The module classifies the [git push] stderr blob into one of a small set of
    semantic rejection reasons. Properties cover:

    - {b Totality}: never raises over arbitrary input.
    - {b Recognizers}: each known fingerprint maps to its expected variant, and
      the recognizer order is consistent (workflow_scope is matched before the
      catch-all hook_failure even when [remote:] lines co-occur).
    - {b Permanence}: [is_permanent] is a pure projection consistent with the
      variant constructor.
    - {b Labels}: [short_label] is non-empty and bounded; [detail_excerpt]
      truncates to ≤ 200 chars and returns [None] for the carrier-less variants.

    The recognizer fingerprints are drawn from real GitHub remote-reject
    messages observed in the wild — see the comment block in
    [push_reject_classify.ml]. *)

module Gen = QCheck2.Gen
module Test = QCheck2.Test

(* ---------- Generators ---------- *)

(* Random printable strings up to a small size, used to push the classifier
   into its catch-all [Unknown] / [Hook_failure] arms. *)
let gen_arb_text = Gen.string_size ~gen:Gen.printable (Gen.int_range 0 200)

(* Known stderr blobs, each tagged with the expected variant. The blobs mirror
   what GitHub actually returns; small variations (capitalization, extra
   "remote:" lines, surrounding `! [remote rejected]` trailer) are layered in
   property bodies that wrap with optional decoration. *)
let workflow_scope_stderr =
  "remote: error: GH013: Repository rule violations found for refs/heads/foo.\n\
   remote: refusing to allow an OAuth App to create or update workflow \
   `.github/workflows/ci.yml` without `workflow` scope\n\
   To https://github.com/o/r.git\n\
   ! [remote rejected]   foo -> foo (refusing to allow an OAuth App to create \
   or update workflow `.github/workflows/ci.yml` without `workflow` scope)"

let branch_protection_stderr =
  "remote: error: GH006: Protected branch update failed for refs/heads/main.\n\
   remote: error: At least 1 approving review is required by reviewers with \
   write access.\n\
   To https://github.com/o/r.git\n\
   ! [remote rejected]   main -> main (protected branch hook declined)"

let push_pattern_stderr =
  "remote: error: GH013: Repository rule violations found for refs/heads/foo.\n\
   remote: - push declined due to repository rule violations\n\
   To https://github.com/o/r.git\n\
   ! [remote rejected]   foo -> foo (push declined due to repository rule \
   violations)"

let lease_violation_stderr =
  "To https://github.com/o/r.git\n\
   ! [rejected]   foo -> foo (stale info)\n\
   error: failed to push some refs to 'https://github.com/o/r.git'\n\
   hint: Updates were rejected because the remote contains work that you do\n\
   hint: not have locally. Try git fetch first."

let generic_hook_stderr =
  "remote: error: server-side hook declined\n\
   remote: see https://example.com/policy for details\n\
   To https://github.com/o/r.git\n\
   ! [remote rejected]   foo -> foo (pre-receive hook declined)"

(* The merge-queue head-branch lock. Deliberately contains BOTH
   [Branch_protection] fingerprints (the GH006 header and the hook-declined
   trailer) around the queue-specific lines, exactly as GitHub emits it — the
   recognizer-order property PRC-17 depends on that. *)
let merge_queue_locked_stderr =
  "remote: error: GH006: Protected branch update failed for refs/heads/foo.\n\
   remote: error: A pull request for this branch has been added to a merge \
   queue. Branches that\n\
   remote: are queued for merging cannot be updated. To modify this branch, \
   dequeue the\n\
   remote: associated pull request.\n\
   To https://github.com/o/r.git\n\
   ! [remote rejected]   foo -> foo (protected branch hook declined)"

(* ---------- Properties ---------- *)

let prop_totality =
  Test.make ~count:500 ~name:"PRC-1: classify is total over arbitrary input"
    (Gen.pair gen_arb_text gen_arb_text) (fun (stderr, stdout) ->
      try
        let _ : Push_reject_classify.rejection =
          Push_reject_classify.classify ~stderr ~stdout
        in
        true
      with _ -> false)

let prop_workflow_scope =
  Test.make ~name:"PRC-2: workflow-scope fingerprint -> Workflow_scope_missing"
    (Gen.return ()) (fun () ->
      Push_reject_classify.equal_rejection
        (Push_reject_classify.classify ~stderr:workflow_scope_stderr ~stdout:"")
        Push_reject_classify.Workflow_scope_missing)

let prop_branch_protection =
  Test.make ~name:"PRC-3: GH006 fingerprint -> Branch_protection"
    (Gen.return ()) (fun () ->
      Push_reject_classify.equal_rejection
        (Push_reject_classify.classify ~stderr:branch_protection_stderr
           ~stdout:"")
        Push_reject_classify.Branch_protection)

let prop_push_pattern =
  Test.make ~name:"PRC-4: repository rule violations -> Push_pattern_block"
    (Gen.return ()) (fun () ->
      Push_reject_classify.equal_rejection
        (Push_reject_classify.classify ~stderr:push_pattern_stderr ~stdout:"")
        Push_reject_classify.Push_pattern_block)

let prop_lease =
  Test.make ~name:"PRC-5: stale info / fetch first -> Lease_violation"
    (Gen.return ()) (fun () ->
      Push_reject_classify.equal_rejection
        (Push_reject_classify.classify ~stderr:lease_violation_stderr ~stdout:"")
        Push_reject_classify.Lease_violation)

let prop_generic_hook =
  Test.make ~name:"PRC-6: unrecognized remote: line -> Hook_failure"
    (Gen.return ()) (fun () ->
      match
        Push_reject_classify.classify ~stderr:generic_hook_stderr ~stdout:""
      with
      | Push_reject_classify.Hook_failure _ -> true
      | Push_reject_classify.Workflow_scope_missing
      | Push_reject_classify.Branch_protection
      | Push_reject_classify.Push_pattern_block
      | Push_reject_classify.Lease_violation
      | Push_reject_classify.Merge_queue_locked | Push_reject_classify.Unknown _
      | Push_reject_classify.Local_state_unsafe _ ->
          false)

let prop_empty_stderr =
  Test.make ~name:"PRC-7: empty stderr -> Unknown \"\"" (Gen.return ())
    (fun () ->
      match Push_reject_classify.classify ~stderr:"" ~stdout:"" with
      | Push_reject_classify.Unknown s -> String.is_empty s
      | Push_reject_classify.Workflow_scope_missing
      | Push_reject_classify.Branch_protection
      | Push_reject_classify.Push_pattern_block
      | Push_reject_classify.Lease_violation
      | Push_reject_classify.Merge_queue_locked
      | Push_reject_classify.Hook_failure _
      | Push_reject_classify.Local_state_unsafe _ ->
          false)

let prop_short_label_bounded =
  Test.make ~count:300 ~name:"PRC-8: short_label is non-empty and <= 32 chars"
    (Gen.pair gen_arb_text gen_arb_text) (fun (stderr, stdout) ->
      let r = Push_reject_classify.classify ~stderr ~stdout in
      let label = Push_reject_classify.short_label r in
      (not (String.is_empty label)) && String.length label <= 32)

let prop_detail_excerpt_bounded =
  Test.make ~count:300 ~name:"PRC-9: detail_excerpt truncates to <= 200 chars"
    (Gen.pair gen_arb_text gen_arb_text) (fun (stderr, stdout) ->
      let r = Push_reject_classify.classify ~stderr ~stdout in
      match Push_reject_classify.detail_excerpt r with
      | None -> true
      | Some s -> String.length s <= 200)

let prop_detail_excerpt_none_for_named =
  Test.make ~name:"PRC-10: detail_excerpt is None for the named variants"
    (Gen.return ()) (fun () ->
      Option.is_none
        (Push_reject_classify.detail_excerpt
           Push_reject_classify.Workflow_scope_missing)
      && Option.is_none
           (Push_reject_classify.detail_excerpt
              Push_reject_classify.Branch_protection)
      && Option.is_none
           (Push_reject_classify.detail_excerpt
              Push_reject_classify.Push_pattern_block)
      && Option.is_none
           (Push_reject_classify.detail_excerpt
              Push_reject_classify.Lease_violation)
      && Option.is_none
           (Push_reject_classify.detail_excerpt
              Push_reject_classify.Merge_queue_locked))

let prop_permanence_matches_variant =
  Test.make ~count:300
    ~name:"PRC-11: is_permanent matches the variant constructor"
    (Gen.pair gen_arb_text gen_arb_text) (fun (stderr, stdout) ->
      let r = Push_reject_classify.classify ~stderr ~stdout in
      let expected =
        match r with
        | Push_reject_classify.Workflow_scope_missing
        | Push_reject_classify.Branch_protection
        | Push_reject_classify.Push_pattern_block
        | Push_reject_classify.Hook_failure _
        | Push_reject_classify.Local_state_unsafe _ ->
            true
        | Push_reject_classify.Lease_violation
        | Push_reject_classify.Merge_queue_locked
        | Push_reject_classify.Unknown _ ->
            false
      in
      Bool.equal (Push_reject_classify.is_permanent r) expected)

let prop_classify_is_deterministic =
  Test.make ~count:300 ~name:"PRC-12: classify is deterministic"
    (Gen.pair gen_arb_text gen_arb_text) (fun (stderr, stdout) ->
      Push_reject_classify.equal_rejection
        (Push_reject_classify.classify ~stderr ~stdout)
        (Push_reject_classify.classify ~stderr ~stdout))

let prop_recognizer_priority =
  Test.make
    ~name:
      "PRC-13: workflow-scope fingerprint beats the generic hook-failure arm \
       even when both [remote:] lines appear" (Gen.return ()) (fun () ->
      (* The workflow-scope blob already starts with a GH013 "rule violations"
         line. Verify the more-specific recognizer wins over the catch-all. *)
      Push_reject_classify.equal_rejection
        (Push_reject_classify.classify ~stderr:workflow_scope_stderr ~stdout:"")
        Push_reject_classify.Workflow_scope_missing)

let prop_local_state_unsafe =
  Test.make ~count:1
    ~name:
      "PRC-15: Local_state_unsafe is permanent, has short_label and \
       detail_excerpt" (Gen.return ()) (fun () ->
      let r =
        Push_reject_classify.Local_state_unsafe
          { reason = "  " ^ String.make 300 'x' }
      in
      Push_reject_classify.is_permanent r
      && String.equal (Push_reject_classify.short_label r) "local_state_unsafe"
      &&
      match Push_reject_classify.detail_excerpt r with
      | Some s -> String.length s = 200 && String.for_all s ~f:(Char.equal 'x')
      | None -> false)

let prop_unknown_truncation =
  Test.make ~count:300 ~name:"PRC-14: Unknown payload is <= 200 chars"
    (Gen.string_size ~gen:Gen.char (Gen.int_range 0 1000))
    (fun garbage ->
      (* Strip any recognizable fingerprints by stripping ASCII letters down
         to a sentinel before classifying — this forces the Unknown arm
         regardless of the random bytes generated. *)
      let sanitized =
        String.map garbage ~f:(fun c -> if Char.is_alpha c then '.' else c)
      in
      match Push_reject_classify.classify ~stderr:sanitized ~stdout:"" with
      | Push_reject_classify.Unknown s -> String.length s <= 200
      | Push_reject_classify.Hook_failure s -> String.length s <= 200
      | Push_reject_classify.Workflow_scope_missing
      | Push_reject_classify.Branch_protection
      | Push_reject_classify.Push_pattern_block
      | Push_reject_classify.Lease_violation
      | Push_reject_classify.Merge_queue_locked
      | Push_reject_classify.Local_state_unsafe _ ->
          (* sanitization stripped all letters, so the named-fingerprint arms
             and the planner-only Local_state_unsafe arm are unreachable here
             — but the type system can't know that. *)
          true)

let prop_merge_queue_locked =
  Test.make ~name:"PRC-16: merge-queue lock fingerprint -> Merge_queue_locked"
    (Gen.return ()) (fun () ->
      Push_reject_classify.equal_rejection
        (Push_reject_classify.classify ~stderr:merge_queue_locked_stderr
           ~stdout:"")
        Push_reject_classify.Merge_queue_locked)

let prop_merge_queue_beats_branch_protection =
  Test.make
    ~name:
      "PRC-17: merge-queue lock beats Branch_protection despite carrying both \
       GH006 fingerprints, and the plain GH006 blob still classifies \
       Branch_protection" (Gen.return ()) (fun () ->
      Push_reject_classify.equal_rejection
        (Push_reject_classify.classify ~stderr:merge_queue_locked_stderr
           ~stdout:"")
        Push_reject_classify.Merge_queue_locked
      && Push_reject_classify.equal_rejection
           (Push_reject_classify.classify ~stderr:branch_protection_stderr
              ~stdout:"")
           Push_reject_classify.Branch_protection)

let () =
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_totality;
      prop_workflow_scope;
      prop_branch_protection;
      prop_push_pattern;
      prop_lease;
      prop_generic_hook;
      prop_empty_stderr;
      prop_short_label_bounded;
      prop_detail_excerpt_bounded;
      prop_detail_excerpt_none_for_named;
      prop_permanence_matches_variant;
      prop_classify_is_deterministic;
      prop_recognizer_priority;
      prop_local_state_unsafe;
      prop_unknown_truncation;
      prop_merge_queue_locked;
      prop_merge_queue_beats_branch_protection;
    ];
  Stdlib.print_endline "Push_reject_classify: all properties passed"
