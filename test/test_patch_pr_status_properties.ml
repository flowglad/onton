(* @archlint.module test
   @archlint.domain patch-pr-status *)

open Base
open Onton_core
open Onton_core.Types
module Gen = QCheck2.Gen
module Test = QCheck2.Test

(** Property tests for [Patch_pr_status]: legal-only transitions, predicate
    consistency, and yojson round-trips (forward + legacy backward-compat). *)

let gen_pr_number = Gen.(map Pr_number.of_int (int_range 1 9999))
let gen_absent : Patch_pr_status.t Gen.t = Gen.return Patch_pr_status.Absent
let gen_present = Gen.(map (fun n -> Patch_pr_status.Present n) gen_pr_number)
let gen_missing = Gen.(map (fun n -> Patch_pr_status.Missing n) gen_pr_number)
let gen_any = Gen.oneof [ gen_absent; gen_present; gen_missing ]

(* ── Helpers ── *)

let is_absent : Patch_pr_status.t -> bool = function
  | Absent -> true
  | Present _ | Missing _ -> false

let is_present : Patch_pr_status.t -> bool = function
  | Present _ -> true
  | Absent | Missing _ -> false

let is_missing : Patch_pr_status.t -> bool = function
  | Missing _ -> true
  | Absent | Present _ -> false

let raises_invalid_argument f =
  try
    let _ = f () in
    false
  with Invalid_argument _ -> true

(* ── Properties ── *)

let prop_predicates_consistent =
  Test.make ~name:"has_pr / is_pr_present / pr_number agree" ~count:200 gen_any
    (fun t ->
      let h = Patch_pr_status.has_pr t in
      let p = Patch_pr_status.is_pr_present t in
      let m = Patch_pr_status.is_missing t in
      let n = Patch_pr_status.pr_number t in
      (* is_pr_present implies has_pr *)
      ((not p) || h)
      (* has_pr iff pr_number is Some *)
      && Bool.equal h (Option.is_some n)
      (* exactly one of is_present / is_missing / is_absent holds *)
      && (p || m || not h)
      && not (p && m))

let prop_set_present_total =
  Test.make ~name:"set_present is total and lands in Present" ~count:500
    Gen.(pair gen_any gen_pr_number)
    (fun (t, n) ->
      match Patch_pr_status.set_present t n with
      | Present n' -> Pr_number.equal n n'
      | Absent | Missing _ -> false)

let prop_clear_for_recreate_partial =
  Test.make ~name:"clear_for_recreate: Present -> Absent; raises otherwise"
    ~count:200 gen_any (fun t ->
      match t with
      | Present _ -> (
          match Patch_pr_status.clear_for_recreate t with
          | Absent -> true
          | Present _ | Missing _ -> false)
      | Absent | Missing _ ->
          raises_invalid_argument (fun () ->
              Patch_pr_status.clear_for_recreate t))

let prop_mark_missing_partial =
  Test.make
    ~name:"mark_missing: Present n -> Missing n; raises on Absent and Missing"
    ~count:200 gen_any (fun t ->
      match t with
      | Present n -> (
          match Patch_pr_status.mark_missing t with
          | Missing n' -> Pr_number.equal n n'
          | Absent | Present _ -> false)
      | Absent | Missing _ ->
          raises_invalid_argument (fun () -> Patch_pr_status.mark_missing t))

let prop_yojson_roundtrip_forward =
  Test.make ~name:"yojson round-trip preserves equality" ~count:500 gen_any
    (fun t ->
      match
        Patch_pr_status.t_of_yojson_compat (Patch_pr_status.yojson_of_t t)
      with
      | Ok t' -> Patch_pr_status.equal t t'
      | Error _ -> false)

let prop_legacy_null_decodes_to_absent =
  Test.make ~name:"legacy `Null decodes to Absent" ~count:1 (Gen.return ())
    (fun () ->
      match Patch_pr_status.t_of_yojson_compat `Null with
      | Ok Absent -> true
      | Ok (Present _ | Missing _) | Error _ -> false)

let prop_legacy_int_decodes_to_present =
  Test.make ~name:"legacy bare int decodes to Present" ~count:100 gen_pr_number
    (fun n ->
      let int_payload = Pr_number.yojson_of_t n in
      match Patch_pr_status.t_of_yojson_compat int_payload with
      | Ok (Present n') -> Pr_number.equal n n'
      | Ok (Absent | Missing _) | Error _ -> false)

let prop_classify_mark_missing_total =
  Test.make ~name:"classify_mark_missing is total and exhaustive" ~count:200
    gen_any (fun t ->
      match Patch_pr_status.classify_mark_missing t with
      | Mark_missing_already -> Patch_pr_status.is_missing t
      | Mark_missing_transition -> Patch_pr_status.is_pr_present t
      | Mark_missing_illegal -> not (Patch_pr_status.has_pr t)
      (* Absent only *))

let prop_classify_mark_missing_agrees =
  Test.make
    ~name:"classify_mark_missing Transition agrees with mark_missing transition"
    ~count:200 gen_present (fun t ->
      match Patch_pr_status.classify_mark_missing t with
      | Mark_missing_transition ->
          Patch_pr_status.is_missing (Patch_pr_status.mark_missing t)
      | Mark_missing_already | Mark_missing_illegal -> false)

let prop_classify_set_present_total =
  Test.make ~name:"classify_set_present partitions by prior-number-match"
    ~count:300
    Gen.(pair gen_any gen_pr_number)
    (fun (t, n) ->
      let prior_matches =
        Option.equal Pr_number.equal (Patch_pr_status.pr_number t) (Some n)
      in
      match Patch_pr_status.classify_set_present t n with
      | Set_present_recover_same -> prior_matches
      | Set_present_adopt_new -> not prior_matches)

let prop_set_present_idempotent =
  Test.make ~name:"set_present same-number applied twice = once" ~count:200
    Gen.(pair gen_any gen_pr_number)
    (fun (t, n) ->
      let once = Patch_pr_status.set_present t n in
      let twice = Patch_pr_status.set_present once n in
      Patch_pr_status.equal once twice)

let prop_classify_recovery_on_observe_total =
  Test.make ~name:"classify_recovery_on_observe is total" ~count:200 gen_any
    (fun t ->
      match Patch_pr_status.classify_recovery_on_observe t with
      | Lift_to_present n -> (
          match t with
          | Missing n' -> Pr_number.equal n n'
          | Absent | Present _ -> false)
      | No_recovery_needed -> not (Patch_pr_status.is_missing t))

let prop_legacy_never_produces_missing =
  Test.make ~name:"legacy formats never produce Missing" ~count:100
    Gen.(oneof [ return `Null; map Pr_number.yojson_of_t gen_pr_number ])
    (fun legacy_json ->
      match Patch_pr_status.t_of_yojson_compat legacy_json with
      | Ok (Absent | Present _) -> true
      | Ok (Missing _) -> false
      | Error _ -> false)

(* I deliberately avoid declaring an _ pattern in the awareness predicates so
   adding a new variant to [Patch_pr_status.t] forces an update here. *)
let _ = is_absent
let _ = is_present
let _ = is_missing

let suite =
  [
    prop_predicates_consistent;
    prop_set_present_total;
    prop_clear_for_recreate_partial;
    prop_mark_missing_partial;
    prop_yojson_roundtrip_forward;
    prop_legacy_null_decodes_to_absent;
    prop_legacy_int_decodes_to_present;
    prop_legacy_never_produces_missing;
    prop_classify_mark_missing_total;
    prop_classify_mark_missing_agrees;
    prop_classify_set_present_total;
    prop_set_present_idempotent;
    prop_classify_recovery_on_observe_total;
  ]

let () =
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode
