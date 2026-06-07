(* @archlint.module core
   @archlint.domain patch-pr-status *)

open Base
open Types

type t = Absent | Present of Pr_number.t | Missing of Pr_number.t
[@@deriving show, eq, sexp_of, compare]

let has_pr = function Absent -> false | Present _ | Missing _ -> true
let is_pr_present = function Present _ -> true | Absent | Missing _ -> false
let is_missing = function Missing _ -> true | Absent | Present _ -> false
let pr_number = function Absent -> None | Present n | Missing n -> Some n
let set_present _t n = Present n

let clear_for_recreate = function
  | Present _ -> Absent
  | Absent -> invalid_arg "Patch_pr_status.clear_for_recreate: already Absent"
  | Missing _ ->
      invalid_arg
        "Patch_pr_status.clear_for_recreate: cannot erase Missing; use \
         remove_agent if the patch is truly gone"

let mark_missing = function
  | Present n -> Missing n
  | Absent ->
      invalid_arg "Patch_pr_status.mark_missing: cannot mark Absent as Missing"
  | Missing _ ->
      invalid_arg
        "Patch_pr_status.mark_missing: already Missing — caller should guard"

(* ── Pure classifiers ── *)

type mark_missing_decision =
  | Mark_missing_already
  | Mark_missing_transition
  | Mark_missing_illegal
[@@deriving show, eq]

let classify_mark_missing = function
  | Missing _ -> Mark_missing_already
  | Present _ -> Mark_missing_transition
  | Absent -> Mark_missing_illegal

type set_present_decision = Set_present_recover_same | Set_present_adopt_new
[@@deriving show, eq]

let classify_set_present t pr_number =
  match t with
  | Present n when Pr_number.equal n pr_number -> Set_present_recover_same
  | Missing n when Pr_number.equal n pr_number -> Set_present_recover_same
  | Absent | Present _ | Missing _ -> Set_present_adopt_new

type recovery_decision = Lift_to_present of Pr_number.t | No_recovery_needed
[@@deriving show, eq]

let classify_recovery_on_observe = function
  | Missing n -> Lift_to_present n
  | Absent | Present _ -> No_recovery_needed

(* ── Persistence ── *)

let yojson_of_t = function
  | Absent -> `Assoc [ ("kind", `String "absent") ]
  | Present n ->
      `Assoc
        [ ("kind", `String "present"); ("pr_number", Pr_number.yojson_of_t n) ]
  | Missing n ->
      `Assoc
        [ ("kind", `String "missing"); ("pr_number", Pr_number.yojson_of_t n) ]

let t_of_yojson_compat (json : Yojson.Safe.t) : (t, string) Result.t =
  let decode_pr_number v =
    match Json.try_of_yojson Pr_number.t_of_yojson v with
    | Ok _ as ok -> ok
    | Error msg -> Error (Printf.sprintf "malformed pr_number: %s" msg)
  in
  match json with
  | `Null ->
      (* Legacy form: pr_number = null *)
      Ok Absent
  | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "kind" with
      | None ->
          (* No "kind" tag — not the new form, not a primitive either. Probably
             malformed; surface the structure to the operator. *)
          Error
            (Printf.sprintf "Patch_pr_status: unexpected object shape: %s"
               (Yojson.Safe.to_string json))
      | Some (`String "absent") -> Ok Absent
      | Some (`String "present") -> (
          match List.Assoc.find fields ~equal:String.equal "pr_number" with
          | None -> Error "Patch_pr_status: present without pr_number"
          | Some v -> Result.map (decode_pr_number v) ~f:(fun n -> Present n))
      | Some (`String "missing") -> (
          match List.Assoc.find fields ~equal:String.equal "pr_number" with
          | None -> Error "Patch_pr_status: missing without pr_number"
          | Some v -> Result.map (decode_pr_number v) ~f:(fun n -> Missing n))
      | Some other ->
          Error
            (Printf.sprintf "Patch_pr_status: unknown kind %s"
               (Yojson.Safe.to_string other)))
  | other ->
      (* Legacy form: bare integer (or whatever Pr_number accepts) → Present. *)
      Result.map (decode_pr_number other) ~f:(fun n -> Present n)

(* ── Inline smoke tests ── *)

let mk_pr n = Pr_number.of_int n

let%test "has_pr / is_pr_present / is_missing agreement" =
  (not (has_pr Absent))
  && (not (is_pr_present Absent))
  && (not (is_missing Absent))
  && has_pr (Present (mk_pr 1))
  && is_pr_present (Present (mk_pr 1))
  && (not (is_missing (Present (mk_pr 1))))
  && has_pr (Missing (mk_pr 1))
  && (not (is_pr_present (Missing (mk_pr 1))))
  && is_missing (Missing (mk_pr 1))

let%test "pr_number lifts Present and Missing, drops Absent" =
  Option.is_none (pr_number Absent)
  && Option.equal Pr_number.equal
       (pr_number (Present (mk_pr 7)))
       (Some (mk_pr 7))
  && Option.equal Pr_number.equal
       (pr_number (Missing (mk_pr 7)))
       (Some (mk_pr 7))

let%test "set_present is total and lands in Present" =
  let n = mk_pr 42 in
  equal (set_present Absent n) (Present n)
  && equal (set_present (Present (mk_pr 1)) n) (Present n)
  && equal (set_present (Missing (mk_pr 1)) n) (Present n)

let%test "clear_for_recreate: Present -> Absent" =
  equal (clear_for_recreate (Present (mk_pr 1))) Absent

let%test "clear_for_recreate raises on Absent" =
  try
    let _ = clear_for_recreate Absent in
    false
  with Invalid_argument _ -> true

let%test "clear_for_recreate raises on Missing" =
  try
    let _ = clear_for_recreate (Missing (mk_pr 1)) in
    false
  with Invalid_argument _ -> true

let%test "mark_missing: Present n -> Missing n preserves number" =
  let n = mk_pr 99 in
  equal (mark_missing (Present n)) (Missing n)

let%test "mark_missing raises on Absent" =
  try
    let _ = mark_missing Absent in
    false
  with Invalid_argument _ -> true

let%test "mark_missing raises on Missing" =
  try
    let _ = mark_missing (Missing (mk_pr 1)) in
    false
  with Invalid_argument _ -> true

let%test "classify_mark_missing: Present -> Transition" =
  equal_mark_missing_decision
    (classify_mark_missing (Present (mk_pr 1)))
    Mark_missing_transition

let%test "classify_mark_missing: Missing -> Already" =
  equal_mark_missing_decision
    (classify_mark_missing (Missing (mk_pr 1)))
    Mark_missing_already

let%test "classify_mark_missing: Absent -> Illegal" =
  equal_mark_missing_decision
    (classify_mark_missing Absent)
    Mark_missing_illegal

let%test "classify_mark_missing agrees with mark_missing on Transition" =
  let s = Present (mk_pr 5) in
  match classify_mark_missing s with
  | Mark_missing_transition -> equal (mark_missing s) (Missing (mk_pr 5))
  | Mark_missing_already | Mark_missing_illegal -> false

let%test "classify_set_present: Absent + any n -> Adopt_new" =
  equal_set_present_decision
    (classify_set_present Absent (mk_pr 1))
    Set_present_adopt_new

let%test "classify_set_present: Present n + same n -> Recover_same" =
  equal_set_present_decision
    (classify_set_present (Present (mk_pr 7)) (mk_pr 7))
    Set_present_recover_same

let%test "classify_set_present: Present n + different m -> Adopt_new" =
  equal_set_present_decision
    (classify_set_present (Present (mk_pr 7)) (mk_pr 8))
    Set_present_adopt_new

let%test "classify_set_present: Missing n + same n -> Recover_same" =
  equal_set_present_decision
    (classify_set_present (Missing (mk_pr 7)) (mk_pr 7))
    Set_present_recover_same

let%test "classify_set_present: Missing n + different m -> Adopt_new" =
  equal_set_present_decision
    (classify_set_present (Missing (mk_pr 7)) (mk_pr 8))
    Set_present_adopt_new

let%test "classify_recovery_on_observe: Missing n -> Lift n" =
  equal_recovery_decision
    (classify_recovery_on_observe (Missing (mk_pr 9)))
    (Lift_to_present (mk_pr 9))

let%test "classify_recovery_on_observe: Present -> No_recovery" =
  equal_recovery_decision
    (classify_recovery_on_observe (Present (mk_pr 9)))
    No_recovery_needed

let%test "classify_recovery_on_observe: Absent -> No_recovery" =
  equal_recovery_decision
    (classify_recovery_on_observe Absent)
    No_recovery_needed

let roundtrip_ok t =
  match t_of_yojson_compat (yojson_of_t t) with
  | Ok t' -> equal t t'
  | Error _ -> false

let%test "yojson round-trip: Absent" = roundtrip_ok Absent
let%test "yojson round-trip: Present" = roundtrip_ok (Present (mk_pr 5))
let%test "yojson round-trip: Missing" = roundtrip_ok (Missing (mk_pr 5))

let%test "legacy null decodes to Absent" =
  match t_of_yojson_compat `Null with
  | Ok Absent -> true
  | Ok (Present _ | Missing _) | Error _ -> false

let%test "legacy bare int decodes to Present" =
  match t_of_yojson_compat (`Int 42) with
  | Ok (Present n) -> Pr_number.equal n (mk_pr 42)
  | Ok (Absent | Missing _) | Error _ -> false

let%test "unknown kind is an Error" =
  match t_of_yojson_compat (`Assoc [ ("kind", `String "bogus") ]) with
  | Error _ -> true
  | Ok (Absent | Present _ | Missing _) -> false

let%test "present without pr_number is an Error" =
  match t_of_yojson_compat (`Assoc [ ("kind", `String "present") ]) with
  | Error _ -> true
  | Ok (Absent | Present _ | Missing _) -> false
