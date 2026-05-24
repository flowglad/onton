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
    try Ok (Pr_number.t_of_yojson v) with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Stdlib.Printexc.to_string exn)
    | Yojson.Safe.Util.Type_error (msg, _) ->
        Error (Printf.sprintf "malformed pr_number: %s" msg)
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
