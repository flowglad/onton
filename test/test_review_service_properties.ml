open Base
open Onton_core

(** Property tests for the pure review-service parsers.

    AGENTS.md mandates that pure modules be total — every parser must return a
    [Result] over arbitrary input rather than raising. The properties below fuzz
    the four wire-shaped entry points ([parse_findings_response_string],
    [parse_resolve_response_string], [parse_error_message],
    [parse_wontfix_artifact]) plus the small enum coders. *)

(* ─────────────────────────────────────────────────────────────────────────
   Generators
   ───────────────────────────────────────────────────────────────────────── *)

let gen_yojson : Yojson.Safe.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  sized
  @@ fix (fun self n ->
      if n <= 0 then
        oneof
          [
            pure `Null;
            map (fun b -> `Bool b) bool;
            map (fun i -> `Int i) nat_small;
            map (fun i -> `Float (Float.of_int i /. 100.)) (int_range 0 10_000);
            map (fun s -> `String s) string;
          ]
      else
        oneof
          [
            self 0;
            map (fun xs -> `List xs) (list_size (int_range 0 5) (self (n / 2)));
            map
              (fun pairs -> `Assoc pairs)
              (list_size (int_range 0 5)
                 (pair (string_size (int_range 1 8)) (self (n / 2))));
          ])

let gen_arbitrary_string =
  let open QCheck2.Gen in
  oneof
    [
      string;
      pure "";
      pure "{}";
      pure "[]";
      pure "not json";
      pure "\x00\x01\xff";
      map (fun y -> Yojson.Safe.to_string y) gen_yojson;
      string_size (int_range 0 1024);
    ]

(* A "shaped-but-not-validated" findings response — the structure is right
   on the outside but the contents may be garbage. Used to exercise the
   "drop malformed entries, keep the rest" path in
   [parse_findings_response]. *)
let gen_finding_object : Yojson.Safe.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let opt_field name g =
    let* present = bool in
    if present then
      let* v = g in
      return [ (name, v) ]
    else return []
  in
  let* id = opt_field "id" (map (fun s -> `String s) string) in
  let* gh =
    opt_field "githubCommentId"
      (oneof [ map (fun n -> `Int n) nat_small; pure `Null ])
  in
  let* sha = opt_field "postingSha" (map (fun s -> `String s) string) in
  let* path = opt_field "path" (map (fun s -> `String s) string) in
  let* startLine = opt_field "startLine" (map (fun n -> `Int n) nat_small) in
  let* endLine = opt_field "endLine" (map (fun n -> `Int n) nat_small) in
  let* sev =
    opt_field "severity"
      (map
         (fun s -> `String s)
         (oneof
            [
              pure "must-fix";
              pure "should-fix";
              pure "note";
              pure "garbage";
              string;
            ]))
  in
  let* body = opt_field "body" (map (fun s -> `String s) string) in
  let* createdAt = opt_field "createdAt" (map (fun s -> `String s) string) in
  let* outcome =
    opt_field "outcome"
      (oneof
         [
           pure (`Assoc [ ("kind", `String "outstanding") ]);
           pure (`Assoc [ ("kind", `String "discussed") ]);
           pure (`Assoc [ ("kind", `String "addressed") ]);
           pure (`Assoc [ ("kind", `String "ignored") ]);
           pure (`Assoc [ ("kind", `String "resolved") ]);
           pure (`Assoc [ ("kind", `String "garbage") ]);
           pure `Null;
           gen_yojson;
         ])
  in
  return
    (`Assoc
       (id @ gh @ sha @ path @ startLine @ endLine @ sev @ body @ createdAt
      @ outcome))

let gen_findings_response : string QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* repo_id = oneof [ string; pure ""; pure "octo/widgets" ] in
  let* pull_number = nat_small in
  let* findings = list_size (int_range 0 6) gen_finding_object in
  return
    (Yojson.Safe.to_string
       (`Assoc
          [
            ("repoId", `String repo_id);
            ("pullNumber", `Int pull_number);
            ("count", `Int (List.length findings));
            ("findings", `List findings);
          ]))

let gen_input =
  let open QCheck2.Gen in
  oneof [ gen_arbitrary_string; gen_findings_response ]

(* ─────────────────────────────────────────────────────────────────────────
   Properties
   ───────────────────────────────────────────────────────────────────────── *)

let totality ~name f =
  QCheck2.Test.make ~name ~count:1000 gen_input (fun s ->
      try
        ignore (f s);
        true
      with _ -> false)

let prop_findings_total =
  totality ~name:"parse_findings_response_string total" (fun s ->
      Review_service.parse_findings_response_string s)

let prop_resolve_total =
  totality ~name:"parse_resolve_response_string total" (fun s ->
      Review_service.parse_resolve_response_string s)

let prop_error_total =
  totality ~name:"parse_error_message total" (fun s ->
      Review_service.parse_error_message s)

let prop_wontfix_total =
  totality ~name:"parse_wontfix_artifact total" (fun s ->
      Review_service.parse_wontfix_artifact s)

(* All findings the parser keeps must round-trip the severity/outcome enums. *)
let prop_findings_kept_have_known_enums =
  QCheck2.Test.make ~name:"parse_findings keeps only known severities/outcomes"
    ~count:500 gen_findings_response (fun s ->
      match Review_service.parse_findings_response_string s with
      | Error _ -> true
      | Ok r ->
          List.for_all r.findings ~f:(fun (f : Review_service.finding) ->
              let sev_ok =
                Option.is_some
                  (Review_service.severity_of_string
                     (Review_service.severity_to_string f.severity))
              in
              let kind_ok =
                Option.is_some
                  (Review_service.outcome_kind_of_string
                     (Review_service.outcome_kind_to_string f.outcome.kind))
              in
              sev_ok && kind_ok))

(* Severity round-trip is total over the three known strings and rejects
   everything else. *)
let prop_severity_round_trip =
  QCheck2.Test.make ~name:"severity_of_string ∘ to_string = id" ~count:500
    QCheck2.Gen.string (fun raw ->
      match Review_service.severity_of_string raw with
      | None -> true
      | Some s -> String.equal (Review_service.severity_to_string s) raw
      (* If of_string accepted [raw], it must round-trip to the canonical
             form — but the canonical form is the input itself for the three
             accepted values. *))

let prop_outcome_kind_round_trip =
  QCheck2.Test.make ~name:"outcome_kind_of_string ∘ to_string = id" ~count:500
    QCheck2.Gen.string (fun raw ->
      match Review_service.outcome_kind_of_string raw with
      | None -> true
      | Some k -> String.equal (Review_service.outcome_kind_to_string k) raw)

let prop_resolve_kind_round_trip =
  QCheck2.Test.make ~name:"resolve_kind_of_string ∘ to_string = id" ~count:500
    QCheck2.Gen.string (fun raw ->
      match Review_service.resolve_kind_of_string raw with
      | None -> true
      | Some k -> String.equal (Review_service.resolve_kind_to_string k) raw)

(* The resolve request encoder must be total and never raise. *)
let prop_resolve_request_to_yojson_total =
  let open QCheck2.Gen in
  let gen =
    let* kind =
      oneof
        [
          pure Review_service.Resolve_addressed;
          pure Review_service.Resolve_wontfix;
        ]
    in
    let* actor = oneof [ pure None; map (fun s -> Some s) string ] in
    let* reason = oneof [ pure None; map (fun s -> Some s) string ] in
    return
      ({ Review_service.kind; actor; reason } : Review_service.resolve_request)
  in
  QCheck2.Test.make ~name:"resolve_request_to_yojson total" ~count:500 gen
    (fun req ->
      try
        ignore (Review_service.resolve_request_to_yojson req);
        true
      with _ -> false)

(* The encoder always emits an Assoc with a "kind" field and at most one each
   of "actor"/"reason". *)
let prop_resolve_request_shape =
  let open QCheck2.Gen in
  let gen =
    let* kind =
      oneof
        [
          pure Review_service.Resolve_addressed;
          pure Review_service.Resolve_wontfix;
        ]
    in
    let* actor = oneof [ pure None; map (fun s -> Some s) string ] in
    let* reason = oneof [ pure None; map (fun s -> Some s) string ] in
    return
      ({ Review_service.kind; actor; reason } : Review_service.resolve_request)
  in
  QCheck2.Test.make ~name:"resolve_request_to_yojson shape" ~count:500 gen
    (fun req ->
      match Review_service.resolve_request_to_yojson req with
      | `Assoc fields ->
          let has_kind =
            List.exists fields ~f:(fun (k, _) -> String.equal k "kind")
          in
          let actor_present =
            List.exists fields ~f:(fun (k, _) -> String.equal k "actor")
          in
          let reason_present =
            List.exists fields ~f:(fun (k, _) -> String.equal k "reason")
          in
          has_kind
          && Bool.equal actor_present (Option.is_some req.actor)
          && Bool.equal reason_present (Option.is_some req.reason)
      | _ -> false)

let () =
  let suite =
    [
      prop_findings_total;
      prop_resolve_total;
      prop_error_total;
      prop_wontfix_total;
      prop_findings_kept_have_known_enums;
      prop_severity_round_trip;
      prop_outcome_kind_round_trip;
      prop_resolve_kind_round_trip;
      prop_resolve_request_to_yojson_total;
      prop_resolve_request_shape;
    ]
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true suite in
  if exit_code <> 0 then Stdlib.exit exit_code
