open Base

type severity = Must_fix | Should_fix | Note
[@@deriving show, eq, sexp_of, compare]

type outcome_kind = Outstanding | Discussed | Addressed | Ignored | Wontfix
[@@deriving show, eq, sexp_of, compare]

type last_reply = { author : string; at : string; body : string }
[@@deriving show, eq, sexp_of, compare]

type outcome = {
  kind : outcome_kind;
  detected_at : string option;
  actor : string option;
  reason : string option;
  last_reply : last_reply option;
}
[@@deriving show, eq, sexp_of, compare]

type finding = {
  id : string;
  github_comment_id : int option;
  posting_sha : string;
  path : string;
  start_line : int;
  end_line : int;
  severity : severity;
  body : string;
  created_at : string;
  outcome : outcome;
}
[@@deriving show, eq, sexp_of, compare]

type findings_response = {
  repo_id : string;
  pull_number : int;
  count : int;
  findings : finding list;
}
[@@deriving show, eq, sexp_of, compare]

type resolve_kind = Resolve_addressed | Resolve_wontfix
[@@deriving show, eq, sexp_of, compare]

let severity_of_string = function
  | "must-fix" -> Some Must_fix
  | "should-fix" -> Some Should_fix
  | "note" -> Some Note
  | _ -> None

let severity_to_string = function
  | Must_fix -> "must-fix"
  | Should_fix -> "should-fix"
  | Note -> "note"

let outcome_kind_of_string = function
  | "outstanding" -> Some Outstanding
  | "discussed" -> Some Discussed
  | "addressed" -> Some Addressed
  | "ignored" -> Some Ignored
  | "wontfix" -> Some Wontfix
  | _ -> None

let outcome_kind_to_string = function
  | Outstanding -> "outstanding"
  | Discussed -> "discussed"
  | Addressed -> "addressed"
  | Ignored -> "ignored"
  | Wontfix -> "wontfix"

let resolve_kind_of_string = function
  | "addressed" -> Some Resolve_addressed
  | "wontfix" -> Some Resolve_wontfix
  | _ -> None

let resolve_kind_to_string = function
  | Resolve_addressed -> "addressed"
  | Resolve_wontfix -> "wontfix"

(** Total: returns [None] for [`Null], non-strings, or empty strings. *)
let string_opt = function
  | `Null -> None
  | `String s when String.is_empty s -> None
  | `String s -> Some s
  | _ -> None

let int_opt = function `Null -> None | `Int n -> Some n | _ -> None

let require_string field json =
  let open Yojson.Safe.Util in
  match json |> member field with
  | `String s -> Ok s
  | `Null -> Error (Printf.sprintf "missing required field %S" field)
  | _ -> Error (Printf.sprintf "field %S must be a string" field)

let require_int field json =
  let open Yojson.Safe.Util in
  match json |> member field with
  | `Int n -> Ok n
  | `Null -> Error (Printf.sprintf "missing required field %S" field)
  | _ -> Error (Printf.sprintf "field %S must be an integer" field)

let parse_last_reply json : last_reply option =
  let open Yojson.Safe.Util in
  match json with
  | `Null -> None
  | `Assoc _ ->
      let author = json |> member "author" |> string_opt in
      let at = json |> member "at" |> string_opt in
      let body = json |> member "body" |> string_opt in
      (* All three required for a meaningful reply; any missing means we skip. *)
      Option.map3 author at body ~f:(fun author at body -> { author; at; body })
  | _ -> None

let parse_outcome json : (outcome, string) Result.t =
  let open Yojson.Safe.Util in
  match json with
  | `Null -> Error "outcome is null"
  | `Assoc _ -> (
      let kind_str =
        json |> member "kind" |> string_opt |> Option.value ~default:""
      in
      match outcome_kind_of_string kind_str with
      | None ->
          Error (Printf.sprintf "outcome.kind is not a known kind: %S" kind_str)
      | Some kind ->
          let detected_at = json |> member "detectedAt" |> string_opt in
          let actor = json |> member "actor" |> string_opt in
          let reason = json |> member "reason" |> string_opt in
          let last_reply = parse_last_reply (json |> member "lastReply") in
          Ok { kind; detected_at; actor; reason; last_reply })
  | _ -> Error "outcome must be an object"

let parse_finding json : (finding, string) Result.t =
  let open Yojson.Safe.Util in
  let ( let* ) = Result.( >>= ) in
  match json with
  | `Assoc _ ->
      let* id = require_string "id" json in
      let github_comment_id = json |> member "githubCommentId" |> int_opt in
      let* posting_sha = require_string "postingSha" json in
      let* path = require_string "path" json in
      let* start_line = require_int "startLine" json in
      let* end_line = require_int "endLine" json in
      let severity_str =
        json |> member "severity" |> string_opt |> Option.value ~default:""
      in
      let* severity =
        match severity_of_string severity_str with
        | Some s -> Ok s
        | None -> Error (Printf.sprintf "unknown severity: %S" severity_str)
      in
      let* body = require_string "body" json in
      let* created_at = require_string "createdAt" json in
      let* outcome = parse_outcome (json |> member "outcome") in
      Ok
        {
          id;
          github_comment_id;
          posting_sha;
          path;
          start_line;
          end_line;
          severity;
          body;
          created_at;
          outcome;
        }
  | _ -> Error "finding must be an object"

let parse_findings_response json : (findings_response, string) Result.t =
  let open Yojson.Safe.Util in
  let ( let* ) = Result.( >>= ) in
  match json with
  | `Assoc _ ->
      let* repo_id = require_string "repoId" json in
      let* pull_number = require_int "pullNumber" json in
      let count = match json |> member "count" with `Int n -> n | _ -> 0 in
      let raw_findings =
        match json |> member "findings" with `List xs -> xs | _ -> []
      in
      (* Drop entries that fail to parse rather than failing the whole response.
         A single schema-drift entry shouldn't blank out a PR's findings — the
         caller can compare [count] against [List.length findings] to detect
         partial-decode loss. *)
      let findings =
        List.filter_map raw_findings ~f:(fun f ->
            match parse_finding f with Ok f -> Some f | Error _ -> None)
      in
      Ok { repo_id; pull_number; count; findings }
  | _ -> Error "findings response must be an object"

let parse_findings_response_string body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error msg -> Error (Printf.sprintf "JSON: %s" msg)
  | json -> parse_findings_response json

type resolve_response = { id : string; outcome : outcome }
[@@deriving show, eq, sexp_of, compare]

let parse_resolve_response json : (resolve_response, string) Result.t =
  let open Yojson.Safe.Util in
  let ( let* ) = Result.( >>= ) in
  match json with
  | `Assoc _ ->
      let* id = require_string "id" json in
      let* outcome = parse_outcome (json |> member "outcome") in
      Ok { id; outcome }
  | _ -> Error "resolve response must be an object"

let parse_resolve_response_string body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error msg -> Error (Printf.sprintf "JSON: %s" msg)
  | json -> parse_resolve_response json

type resolve_request = {
  kind : resolve_kind;
  actor : string option;
  reason : string option;
}
[@@deriving show, eq, sexp_of, compare]

let resolve_request_to_yojson (req : resolve_request) : Yojson.Safe.t =
  let fields = [ ("kind", `String (resolve_kind_to_string req.kind)) ] in
  let fields =
    match req.actor with
    | Some s -> fields @ [ ("actor", `String s) ]
    | None -> fields
  in
  let fields =
    match req.reason with
    | Some s -> fields @ [ ("reason", `String s) ]
    | None -> fields
  in
  `Assoc fields

let parse_error_message body =
  match Yojson.Safe.from_string body with
  | exception _ -> None
  | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "error" with
      | Some (`String s) -> Some s
      | _ -> None)
  | _ -> None

type wontfix_entry = { id : string; reason : string }
[@@deriving show, eq, sexp_of, compare]

let parse_wontfix_artifact body : (wontfix_entry list, string) Result.t =
  let trimmed = String.strip body in
  if String.is_empty trimmed then Ok []
  else
    match Yojson.Safe.from_string trimmed with
    | exception Yojson.Json_error msg ->
        Error (Printf.sprintf "wontfix artifact: %s" msg)
    | `List entries ->
        let parsed =
          List.filter_map entries ~f:(fun entry ->
              let open Yojson.Safe.Util in
              match entry with
              | `Assoc _ ->
                  let id = entry |> member "id" |> string_opt in
                  let reason = entry |> member "reason" |> string_opt in
                  Option.map2 id reason ~f:(fun id reason -> { id; reason })
              | _ -> None)
        in
        Ok parsed
    | _ -> Error "wontfix artifact must be a JSON array"

(* {2 Inline tests} *)

let%test "severity round trip" =
  List.for_all [ Must_fix; Should_fix; Note ] ~f:(fun s ->
      match severity_of_string (severity_to_string s) with
      | Some s' -> equal_severity s s'
      | None -> false)

let%test "severity unknown -> None" =
  Option.is_none (severity_of_string "critical")

let%test "outcome_kind round trip" =
  List.for_all [ Outstanding; Discussed; Addressed; Ignored; Wontfix ]
    ~f:(fun k ->
      match outcome_kind_of_string (outcome_kind_to_string k) with
      | Some k' -> equal_outcome_kind k k'
      | None -> false)

let%test "resolve_kind round trip" =
  List.for_all [ Resolve_addressed; Resolve_wontfix ] ~f:(fun k ->
      match resolve_kind_of_string (resolve_kind_to_string k) with
      | Some k' -> equal_resolve_kind k k'
      | None -> false)

let%test "parse_finding: full object" =
  let raw =
    {|{"id":"abc","githubCommentId":42,"postingSha":"deadbeef","path":"src/x.ml","startLine":3,"endLine":5,"severity":"must-fix","body":"oops","createdAt":"2026-05-06T00:00:00Z","outcome":{"kind":"outstanding"}}|}
  in
  match parse_finding (Yojson.Safe.from_string raw) with
  | Ok f ->
      String.equal f.id "abc"
      && Option.equal Int.equal f.github_comment_id (Some 42)
      && String.equal f.posting_sha "deadbeef"
      && String.equal f.path "src/x.ml"
      && f.start_line = 3 && f.end_line = 5
      && equal_severity f.severity Must_fix
      && equal_outcome_kind f.outcome.kind Outstanding
  | Error _ -> false

let%test "parse_finding: missing required field" =
  let raw = {|{"id":"abc"}|} in
  match parse_finding (Yojson.Safe.from_string raw) with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_finding: githubCommentId null -> None" =
  let raw =
    {|{"id":"abc","githubCommentId":null,"postingSha":"x","path":"a","startLine":1,"endLine":1,"severity":"note","body":"b","createdAt":"t","outcome":{"kind":"outstanding"}}|}
  in
  match parse_finding (Yojson.Safe.from_string raw) with
  | Ok f -> Option.is_none f.github_comment_id
  | Error _ -> false

let%test "parse_findings_response: drops malformed entries, keeps the rest" =
  let raw =
    {|{"repoId":"o/r","pullNumber":7,"count":2,"findings":[
       {"id":"a","postingSha":"s","path":"f","startLine":1,"endLine":1,"severity":"note","body":"b","createdAt":"t","outcome":{"kind":"outstanding"}},
       {"id":"bad"}
     ]}|}
  in
  match parse_findings_response (Yojson.Safe.from_string raw) with
  | Ok r ->
      String.equal r.repo_id "o/r"
      && r.pull_number = 7
      && List.length r.findings = 1
  | Error _ -> false

let%test "parse_findings_response_string: invalid JSON -> Error" =
  match parse_findings_response_string "not json" with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_findings_response_string: empty findings -> Ok empty" =
  let raw = {|{"repoId":"o/r","pullNumber":1,"count":0,"findings":[]}|} in
  match parse_findings_response_string raw with
  | Ok r -> List.is_empty r.findings && r.count = 0
  | Error _ -> false

let%test "parse_resolve_response: ok with addressed outcome" =
  let raw =
    {|{"id":"abc","outcome":{"kind":"addressed","detectedAt":"t","actor":"onton:agent-7"}}|}
  in
  match parse_resolve_response (Yojson.Safe.from_string raw) with
  | Ok r ->
      String.equal r.id "abc"
      && equal_outcome_kind r.outcome.kind Addressed
      && Option.equal String.equal r.outcome.actor (Some "onton:agent-7")
  | Error _ -> false

let%test "resolve_request_to_yojson: omits None fields" =
  let req = { kind = Resolve_addressed; actor = None; reason = None } in
  let json = resolve_request_to_yojson req in
  String.equal (Yojson.Safe.to_string json) {|{"kind":"addressed"}|}

let%test "resolve_request_to_yojson: includes Some fields" =
  let req =
    { kind = Resolve_wontfix; actor = Some "x"; reason = Some "rationale" }
  in
  let json = resolve_request_to_yojson req in
  String.equal
    (Yojson.Safe.to_string json)
    {|{"kind":"wontfix","actor":"x","reason":"rationale"}|}

let%test "parse_error_message: extracts error" =
  match parse_error_message {|{"error":"unauthorized"}|} with
  | Some "unauthorized" -> true
  | _ -> false

let%test "parse_error_message: returns None on non-JSON" =
  Option.is_none (parse_error_message "<html></html>")

let%test "parse_error_message: returns None on missing error field" =
  Option.is_none (parse_error_message {|{"ok":true}|})

let%test "parse_outcome: lastReply roundtrip" =
  let raw =
    {|{"kind":"discussed","detectedAt":"t","lastReply":{"author":"u","at":"t2","body":"hi"}}|}
  in
  match parse_outcome (Yojson.Safe.from_string raw) with
  | Ok o -> (
      match o.last_reply with
      | Some lr ->
          String.equal lr.author "u" && String.equal lr.body "hi"
          && String.equal lr.at "t2"
      | None -> false)
  | Error _ -> false

let%test "parse_outcome: partial lastReply -> None (skipped)" =
  let raw = {|{"kind":"discussed","lastReply":{"author":"u"}}|} in
  match parse_outcome (Yojson.Safe.from_string raw) with
  | Ok o -> Option.is_none o.last_reply
  | Error _ -> false

let%test "parse_wontfix_artifact: empty string -> Ok []" =
  match parse_wontfix_artifact "" with
  | Ok [] -> true
  | Ok _ -> false
  | Error _ -> false

let%test "parse_wontfix_artifact: whitespace-only -> Ok []" =
  match parse_wontfix_artifact "   \n\t  " with
  | Ok [] -> true
  | Ok _ -> false
  | Error _ -> false

let%test "parse_wontfix_artifact: empty array -> Ok []" =
  match parse_wontfix_artifact "[]" with
  | Ok [] -> true
  | Ok _ -> false
  | Error _ -> false

let%test "parse_wontfix_artifact: well-formed entries" =
  let raw =
    {|[{"id":"a","reason":"out of scope"},{"id":"b","reason":"false positive"}]|}
  in
  match parse_wontfix_artifact raw with
  | Ok [ a; b ] ->
      String.equal a.id "a"
      && String.equal a.reason "out of scope"
      && String.equal b.id "b"
      && String.equal b.reason "false positive"
  | Ok _ -> false
  | Error _ -> false

let%test "parse_wontfix_artifact: malformed entries skipped, valid ones kept" =
  let raw =
    {|[{"id":"a","reason":"r"},{"id":"only-id"},{"reason":"only-r"}]|}
  in
  match parse_wontfix_artifact raw with
  | Ok [ e ] -> String.equal e.id "a" && String.equal e.reason "r"
  | Ok _ -> false
  | Error _ -> false

let%test "parse_wontfix_artifact: invalid JSON -> Error" =
  match parse_wontfix_artifact "not json" with Error _ -> true | Ok _ -> false

let%test "parse_wontfix_artifact: object instead of array -> Error" =
  match parse_wontfix_artifact {|{"id":"a","reason":"r"}|} with
  | Error _ -> true
  | Ok _ -> false
