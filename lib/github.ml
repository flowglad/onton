open Base

module Pr_state = struct
  type merge_state = Mergeable | Conflicting | Unknown [@@deriving show, eq]
  type check_status = Passing | Failing | Pending [@@deriving show, eq]

  type t = {
    merged : bool;
    merge_state : merge_state;
    check_status : check_status;
    comments : Types.Comment.t list;
    unresolved_comment_count : int;
  }
  [@@deriving show, eq]
end

type error =
  | Http_error of int * string
  | Json_parse_error of string
  | Graphql_error of string list
[@@deriving show, eq]

type t = { token : string; owner : string; repo : string }

let create ~token ~owner ~repo = { token; owner; repo }

let graphql_query =
  {|query($owner: String!, $repo: String!, $number: Int!) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $number) {
      merged
      mergeable
      commits(last: 1) {
        nodes {
          commit {
            statusCheckRollup {
              state
            }
          }
        }
      }
      reviewThreads(first: 100) {
        nodes {
          isResolved
          comments(first: 1) {
            nodes {
              body
              path
              line
            }
          }
        }
      }
    }
  }
}|}

let build_request_body t (pr : Types.Pr_number.t) =
  let variables =
    `Assoc
      [
        ("owner", `String t.owner);
        ("repo", `String t.repo);
        ("number", `Int (Types.Pr_number.to_int pr));
      ]
  in
  `Assoc [ ("query", `String graphql_query); ("variables", variables) ]
  |> Yojson.Safe.to_string

let parse_merge_state = function
  | "MERGEABLE" -> Pr_state.Mergeable
  | "CONFLICTING" -> Pr_state.Conflicting
  | _ -> Pr_state.Unknown

let parse_check_status = function
  | "SUCCESS" -> Pr_state.Passing
  | "FAILURE" | "ERROR" -> Pr_state.Failing
  | _ -> Pr_state.Pending

let parse_comment_node node =
  let open Yojson.Safe.Util in
  let body = node |> member "body" |> to_string in
  let path = node |> member "path" |> to_string_option in
  let line = node |> member "line" |> to_int_option in
  Types.Comment.{ body; path; line }

let parse_response body =
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_string body in
    let errors = json |> member "errors" in
    match errors with
    | `Null -> (
        let pr =
          json |> member "data" |> member "repository" |> member "pullRequest"
        in
        match pr with
        | `Null -> Error (Json_parse_error "pullRequest not found")
        | pr ->
            let merged = pr |> member "merged" |> to_bool in
            let merge_state =
              pr |> member "mergeable" |> to_string |> parse_merge_state
            in
            let check_status =
              let commits = pr |> member "commits" |> member "nodes" in
              match commits |> to_list with
              | [] -> Pr_state.Pending
              | node :: _ -> (
                  let rollup =
                    node |> member "commit" |> member "statusCheckRollup"
                  in
                  match rollup with
                  | `Null -> Pr_state.Pending
                  | rollup ->
                      rollup |> member "state" |> to_string
                      |> parse_check_status)
            in
            let review_threads =
              pr |> member "reviewThreads" |> member "nodes" |> to_list
            in
            let unresolved_comment_count =
              List.count review_threads ~f:(fun thread ->
                  not (thread |> member "isResolved" |> to_bool))
            in
            let comments =
              List.concat_map review_threads ~f:(fun thread ->
                  thread |> member "comments" |> member "nodes" |> to_list
                  |> List.map ~f:parse_comment_node)
            in
            Ok
              Pr_state.
                {
                  merged;
                  merge_state;
                  check_status;
                  comments;
                  unresolved_comment_count;
                })
    | errors ->
        let msgs =
          errors |> to_list
          |> List.map ~f:(fun e -> e |> member "message" |> to_string)
        in
        Error (Graphql_error msgs)
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (Json_parse_error msg)
  | Yojson.Json_error msg -> Error (Json_parse_error msg)

let pr_state t pr =
  let body = build_request_body t pr in
  ignore (t.token, body, parse_response);
  (* TODO: Wire up cohttp-eio HTTP call in orchestration patch *)
  Error (Http_error (0, "not yet wired to HTTP client"))

(* WorldCtx predicate accessors *)

let merged (st : Pr_state.t) = st.Pr_state.merged

let mergeable (st : Pr_state.t) =
  let open Pr_state in
  (not st.merged) && equal_merge_state st.merge_state Mergeable

let checks_passing (st : Pr_state.t) =
  let open Pr_state in
  equal_check_status st.check_status Passing

let no_unresolved_comments (st : Pr_state.t) =
  st.Pr_state.unresolved_comment_count = 0

let has_conflict (st : Pr_state.t) =
  let open Pr_state in
  equal_merge_state st.merge_state Conflicting

let ci_failed (st : Pr_state.t) =
  let open Pr_state in
  equal_check_status st.check_status Failing
