open Base

module Pr_state = struct
  type merge_state = Mergeable | Conflicting | Unknown [@@deriving show, eq]
  type check_status = Passing | Failing | Pending [@@deriving show, eq]
  type pr_status = Open | Merged | Closed [@@deriving show, eq]

  type t = {
    status : pr_status;
    merge_state : merge_state;
    merge_ready : bool;
    check_status : check_status;
    ci_checks : Types.Ci_check.t list;
    ci_checks_truncated : bool;
    comments : Types.Comment.t list;
    unresolved_comment_count : int;
    head_branch : Types.Branch.t option;
    base_branch : Types.Branch.t option;
  }
  [@@deriving show, eq]
end

type error =
  | Http_error of int * string
  | Json_parse_error of string
  | Graphql_error of string list
  | Transport_error of string
[@@deriving show, eq]

type t = { token : string; owner : string; repo : string }

let create ~token ~owner ~repo = { token; owner; repo }

let graphql_query =
  {|query($owner: String!, $repo: String!, $number: Int!) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $number) {
      state
      mergeable
      mergeStateStatus
      headRefName
      baseRefName
      commits(last: 1) {
        nodes {
          commit {
            statusCheckRollup {
              state
              contexts(first: 100) {
                pageInfo { hasNextPage }
                nodes {
                  ... on CheckRun {
                    __typename
                    name
                    conclusion
                    detailsUrl
                    text
                    startedAt
                  }
                  ... on StatusContext {
                    __typename
                    context
                    state
                    targetUrl
                    description
                    createdAt
                  }
                }
              }
            }
          }
        }
      }
      reviewThreads(first: 100) {
        nodes {
          id
          isResolved
          comments(first: 1) {
            nodes {
              databaseId
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

let parse_check_context_node node =
  let open Yojson.Safe.Util in
  let typename = node |> member "__typename" |> to_string_option in
  match typename with
  | Some "CheckRun" -> (
      match node |> member "name" |> to_string_option with
      | None -> None
      | Some name ->
          let conclusion =
            match node |> member "conclusion" |> to_string_option with
            | Some c -> String.lowercase c
            | None -> "pending"
          in
          let details_url = node |> member "detailsUrl" |> to_string_option in
          let description = node |> member "text" |> to_string_option in
          let started_at = node |> member "startedAt" |> to_string_option in
          Some
            Types.Ci_check.
              { name; conclusion; details_url; description; started_at })
  | Some "StatusContext" -> (
      match node |> member "context" |> to_string_option with
      | None -> None
      | Some name ->
          let conclusion =
            match node |> member "state" |> to_string_option with
            | Some s -> String.lowercase s
            | None -> "pending"
          in
          let details_url = node |> member "targetUrl" |> to_string_option in
          let description = node |> member "description" |> to_string_option in
          let started_at = node |> member "createdAt" |> to_string_option in
          Some
            Types.Ci_check.
              { name; conclusion; details_url; description; started_at })
  | _ -> None

let parse_comment_node ~thread_id node =
  let open Yojson.Safe.Util in
  let id =
    match node |> member "databaseId" |> to_int_option with
    | Some raw_id -> Types.Comment_id.of_int raw_id
    | None -> Types.Comment_id.next_synthetic ()
  in
  let body = node |> member "body" |> to_string in
  let path = node |> member "path" |> to_string_option in
  let line = node |> member "line" |> to_int_option in
  Types.Comment.{ id; thread_id; body; path; line }

let parse_response_json json =
  let open Yojson.Safe.Util in
  try
    let errors = json |> member "errors" in
    match errors with
    | `Null -> (
        let pr =
          json |> member "data" |> member "repository" |> member "pullRequest"
        in
        match pr with
        | `Null -> Error (Json_parse_error "pullRequest not found")
        | pr ->
            let status =
              match pr |> member "state" |> to_string with
              | "MERGED" -> Pr_state.Merged
              | "CLOSED" -> Pr_state.Closed
              | _ -> Pr_state.Open
            in
            let merge_state =
              pr |> member "mergeable" |> to_string |> parse_merge_state
            in
            let merge_ready =
              match pr |> member "mergeStateStatus" |> to_string_option with
              | Some "CLEAN" -> true
              | _ -> false
            in
            let check_status, ci_checks, ci_checks_truncated =
              let commits = pr |> member "commits" |> member "nodes" in
              match commits |> to_list with
              | [] -> (Pr_state.Pending, [], false)
              | node :: _ -> (
                  let rollup =
                    node |> member "commit" |> member "statusCheckRollup"
                  in
                  match rollup with
                  | `Null -> (Pr_state.Pending, [], false)
                  | rollup ->
                      let status =
                        rollup |> member "state" |> to_string
                        |> parse_check_status
                      in
                      let contexts = rollup |> member "contexts" in
                      let truncated =
                        contexts |> member "pageInfo" |> member "hasNextPage"
                        |> to_bool_option
                        |> Option.value ~default:false
                      in
                      let checks =
                        contexts |> member "nodes" |> to_list
                        |> List.filter_map ~f:parse_check_context_node
                      in
                      (status, checks, truncated))
            in
            let review_threads =
              pr |> member "reviewThreads" |> member "nodes" |> to_list
            in
            let comments =
              List.concat_map review_threads ~f:(fun thread ->
                  if thread |> member "isResolved" |> to_bool then []
                  else
                    let thread_id = thread |> member "id" |> to_string_option in
                    thread |> member "comments" |> member "nodes" |> to_list
                    |> List.map ~f:(parse_comment_node ~thread_id))
            in
            let unresolved_comment_count =
              List.count review_threads ~f:(fun thread ->
                  not (thread |> member "isResolved" |> to_bool))
            in
            let head_branch =
              pr |> member "headRefName" |> to_string_option
              |> Option.map ~f:Types.Branch.of_string
            in
            let base_branch =
              pr |> member "baseRefName" |> to_string_option
              |> Option.map ~f:Types.Branch.of_string
            in
            Ok
              Pr_state.
                {
                  status;
                  merge_state;
                  merge_ready;
                  check_status;
                  ci_checks;
                  ci_checks_truncated;
                  comments;
                  unresolved_comment_count;
                  head_branch;
                  base_branch;
                })
    | errors ->
        let msgs =
          errors |> to_list
          |> List.map ~f:(fun e -> e |> member "message" |> to_string)
        in
        Error (Graphql_error msgs)
  with Yojson.Safe.Util.Type_error (msg, _) -> Error (Json_parse_error msg)

let parse_response body =
  try parse_response_json (Yojson.Safe.from_string body)
  with Yojson.Json_error msg -> Error (Json_parse_error msg)

let https_config () =
  match Ca_certs.authenticator () with
  | Error (`Msg msg) -> Error ("TLS CA setup failed: " ^ msg)
  | Ok authenticator -> (
      match Tls.Config.client ~authenticator () with
      | Ok cfg -> Ok cfg
      | Error (`Msg msg) -> Error ("TLS config failed: " ^ msg))

let https_fun tls_config uri flow =
  let host =
    Uri.host uri
    |> Option.map ~f:(fun h -> Domain_name.(of_string_exn h |> host_exn))
  in
  (Tls_eio.client_of_flow tls_config ?host flow :> _ Eio.Flow.two_way)

let max_response_size = 1_000_000

let pr_state ~net t pr =
  try
    Mirage_crypto_rng_unix.use_default ();
    Result.bind
      (Result.map_error (https_config ()) ~f:(fun msg -> Transport_error msg))
      ~f:(fun tls_config ->
        let client =
          Cohttp_eio.Client.make ~https:(Some (https_fun tls_config)) net
        in
        let request_body = build_request_body t pr in
        let uri = Uri.of_string "https://api.github.com/graphql" in
        let headers =
          Http.Header.of_list
            [
              ("Authorization", "Bearer " ^ t.token);
              ("Content-Type", "application/json");
              ("User-Agent", "onton/0.1.0");
            ]
        in
        let body = Cohttp_eio.Body.of_string request_body in
        Eio.Switch.run @@ fun sw ->
        let resp, resp_body =
          Cohttp_eio.Client.post client ~sw ~headers ~body uri
        in
        let status = Http.Response.status resp |> Http.Status.to_int in
        let resp_str =
          Eio.Buf_read.(
            of_flow ~max_size:max_response_size resp_body |> take_all)
        in
        if status >= 200 && status < 300 then parse_response resp_str
        else Error (Http_error (status, resp_str)))
  with exn -> Error (Transport_error (Exn.to_string exn))

(* WorldCtx predicate accessors *)

let merged (st : Pr_state.t) =
  Pr_state.equal_pr_status st.Pr_state.status Pr_state.Merged

let closed (st : Pr_state.t) =
  Pr_state.equal_pr_status st.Pr_state.status Pr_state.Closed

let mergeable (st : Pr_state.t) =
  let open Pr_state in
  equal_pr_status st.status Open && equal_merge_state st.merge_state Mergeable

let merge_ready (st : Pr_state.t) =
  Pr_state.equal_pr_status st.status Open && st.merge_ready

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
