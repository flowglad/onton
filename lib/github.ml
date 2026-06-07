(* @archlint.module exempt
   @archlint.exempt-reason framework-boundary *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type error =
  | Http_error of { meth : string; path : string; status : int; body : string }
  | Json_parse_error of string
  | Graphql_error of string list
  | Timeout of { meth : string; path : string; seconds : float }
  | Transport_error of { meth : string; path : string; msg : string }

(* Extract GitHub's "message" field and validation details from a JSON error
   body, falling back to the raw body (truncated) when parsing fails. GitHub 422
   responses often put the useful cause in [errors[].message] while the top-level
   message is only "Validation Failed". *)
let extract_github_message body =
  let truncate s =
    if String.length s <= 200 then s else String.sub s ~pos:0 ~len:200 ^ "…"
  in
  try
    let json = Yojson.Safe.from_string body in
    let top_message = Json.string_field "message" json in
    let validation_messages =
      match Json.field "errors" json |> Option.bind ~f:Json.list with
      | Some errors -> List.filter_map errors ~f:(Json.string_field "message")
      | None -> []
    in
    let messages = validation_messages @ Option.to_list top_message in
    let messages = List.stable_dedup messages ~compare:String.compare in
    match messages with
    | [] -> truncate body
    | [ msg ] -> msg
    | msgs -> String.concat ~sep:": " msgs
  with _ -> truncate body

let response_error_messages body =
  try
    let json = Yojson.Safe.from_string body in
    let top_message = Json.string_field "message" json in
    let validation_messages =
      match Json.field "errors" json |> Option.bind ~f:Json.list with
      | Some errors ->
          List.concat_map errors ~f:(fun err ->
              [
                Json.string_field "message" err;
                Json.string_field "code" err;
                Json.string_field "field" err;
                Json.string_field "resource" err;
              ]
              |> List.filter_opt)
      | None -> []
    in
    Option.to_list top_message @ validation_messages
  with _ -> []

(* Returns [true] if any human-meaningful field in a GitHub error response
   contains [substring] (case-insensitive). 422 covers many distinct validation
   cases (no commits between head/base, head doesn't exist, branch already has
   PR, etc.) — callers use this to discriminate which one. Pure; safe on
   malformed input (returns false). *)
let response_error_message_contains body ~substring =
  let needle = String.lowercase substring in
  response_error_messages body
  |> List.exists ~f:(fun msg ->
      String.is_substring (String.lowercase msg) ~substring:needle)

let%test "response_error_message_contains: empty body returns false" =
  not (response_error_message_contains "" ~substring:"already exists")

let%test "response_error_message_contains: matches errors[].message substring" =
  response_error_message_contains
    {|{"message":"Validation Failed","errors":[{"resource":"PullRequest","code":"custom","message":"A pull request already exists for foo/bar:branch."}]}|}
    ~substring:"pull request already exists"

let%test "response_error_message_contains: matches top-level message substring"
    =
  response_error_message_contains {|{"message":"Already exists"}|}
    ~substring:"already exists"

let%test "response_error_message_contains: matches errors[].code substring" =
  response_error_message_contains
    {|{"message":"Validation Failed","errors":[{"resource":"PullRequest","code":"already_exists","field":"head"}]}|}
    ~substring:"already_exists"

let%test "response_error_message_contains: no match for unrelated error" =
  not
    (response_error_message_contains
       {|{"message":"Validation Failed","errors":[{"resource":"PullRequest","code":"custom","message":"No commits between main and feature."}]}|}
       ~substring:"pull request already exists")

let%test "response_error_message_contains: missing errors[] returns false" =
  not
    (response_error_message_contains {|{"message":"Not Found"}|}
       ~substring:"pull request already exists")

(* Hint added to permission-related HTTP errors so users know which PAT scopes
   to check. Fine-grained PATs need distinct permissions per endpoint category,
   which is the root cause of the opaque-error problem in issue #166. *)
let permission_hint = function
  | 401 -> " — check that your GITHUB_TOKEN is set and not expired"
  | 403 ->
      " — check your GH PAT scopes (classic: `repo`; fine-grained: Pull \
       requests read/write, Contents read, Metadata read)"
  | 404 -> " — resource not found, or your PAT lacks access to this repo"
  | _ -> ""

let show_error = function
  | Http_error { meth; path; status; body } ->
      Printf.sprintf "GitHub API %s %s → HTTP %d: %s%s" meth path status
        (extract_github_message body)
        (permission_hint status)
  | Transport_error { meth; path; msg } ->
      Printf.sprintf "GitHub API %s %s → transport error: %s" meth path msg
  | Timeout { meth; path; seconds } ->
      Printf.sprintf "GitHub API %s %s → request timed out after %.0fs" meth
        path seconds
  | Json_parse_error msg -> Printf.sprintf "GitHub API JSON parse error: %s" msg
  | Graphql_error msgs ->
      Printf.sprintf "GitHub GraphQL error: %s" (String.concat ~sep:"; " msgs)

type t = {
  token : string;
  owner : string;
  repo : string;
  main_branch : Types.Branch.t;
}

let create ~token ~owner ~repo ~main_branch =
  { token; owner; repo; main_branch }

let graphql_query =
  {|query($owner: String!, $repo: String!, $number: Int!, $mergeQueueBranch: String) {
  repository(owner: $owner, name: $repo) {
    mergeQueue(branch: $mergeQueueBranch) {
      id
    }
    pullRequest(number: $number) {
      id
      state
      isDraft
      mergeable
      mergeStateStatus
      reviewDecision
      headRefName
      headRefOid
      baseRefName
      mergeCommit { oid }
      mergeQueueEntry {
        id
        state
        position
      }
      headRepositoryOwner { login }
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
                    databaseId
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
          isOutdated
          # [comments(first: 1)] only returns the thread opener. If a caller
          # ever needs per-reply SHAs, raise this limit and add pagination.
          comments(first: 1) {
            nodes {
              databaseId
              body
              path
              line
              commit { oid }
              originalCommit { oid }
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
        ("mergeQueueBranch", `String (Types.Branch.to_string t.main_branch));
      ]
  in
  `Assoc [ ("query", `String graphql_query); ("variables", variables) ]
  |> Yojson.Safe.to_string

let parse_merge_state = function
  | "MERGEABLE" -> Pr_state.Mergeable
  | "CONFLICTING" -> Pr_state.Conflicting
  | _ -> Pr_state.Unknown

let parse_merge_queue_entry_state = function
  | "QUEUED" -> Pr_state.Mq_queued
  | "AWAITING_CHECKS" -> Pr_state.Mq_awaiting_checks
  | "MERGEABLE" -> Pr_state.Mq_mergeable
  | "UNMERGEABLE" -> Pr_state.Mq_unmergeable
  | "LOCKED" -> Pr_state.Mq_locked
  | _ -> Pr_state.Mq_queued

(* Typed model of the GraphQL PR response. Every nullable scalar/object is an
   [option] ([@yojson.default None] tolerates both a missing key and an explicit
   [null]); every record allows unmodeled fields so a schema addition never
   fails the parse. Decoding goes through [Json.try_of_yojson], so a shape
   mismatch becomes [Error (Json_parse_error _)] rather than a raised exception
   that fails the whole poll — the failure class behind PR #333. *)

type oid_obj = { oid : string option [@yojson.default None] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

(* A check-context node is an internally-tagged union on [__typename]
   (CheckRun | StatusContext) whose two arms share no keys. ppx's derived
   variant encoding does not match that wire shape, so decode every node into
   one flat all-optional record and dispatch in OCaml ([ci_check_of_context]).
   Optional [typename]/[name]/[context] preserve the "skip unrecognized or
   incomplete node" behavior of the previous hand-rolled parser. *)
type context_node = {
  typename : string option; [@key "__typename"] [@yojson.default None]
  database_id : int option; [@key "databaseId"] [@yojson.default None]
  name : string option; [@yojson.default None]
  conclusion : string option; [@yojson.default None]
  details_url : string option; [@key "detailsUrl"] [@yojson.default None]
  text : string option; [@yojson.default None]
  started_at : string option; [@key "startedAt"] [@yojson.default None]
  context : string option; [@yojson.default None]
  state : string option; [@yojson.default None]
  target_url : string option; [@key "targetUrl"] [@yojson.default None]
  description : string option; [@yojson.default None]
  created_at : string option; [@key "createdAt"] [@yojson.default None]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type page_info = {
  has_next_page : bool; [@key "hasNextPage"] [@yojson.default false]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type contexts = {
  page_info : page_info;
      [@key "pageInfo"] [@yojson.default { has_next_page = false }]
  nodes : context_node list; [@yojson.default []]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type status_check_rollup = {
  contexts : contexts;
      [@yojson.default { page_info = { has_next_page = false }; nodes = [] }]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type commit = {
  status_check_rollup : status_check_rollup option;
      [@key "statusCheckRollup"] [@yojson.default None]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type commit_node = {
  commit : commit; [@yojson.default { status_check_rollup = None }]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type commits = { nodes : commit_node list [@yojson.default []] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type comment_node = {
  database_id : int option; [@key "databaseId"] [@yojson.default None]
  body : string; [@yojson.default ""]
  path : string option; [@yojson.default None]
  line : int option; [@yojson.default None]
  commit : oid_obj option; [@yojson.default None]
  original_commit : oid_obj option;
      [@key "originalCommit"] [@yojson.default None]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type comment_connection = { nodes : comment_node list [@yojson.default []] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type review_thread = {
  id : string option; [@yojson.default None]
  is_resolved : bool; [@key "isResolved"] [@yojson.default false]
  is_outdated : bool; [@key "isOutdated"] [@yojson.default false]
  comments : comment_connection; [@yojson.default { nodes = [] }]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type review_threads = { nodes : review_thread list [@yojson.default []] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type repo_owner = { login : string option [@yojson.default None] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type merge_queue = Merge_queue_present

let merge_queue_of_yojson _ = Merge_queue_present

type merge_queue_entry_node = {
  id : string option; [@yojson.default None]
  state : string option; [@yojson.default None]
  position : int option; [@yojson.default None]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

let merge_queue_entry_of_node (node : merge_queue_entry_node) =
  match node.id with
  | None -> None
  | Some id ->
      let state =
        Option.value_map node.state ~default:Pr_state.Mq_queued
          ~f:parse_merge_queue_entry_state
      in
      let position = Option.value node.position ~default:0 in
      Some { Pr_state.id; state; position }

type pull_request = {
  id : string option; [@yojson.default None]
  state : string;
  mergeable : string option; [@yojson.default None]
  is_draft : bool; [@key "isDraft"] [@yojson.default false]
  merge_state_status : string option;
      [@key "mergeStateStatus"] [@yojson.default None]
  review_decision : string option;
      [@key "reviewDecision"] [@yojson.default None]
  head_ref_name : string option; [@key "headRefName"] [@yojson.default None]
  head_ref_oid : string option; [@key "headRefOid"] [@yojson.default None]
  base_ref_name : string option; [@key "baseRefName"] [@yojson.default None]
  merge_commit : oid_obj option; [@key "mergeCommit"] [@yojson.default None]
  commits : commits; [@yojson.default { nodes = [] }]
  review_threads : review_threads;
      [@key "reviewThreads"] [@yojson.default { nodes = [] }]
  head_repository_owner : repo_owner option;
      [@key "headRepositoryOwner"] [@yojson.default None]
  merge_queue_entry : merge_queue_entry_node option;
      [@key "mergeQueueEntry"] [@yojson.default None]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type repository = {
  merge_queue : merge_queue option; [@key "mergeQueue"] [@yojson.default None]
  pull_request : pull_request option; [@key "pullRequest"] [@yojson.default None]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type data = { repository : repository option [@yojson.default None] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type response = { data : data option [@yojson.default None] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

(* Map a decoded context node to a [Ci_check.t], or [None] to skip it.
   Byte-for-byte equivalent to the former [parse_check_context_node]: unknown or
   incomplete nodes are skipped, conclusion is lowercased with a "pending"
   default, and the CheckRun/StatusContext field asymmetry (text vs description,
   id present only for CheckRun) is preserved. *)
let ci_check_of_context (n : context_node) : Types.Ci_check.t option =
  match n.typename with
  | Some "CheckRun" ->
      Option.map n.name ~f:(fun name ->
          let conclusion =
            match n.conclusion with
            | Some c -> String.lowercase c
            | None -> "pending"
          in
          {
            Types.Ci_check.name;
            conclusion;
            details_url = n.details_url;
            description = n.text;
            started_at = n.started_at;
            id = n.database_id;
          })
  | Some "StatusContext" ->
      Option.map n.context ~f:(fun name ->
          let conclusion =
            match n.state with
            | Some s -> String.lowercase s
            | None -> "pending"
          in
          {
            Types.Ci_check.name;
            conclusion;
            details_url = n.target_url;
            description = n.description;
            started_at = n.created_at;
            id = None;
          })
  | Some _ | None -> None

let comment_of_node ~thread_id ~outdated (n : comment_node) : Types.Comment.t =
  let id =
    match n.database_id with
    | Some raw_id -> Types.Comment_id.of_int raw_id
    | None -> Types.Comment_id.next_synthetic ()
  in
  {
    Types.Comment.id;
    thread_id;
    body = n.body;
    path = n.path;
    line = n.line;
    commit_sha = Option.bind n.commit ~f:(fun o -> o.oid);
    original_commit_sha = Option.bind n.original_commit ~f:(fun o -> o.oid);
    outdated;
  }

let pr_state_of_pull_request ~owner ~merge_queue_required (pr : pull_request) :
    Pr_state.t =
  let status =
    match pr.state with
    | "MERGED" -> Pr_state.Merged
    | "CLOSED" -> Pr_state.Closed
    | _ -> Pr_state.Open
  in
  let merge_state =
    Option.value_map pr.mergeable ~default:Pr_state.Unknown ~f:parse_merge_state
  in
  let check_status, ci_checks, ci_checks_truncated =
    match pr.commits.nodes with
    | [] -> (Pr_state.Pending, [], false)
    | node :: _ -> (
        match node.commit.status_check_rollup with
        | None -> (Pr_state.Pending, [], false)
        | Some rollup ->
            let truncated = rollup.contexts.page_info.has_next_page in
            let checks =
              List.filter_map rollup.contexts.nodes ~f:ci_check_of_context
            in
            (* Derive check_status from individual conclusions; we deliberately
               do NOT use the GraphQL rollup state (it conflates cancelled runs
               with real failures). When truncated, downgrade Passing to Pending
               so we never approve a merge that might fail on page 2+. *)
            let status =
              let base = Pr_state.derive_check_status checks in
              if truncated && Pr_state.equal_check_status base Pr_state.Passing
              then Pr_state.Pending
              else base
            in
            (status, checks, truncated))
  in
  let merge_ready =
    Pr_state.merge_ready_of ~merge_state ~check_status
      ~review_decision:pr.review_decision
  in
  (* Diagnostics only: record when our derived readiness disagrees with GitHub's
     [mergeStateStatus] rollup. [merge_state_status] is otherwise unused — it is
     not carried into [Pr_state] state, and no decision reads it. *)
  let merge_ready_divergence =
    Pr_state.merge_ready_divergence_of ~merge_ready
      ~github_merge_state_status:pr.merge_state_status
  in
  let comments =
    List.concat_map pr.review_threads.nodes ~f:(fun thread ->
        if thread.is_resolved then []
        else
          (* [isOutdated] is the authoritative signal from GitHub: the thread's
             anchored lines were changed by a later commit. Do not infer it from
             [commit] vs [originalCommit] — GitHub advances [commit] to whatever
             commit still contains the line, flagging false positives. *)
          List.map thread.comments.nodes
            ~f:
              (comment_of_node ~thread_id:thread.id ~outdated:thread.is_outdated))
  in
  let unresolved_comment_count =
    List.count pr.review_threads.nodes ~f:(fun thread -> not thread.is_resolved)
  in
  let is_fork =
    match Option.bind pr.head_repository_owner ~f:(fun r -> r.login) with
    | Some head_owner -> not (String.equal head_owner owner)
    | None -> false
  in
  let merge_queue_entry =
    Option.bind pr.merge_queue_entry ~f:merge_queue_entry_of_node
  in
  {
    Pr_state.status;
    is_draft = pr.is_draft;
    merge_state;
    merge_ready;
    merge_ready_divergence;
    review_decision = pr.review_decision;
    check_status;
    ci_checks;
    ci_checks_truncated;
    comments;
    unresolved_comment_count;
    findings = [];
    (* GitHub does not produce review-service findings; the poller in
       [bin/main.ml] augments this list from configured review-service
       backends. *)
    node_id = pr.id;
    merge_queue_required;
    merge_queue_entry;
    head_branch = Option.map pr.head_ref_name ~f:Types.Branch.of_string;
    head_oid = pr.head_ref_oid;
    merge_commit_sha = Option.bind pr.merge_commit ~f:(fun o -> o.oid);
    base_branch = Option.map pr.base_ref_name ~f:Types.Branch.of_string;
    is_fork;
  }

let parse_response_json ~owner json =
  (* [Json.field] is [None] for both a missing and an explicit-null [errors],
     which is the "no errors" case; a present non-null [errors] is the GraphQL
     error arm. *)
  match Json.field "errors" json with
  | Some errors_json ->
      let msgs =
        match Json.list errors_json with
        | Some es -> List.filter_map es ~f:(Json.string_field "message")
        | None -> []
      in
      Error (Graphql_error msgs)
  | None -> (
      (* Decode the whole object: the [response] wrapper turns a null/absent
         [data]/[repository]/[pullRequest] into [None] rather than raising, so
         all three funnel to the same "pullRequest not found" as before. *)
      match Json.try_of_yojson response_of_yojson json with
      | Error msg -> Error (Json_parse_error msg)
      | Ok { data = None }
      | Ok { data = Some { repository = None } }
      | Ok { data = Some { repository = Some { pull_request = None; _ } } } ->
          Error (Json_parse_error "pullRequest not found")
      | Ok { data = Some { repository = Some repo } } -> (
          let merge_queue_required = Option.is_some repo.merge_queue in
          match repo.pull_request with
          | None -> Error (Json_parse_error "pullRequest not found")
          | Some pr ->
              Ok (pr_state_of_pull_request ~owner ~merge_queue_required pr)))

let parse_response ~owner body =
  try parse_response_json ~owner (Yojson.Safe.from_string body)
  with Yojson.Json_error msg -> Error (Json_parse_error msg)

let graphql_errors json =
  match Json.field "errors" json with
  | None -> None
  | Some errors_json -> (
      match Json.list errors_json with
      | Some [] -> None
      | Some errors ->
          Some (List.filter_map errors ~f:(Json.string_field "message"))
      | None -> Some [])

let merge_queue_entry_field ~context json =
  match json with
  | None -> Error (Json_parse_error (context ^ " missing mergeQueueEntry"))
  | Some entry_json -> (
      match Json.try_of_yojson merge_queue_entry_node_of_yojson entry_json with
      | Error msg -> Error (Json_parse_error msg)
      | Ok entry_node -> (
          match merge_queue_entry_of_node entry_node with
          | Some entry -> Ok entry
          | None ->
              Error (Json_parse_error (context ^ " mergeQueueEntry missing id"))
          ))

let parse_enqueue_response_json json =
  match graphql_errors json with
  | Some msgs -> Error (Graphql_error msgs)
  | None ->
      let entry_json =
        Json.field "data" json
        |> Option.bind ~f:(Json.field "enqueuePullRequest")
        |> Option.bind ~f:(Json.field "mergeQueueEntry")
      in
      merge_queue_entry_field ~context:"enqueuePullRequest" entry_json

let parse_enqueue_response body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error msg -> Error (Json_parse_error msg)
  | json -> parse_enqueue_response_json json

let parse_dequeue_response_json json =
  match graphql_errors json with
  | Some msgs -> Error (Graphql_error msgs)
  | None -> (
      match
        Json.field "data" json
        |> Option.bind ~f:(Json.field "dequeuePullRequest")
      with
      | Some _ -> Ok ()
      | None ->
          Error (Json_parse_error "dequeuePullRequest response missing payload")
      )

let parse_dequeue_response body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error msg -> Error (Json_parse_error msg)
  | json -> parse_dequeue_response_json json

type enqueue_pr_info_pull_request = {
  enqueue_info_id : string option; [@key "id"] [@yojson.default None]
  enqueue_info_head_ref_oid : string option;
      [@key "headRefOid"] [@yojson.default None]
  enqueue_info_merge_queue_entry : merge_queue_entry_node option;
      [@key "mergeQueueEntry"] [@yojson.default None]
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type enqueue_pr_info = {
  node_id : string;
  head_oid : string;
  merge_queue_entry : Pr_state.merge_queue_entry option;
}

let parse_enqueue_pr_info_response body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error msg -> Error (Json_parse_error msg)
  | json -> (
      match graphql_errors json with
      | Some msgs -> Error (Graphql_error msgs)
      | None -> (
          let pr_json =
            Json.field "data" json
            |> Option.bind ~f:(Json.field "repository")
            |> Option.bind ~f:(Json.field "pullRequest")
          in
          match pr_json with
          | None -> Error (Json_parse_error "pullRequest not found")
          | Some pr_json -> (
              match
                Json.try_of_yojson enqueue_pr_info_pull_request_of_yojson
                  pr_json
              with
              | Error msg -> Error (Json_parse_error msg)
              | Ok pr -> (
                  match (pr.enqueue_info_id, pr.enqueue_info_head_ref_oid) with
                  | Some node_id, Some head_oid ->
                      Ok
                        {
                          node_id;
                          head_oid;
                          merge_queue_entry =
                            Option.bind pr.enqueue_info_merge_queue_entry
                              ~f:merge_queue_entry_of_node;
                        }
                  | None, _ ->
                      Error (Json_parse_error "pullRequest response missing id")
                  | _, None ->
                      Error
                        (Json_parse_error
                           "pullRequest response missing headRefOid")))))

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

(** Default per-request timeout, in seconds. Matches review_service_client.
    Without a timeout, a TCP connect stuck in SYN_SENT can block the calling
    fiber indefinitely — which is how the poll loop wedged in production. *)
let default_timeout = 30.0

(** Internal: execute an HTTP request against api.github.com. Returns the raw
    response body on 2xx, [Http_error] otherwise. *)
let meth_to_string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PATCH -> "PATCH"
  | `PUT -> "PUT"

let request ~net ~clock ?(timeout = default_timeout) t ~meth ~path ?(query = [])
    ?body () =
  let meth_s = meth_to_string meth in
  let do_request () : (string, error) Result.t =
    try
      Mirage_crypto_rng_unix.use_default ();
      Result.bind
        (Result.map_error (https_config ()) ~f:(fun msg ->
             Transport_error { meth = meth_s; path; msg }))
        ~f:(fun tls_config ->
          let client =
            Cohttp_eio.Client.make ~https:(Some (https_fun tls_config)) net
          in
          let uri =
            Uri.of_string ("https://api.github.com" ^ path) |> fun u ->
            Uri.add_query_params u query
          in
          let headers =
            Http.Header.of_list
              [
                ("Authorization", "Bearer " ^ t.token);
                ("Content-Type", "application/json");
                ("Accept", "application/vnd.github+json");
                ("User-Agent", "onton/0.1.0");
                ("X-GitHub-Api-Version", "2022-11-28");
              ]
          in
          Eio.Switch.run @@ fun sw ->
          let resp, resp_body =
            match meth with
            | `GET -> Cohttp_eio.Client.get client ~sw ~headers uri
            | `POST ->
                let body =
                  Cohttp_eio.Body.of_string (Option.value body ~default:"{}")
                in
                Cohttp_eio.Client.post client ~sw ~headers ~body uri
            | `PATCH ->
                let body =
                  Cohttp_eio.Body.of_string (Option.value body ~default:"{}")
                in
                Cohttp_eio.Client.patch client ~sw ~headers ~body uri
            | `PUT ->
                let body =
                  Cohttp_eio.Body.of_string (Option.value body ~default:"{}")
                in
                Cohttp_eio.Client.put client ~sw ~headers ~body uri
          in
          let status = Http.Response.status resp |> Http.Status.to_int in
          let resp_str =
            Eio.Buf_read.(
              of_flow ~max_size:max_response_size resp_body |> take_all)
          in
          if status >= 200 && status < 300 then Ok resp_str
          else
            Error (Http_error { meth = meth_s; path; status; body = resp_str }))
    with
    | Eio.Cancel.Cancelled _ as exn ->
        (* Don't swallow cancellation — let [with_timeout] convert it to a
           [`Timeout] result so the caller sees a proper timeout error. *)
        raise exn
    | exn ->
        Error (Transport_error { meth = meth_s; path; msg = Exn.to_string exn })
  in
  match Eio.Time.with_timeout clock timeout (fun () -> Ok (do_request ())) with
  | Ok inner -> inner
  | Error `Timeout -> Error (Timeout { meth = meth_s; path; seconds = timeout })

let check_repo_access_internal ~net ~clock ?timeout t =
  let path = Printf.sprintf "/repos/%s/%s" t.owner t.repo in
  match request ~net ~clock ?timeout t ~meth:`GET ~path () with
  | Ok _ -> Ok ()
  | Error _ as e -> e

let pr_state ~net ~clock ?timeout t pr =
  let body = build_request_body t pr in
  match
    request ~net ~clock ?timeout t ~meth:`POST ~path:"/graphql" ~body ()
  with
  | Ok resp_str -> parse_response ~owner:t.owner resp_str
  | Error _ as e -> e

let enqueue_info_query =
  {|query($owner: String!, $repo: String!, $number: Int!) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $number) {
      id
      headRefOid
      mergeQueueEntry { id state position }
    }
  }
}|}

let build_enqueue_info_request_body t (pr : Types.Pr_number.t) =
  let variables =
    `Assoc
      [
        ("owner", `String t.owner);
        ("repo", `String t.repo);
        ("number", `Int (Types.Pr_number.to_int pr));
      ]
  in
  `Assoc [ ("query", `String enqueue_info_query); ("variables", variables) ]
  |> Yojson.Safe.to_string

let enqueue_pr_info ~net ~clock ?timeout t pr =
  let body = build_enqueue_info_request_body t pr in
  match
    request ~net ~clock ?timeout t ~meth:`POST ~path:"/graphql" ~body ()
  with
  | Ok resp_str -> parse_enqueue_pr_info_response resp_str
  | Error _ as e -> e

(** Parse the REST response from [GET /repos/:owner/:repo/pulls]. Returns a list
    of [(pr_number, base_branch, merged)] for non-CLOSED PRs, newest first. Pure
    function — no I/O. *)
let parse_rest_pr_list body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error msg -> Error (Json_parse_error msg)
  | `List entries ->
      let prs =
        List.filter_map entries ~f:(fun entry ->
            (* A null/absent [merged_at] means "not merged"; [Json.field]
               collapses both to [None]. Entries missing [number] or [base.ref]
               are skipped (malformed) rather than failing the whole list. *)
            let merged = Option.is_some (Json.field "merged_at" entry) in
            let state =
              Json.string_field "state" entry |> Option.map ~f:String.lowercase
            in
            let base_ref =
              Option.bind (Json.field "base" entry) ~f:(Json.string_field "ref")
            in
            match (Json.int_field "number" entry, base_ref) with
            | Some number, Some base_ref ->
                let truly_closed =
                  match state with Some "closed" -> not merged | _ -> false
                in
                if truly_closed then None
                else
                  Some
                    ( Types.Pr_number.of_int number,
                      Types.Branch.of_string base_ref,
                      merged )
            | _ -> None)
      in
      Ok prs
  | _ -> Error (Json_parse_error "expected JSON array from REST PR list")

(** List PRs for a branch via REST API. Returns non-CLOSED PRs. *)
let list_prs ~net ~clock ?timeout t ~branch ?(base = None) ~state () =
  let state_str = match state with `Open -> "open" | `All -> "all" in
  let query =
    [
      ("head", [ t.owner ^ ":" ^ Types.Branch.to_string branch ]);
      ("state", [ state_str ]);
      ("per_page", [ "100" ]);
    ]
    @
    match base with
    | Some b -> [ ("base", [ Types.Branch.to_string b ]) ]
    | None -> []
  in
  let path = Printf.sprintf "/repos/%s/%s/pulls" t.owner t.repo in
  match request ~net ~clock ?timeout t ~meth:`GET ~path ~query () with
  | Ok body -> parse_rest_pr_list body
  | Error _ as e -> e

(** Update the body (description) of a PR via REST API. *)
let update_pr_body ~net ~clock ?timeout t ~pr_number ~body =
  let path =
    Printf.sprintf "/repos/%s/%s/pulls/%d" t.owner t.repo
      (Types.Pr_number.to_int pr_number)
  in
  let req_body = `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string in
  match request ~net ~clock ?timeout t ~meth:`PATCH ~path ~body:req_body () with
  | Ok _ -> Ok ()
  | Error _ as e -> e

(** Create a draft pull request via REST API. Returns the new PR number. *)
let create_pull_request ~net ~clock ?timeout t ~title ~head ~base ~body ~draft =
  let path = Printf.sprintf "/repos/%s/%s/pulls" t.owner t.repo in
  let req_body =
    `Assoc
      [
        ("title", `String title);
        ("head", `String (Types.Branch.to_string head));
        ("base", `String (Types.Branch.to_string base));
        ("body", `String body);
        ("draft", `Bool draft);
      ]
    |> Yojson.Safe.to_string
  in
  match request ~net ~clock ?timeout t ~meth:`POST ~path ~body:req_body () with
  | Error _ as e -> e
  | Ok resp_str -> (
      match Yojson.Safe.from_string resp_str with
      | exception Yojson.Json_error msg -> Error (Json_parse_error msg)
      | json -> (
          match Json.int_field "number" json with
          | Some number -> Ok (Types.Pr_number.of_int number)
          | None ->
              Error
                (Json_parse_error "PR create response missing numeric 'number'")
          ))

(** Update the base (target) branch of a PR via REST API. *)
let update_pr_base ~net ~clock ?timeout t ~pr_number ~base =
  let path =
    Printf.sprintf "/repos/%s/%s/pulls/%d" t.owner t.repo
      (Types.Pr_number.to_int pr_number)
  in
  let req_body =
    `Assoc [ ("base", `String (Types.Branch.to_string base)) ]
    |> Yojson.Safe.to_string
  in
  match request ~net ~clock ?timeout t ~meth:`PATCH ~path ~body:req_body () with
  | Ok _ -> Ok ()
  | Error _ as e -> e

(** Fetch the GraphQL node ID for a PR via REST API. *)
let pr_node_id ~net ~clock ?timeout t ~pr_number =
  let path =
    Printf.sprintf "/repos/%s/%s/pulls/%d" t.owner t.repo
      (Types.Pr_number.to_int pr_number)
  in
  match request ~net ~clock ?timeout t ~meth:`GET ~path () with
  | Error _ as e -> e
  | Ok body -> (
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error msg -> Error (Json_parse_error msg)
      | json -> (
          match Json.string_field "node_id" json with
          | Some node_id -> Ok node_id
          | None -> Error (Json_parse_error "PR response missing 'node_id'")))

(** Set or unset draft status on a PR via GraphQL mutation. REST API does not
    support changing the draft field. *)
let set_draft ~net ~clock ?timeout t ~pr_number ~draft =
  Result.bind (pr_node_id ~net ~clock ?timeout t ~pr_number) ~f:(fun node_id ->
      let mutation =
        if draft then "convertPullRequestToDraft"
        else "markPullRequestReadyForReview"
      in
      let query =
        Printf.sprintf
          {|mutation($id: ID!) { %s(input: {pullRequestId: $id}) { pullRequest { isDraft } } }|}
          mutation
      in
      let req_body =
        `Assoc
          [
            ("query", `String query);
            ("variables", `Assoc [ ("id", `String node_id) ]);
          ]
        |> Yojson.Safe.to_string
      in
      match
        request ~net ~clock ?timeout t ~meth:`POST ~path:"/graphql"
          ~body:req_body ()
      with
      | Ok resp -> (
          match Yojson.Safe.from_string resp with
          | exception Yojson.Json_error msg -> Error (Json_parse_error msg)
          | json -> (
              match Json.field "errors" json |> Option.bind ~f:Json.list with
              | None | Some [] -> Ok () (* null / absent / empty errors *)
              | Some errors ->
                  Error
                    (Graphql_error
                       (List.filter_map errors ~f:(Json.string_field "message")))
              ))
      | Error _ as e -> e)

(** Outcome of a [PUT /pulls/:n/merge] call that returned a 2xx status. GitHub
    does not guarantee a 2xx means the merge completed — the endpoint is used
    for both immediate merges and the native auto-merge queue. *)
type merge_result =
  | Merge_succeeded
      (** Response body confirmed [merged = true]; the PR is merged. *)
  | Merge_queued of string
      (** Response body had [merged = false]; GitHub accepted the request
          (typically into its auto-merge queue waiting for required checks) but
          has not yet merged. Carries GitHub's [message] for logs. *)
  | Merge_unconfirmed
      (** Response was 2xx but did not include a parseable [merged] field (no
          JSON, unexpected shape, etc.). Treat as non-authoritative: don't mark
          the patch merged, but also don't count as a failure — the poller will
          observe the real PR state next cycle. *)

type enqueue_result =
  | Enqueued of Pr_state.merge_queue_entry
  | Already_enqueued of Pr_state.merge_queue_entry

(** Interpret a 2xx body from [PUT /pulls/:n/merge].
    - Valid JSON with [merged = true] → [Merge_succeeded].
    - Valid JSON with [merged = false] → [Merge_queued msg].
    - Anything else (malformed JSON, non-JSON body, missing/non-bool [merged]) →
      [Merge_unconfirmed]. GitHub's documented shape always includes
      [merged : bool], so absence is suspicious and we let the poller confirm
      rather than guessing. *)
let interpret_merge_response body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error _ -> Merge_unconfirmed
  | json -> (
      match Json.bool_field "merged" json with
      | Some true -> Merge_succeeded
      | Some false ->
          let msg =
            Option.value
              (Json.string_field "message" json)
              ~default:"merged=false"
          in
          Merge_queued msg
      | None -> Merge_unconfirmed)

(** Merge a pull request via the REST API. Maps to
    [PUT /repos/:owner/:repo/pulls/:number/merge]. Returns the parsed
    [merge_result] on 2xx or an [error] on transport/4xx/5xx failures. *)
let merge_pr ~net ~clock ?timeout t ~pr_number ~merge_method =
  let path =
    Printf.sprintf "/repos/%s/%s/pulls/%d/merge" t.owner t.repo
      (Types.Pr_number.to_int pr_number)
  in
  let method_str =
    match merge_method with
    | `Merge -> "merge"
    | `Squash -> "squash"
    | `Rebase -> "rebase"
  in
  let req_body =
    `Assoc [ ("merge_method", `String method_str) ] |> Yojson.Safe.to_string
  in
  match request ~net ~clock ?timeout t ~meth:`PUT ~path ~body:req_body () with
  | Ok body -> Ok (interpret_merge_response body)
  | Error _ as e -> e

let enqueue_pull_request_mutation =
  {|mutation($pullRequestId: ID!, $expectedHeadOid: GitObjectID!) {
  enqueuePullRequest(input: {
    pullRequestId: $pullRequestId,
    expectedHeadOid: $expectedHeadOid
  }) {
    mergeQueueEntry { id state position }
  }
}|}

let dequeue_pull_request_mutation =
  {|mutation($id: ID!) {
  dequeuePullRequest(input: { id: $id }) {
    clientMutationId
  }
}|}

let enqueue_pr ~net ~clock ?timeout t ~pr_number =
  Result.bind (enqueue_pr_info ~net ~clock ?timeout t pr_number) ~f:(fun info ->
      match info.merge_queue_entry with
      | Some entry -> Ok (Already_enqueued entry)
      | None -> (
          let req_body =
            `Assoc
              [
                ("query", `String enqueue_pull_request_mutation);
                ( "variables",
                  `Assoc
                    [
                      ("pullRequestId", `String info.node_id);
                      ("expectedHeadOid", `String info.head_oid);
                    ] );
              ]
            |> Yojson.Safe.to_string
          in
          match
            request ~net ~clock ?timeout t ~meth:`POST ~path:"/graphql"
              ~body:req_body ()
          with
          | Error _ as e -> e
          | Ok resp ->
              Result.map (parse_enqueue_response resp) ~f:(fun entry ->
                  Enqueued entry)))

let dequeue_pr ~net ~clock ?timeout t ~entry_id =
  let req_body =
    `Assoc
      [
        ("query", `String dequeue_pull_request_mutation);
        ("variables", `Assoc [ ("id", `String entry_id) ]);
      ]
    |> Yojson.Safe.to_string
  in
  match
    request ~net ~clock ?timeout t ~meth:`POST ~path:"/graphql" ~body:req_body
      ()
  with
  | Ok resp -> parse_dequeue_response resp
  | Error _ as e -> e

(* Which merge methods the repository permits. GitHub rejects a [PUT
   /pulls/:n/merge] whose [merge_method] is disabled with HTTP 405 ("Squash
   merges are not allowed on this repository", etc.), so onton must not assume
   any particular method is available — it detects and chooses one. *)
type repo_merge_methods = { squash : bool; merge : bool; rebase : bool }

let method_allowed (m : repo_merge_methods) = function
  | `Squash -> m.squash
  | `Merge -> m.merge
  | `Rebase -> m.rebase

let disable_method (m : repo_merge_methods) = function
  | `Squash -> { m with squash = false }
  | `Merge -> { m with merge = false }
  | `Rebase -> { m with rebase = false }

(* Preference order, most → least preferred. Squash first preserves onton's
   historical default on repos that allow it; the choice is method-agnostic
   downstream — rebase anchoring keys off the actual [mergeCommit.oid] and the
   dependency's pre-merge tip, not on the merge having been a squash. *)
let merge_method_preference = [ `Squash; `Merge; `Rebase ]

let allowed_methods_in_preference (m : repo_merge_methods) =
  List.filter merge_method_preference ~f:(method_allowed m)

(* Parse the [allow_*_merge] booleans from a [GET /repos/:owner/:repo] body. An
   absent field defaults to [true]: older GitHub Enterprise omits them, and the
   worst case of a wrong [true] is a 405 that [merge_pr]'s fallback recovers
   from. Returns [None] only when the body is not parseable JSON. *)
let parse_repo_merge_methods body : repo_merge_methods option =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error _ -> None
  | json ->
      (* Absent (or non-bool) → [true]: see comment above. [Json.bool_field]
         returns [None] for both, which [Option.value ~default:true] maps to the
         permissive default. *)
      let allowed field =
        Option.value (Json.bool_field field json) ~default:true
      in
      Some
        {
          squash = allowed "allow_squash_merge";
          merge = allowed "allow_merge_commit";
          rebase = allowed "allow_rebase_merge";
        }

let get_repo_merge_methods ~net ~clock ?timeout t :
    (repo_merge_methods, error) Result.t =
  let path = Printf.sprintf "/repos/%s/%s" t.owner t.repo in
  match request ~net ~clock ?timeout t ~meth:`GET ~path () with
  | Ok body -> (
      match parse_repo_merge_methods body with
      | Some m -> Ok m
      | None ->
          Error (Json_parse_error "could not parse repo merge-method settings"))
  | Error _ as e -> e

(* A non-2xx response meaning "this merge method is disabled on the repo"
   (GitHub 405 "… merges are not allowed on this repository"), as distinct from
   a transient/auth/conflict failure that must not trigger method fallback. *)
let is_method_not_allowed = function
  | Http_error { status = 405; body; _ } ->
      response_error_message_contains body ~substring:"not allowed"
  | Http_error _ | Json_parse_error _ | Graphql_error _ | Timeout _
  | Transport_error _ ->
      false

let is_merge_queue_required_error = function
  | Http_error { status = 405; body; _ } ->
      response_error_message_contains body ~substring:"merge queue"
  | Http_error _ | Json_parse_error _ | Graphql_error _ | Timeout _
  | Transport_error _ ->
      false

(* ── Inline tests ── *)

let%test "parse_rest_pr_list open PR" =
  let body =
    {|[{"number":42,"state":"open","merged_at":null,"base":{"ref":"main"},"node_id":"PR_1"}]|}
  in
  match parse_rest_pr_list body with
  | Ok [ (n, b, merged) ] ->
      Types.Pr_number.to_int n = 42
      && String.equal (Types.Branch.to_string b) "main"
      && not merged
  | Ok _ | Error _ -> false

let%test "parse_rest_pr_list merged PR" =
  let body =
    {|[{"number":10,"state":"closed","merged_at":"2024-01-01T00:00:00Z","base":{"ref":"develop"},"node_id":"PR_2"}]|}
  in
  match parse_rest_pr_list body with
  | Ok [ (n, b, merged) ] ->
      Types.Pr_number.to_int n = 10
      && String.equal (Types.Branch.to_string b) "develop"
      && merged
  | Ok _ | Error _ -> false

let%test "parse_rest_pr_list filters truly closed" =
  let body =
    {|[{"number":1,"state":"closed","merged_at":null,"base":{"ref":"main"},"node_id":"PR_3"}]|}
  in
  match parse_rest_pr_list body with Ok [] -> true | Ok _ | Error _ -> false

let%test "parse_rest_pr_list mixed" =
  let body =
    {|[{"number":5,"state":"open","merged_at":null,"base":{"ref":"main"},"node_id":"PR_4"},{"number":3,"state":"closed","merged_at":null,"base":{"ref":"main"},"node_id":"PR_5"},{"number":2,"state":"closed","merged_at":"2024-01-01T00:00:00Z","base":{"ref":"main"},"node_id":"PR_6"}]|}
  in
  match parse_rest_pr_list body with
  | Ok prs -> List.length prs = 2 (* open + merged, not the truly closed *)
  | Error _ -> false

let%test "parse_rest_pr_list invalid json" =
  match parse_rest_pr_list "not json" with Error _ -> true | Ok _ -> false

let%test "interpret_merge_response merged=true -> Merge_succeeded" =
  match
    interpret_merge_response
      {|{"sha":"abc","merged":true,"message":"Pull Request successfully merged"}|}
  with
  | Merge_succeeded -> true
  | Merge_queued _ | Merge_unconfirmed -> false

let%test "interpret_merge_response merged=false -> Merge_queued with message" =
  match
    interpret_merge_response
      {|{"merged":false,"message":"Required status check did not succeed"}|}
  with
  | Merge_queued msg -> String.equal msg "Required status check did not succeed"
  | Merge_succeeded | Merge_unconfirmed -> false

let%test "interpret_merge_response missing merged field -> Merge_unconfirmed" =
  match interpret_merge_response {|{"sha":"abc"}|} with
  | Merge_unconfirmed -> true
  | Merge_succeeded | Merge_queued _ -> false

let%test "interpret_merge_response non-json body -> Merge_unconfirmed" =
  match interpret_merge_response "" with
  | Merge_unconfirmed -> true
  | Merge_succeeded | Merge_queued _ -> false

let%test "parse_repo_merge_methods: all flags honored" =
  match
    parse_repo_merge_methods
      {|{"allow_squash_merge":false,"allow_merge_commit":true,"allow_rebase_merge":false}|}
  with
  | Some { squash = false; merge = true; rebase = false } -> true
  | _ -> false

let%test "parse_repo_merge_methods: absent fields default to allowed" =
  match parse_repo_merge_methods {|{"name":"repo"}|} with
  | Some { squash = true; merge = true; rebase = true } -> true
  | _ -> false

let%test "parse_repo_merge_methods: non-json -> None" =
  Option.is_none (parse_repo_merge_methods "not json")

let%test "allowed_methods_in_preference: squash-disabled repo picks merge first"
    =
  match
    allowed_methods_in_preference
      { squash = false; merge = true; rebase = true }
  with
  | [ `Merge; `Rebase ] -> true
  | _ -> false

let%test "allowed_methods_in_preference: squash preferred when allowed" =
  match
    allowed_methods_in_preference
      { squash = true; merge = true; rebase = false }
  with
  | [ `Squash; `Merge ] -> true
  | _ -> false

let%test "allowed_methods_in_preference: none allowed -> empty" =
  List.is_empty
    (allowed_methods_in_preference
       { squash = false; merge = false; rebase = false })

let%test "is_method_not_allowed: 405 'not allowed' -> true" =
  is_method_not_allowed
    (Http_error
       {
         meth = "PUT";
         path = "/merge";
         status = 405;
         body =
           {|{"message":"Squash merges are not allowed on this repository"}|};
       })

let%test "is_method_not_allowed: 405 without the phrase -> false" =
  not
    (is_method_not_allowed
       (Http_error
          {
            meth = "PUT";
            path = "/merge";
            status = 405;
            body = {|{"message":"Pull Request is not mergeable"}|};
          }))

let%test "is_method_not_allowed: 409 conflict -> false" =
  not
    (is_method_not_allowed
       (Http_error
          { meth = "PUT"; path = "/merge"; status = 409; body = "conflict" }))

let%expect_test "show_error includes endpoint + permission hint on 403" =
  let err =
    Http_error
      {
        meth = "PATCH";
        path = "/repos/foo/bar/pulls/42";
        status = 403;
        body = {|{"message":"Resource not accessible by integration"}|};
      }
  in
  Stdlib.print_endline (show_error err);
  [%expect
    {| GitHub API PATCH /repos/foo/bar/pulls/42 → HTTP 403: Resource not accessible by integration — check your GH PAT scopes (classic: `repo`; fine-grained: Pull requests read/write, Contents read, Metadata read) |}]

let%expect_test "show_error includes GitHub 422 validation details" =
  let err =
    Http_error
      {
        meth = "POST";
        path = "/repos/foo/bar/pulls";
        status = 422;
        body =
          {|{"message":"Validation Failed","errors":[{"resource":"PullRequest","code":"custom","message":"No commits between main and feature."}]}|};
      }
  in
  Stdlib.print_endline (show_error err);
  [%expect
    {| GitHub API POST /repos/foo/bar/pulls → HTTP 422: No commits between main and feature.: Validation Failed |}]

let%expect_test "show_error falls back to raw body when not JSON" =
  let err =
    Http_error { meth = "GET"; path = "/graphql"; status = 500; body = "oops" }
  in
  Stdlib.print_endline (show_error err);
  [%expect {| GitHub API GET /graphql → HTTP 500: oops |}]

let%expect_test "show_error transport error includes endpoint" =
  let err =
    Transport_error
      { meth = "POST"; path = "/graphql"; msg = "connection refused" }
  in
  Stdlib.print_endline (show_error err);
  [%expect {| GitHub API POST /graphql → transport error: connection refused |}]

let make ~net ~clock ~token ~owner ~repo ~main_branch :
    (module Forge.S with type error = error) =
  let client = create ~token ~owner ~repo ~main_branch in
  (* Cache the repo's allowed merge methods across calls; populated lazily on
     the first merge and narrowed if a 405 later reveals a method is disabled. *)
  let merge_methods_cache = ref None in
  let resolve_allowed_methods () =
    match !merge_methods_cache with
    | Some m -> Some m
    | None -> (
        match get_repo_merge_methods ~net ~clock client with
        | Ok m ->
            merge_methods_cache := Some m;
            Some m
        | Error _ -> None)
  in
  let merge_pr_choosing ~pr_number =
    (* Candidate methods, preferred first: the detected allowed set when we
       could read it, otherwise the full preference list (a failed detect must
       not block merges — the 405 fallback below still protects us). *)
    let order =
      match resolve_allowed_methods () with
      | Some m -> allowed_methods_in_preference m
      | None -> merge_method_preference
    in
    let order =
      if List.is_empty order then merge_method_preference else order
    in
    let rec attempt = function
      | [] ->
          assert false
          (* unreachable: order is always non-empty per the guard above *)
      | m :: rest -> (
          match merge_pr ~net ~clock client ~pr_number ~merge_method:m with
          | Error e when is_method_not_allowed e ->
              (* Stale allow-set: narrow the cache so future merges skip this
                 method, then try the next candidate. Disabling happens even
                 when [m] is the last candidate, so a repeat 405 isn't retried
                 on the next call. *)
              merge_methods_cache :=
                Option.map !merge_methods_cache ~f:(fun mm ->
                    disable_method mm m);
              if List.is_empty rest then Error e else attempt rest
          | (Ok _ | Error _) as other -> other)
    in
    attempt order
  in
  let module M = struct
    type nonrec error = error

    let show_error = show_error
    let owner = owner

    type nonrec merge_result = merge_result =
      | Merge_succeeded
      | Merge_queued of string
      | Merge_unconfirmed

    type nonrec enqueue_result = enqueue_result =
      | Enqueued of Pr_state.merge_queue_entry
      | Already_enqueued of Pr_state.merge_queue_entry

    let pr_state pr_number = pr_state ~net ~clock client pr_number

    let list_prs ~branch ?base ~state () =
      match base with
      | None -> list_prs ~net ~clock client ~branch ~state ()
      | Some base ->
          list_prs ~net ~clock client ~branch ~base:(Some base) ~state ()

    let update_pr_body ~pr_number ~body =
      update_pr_body ~net ~clock client ~pr_number ~body

    let create_pull_request ~title ~head ~base ~body ~draft =
      create_pull_request ~net ~clock client ~title ~head ~base ~body ~draft

    let update_pr_base ~pr_number ~base =
      update_pr_base ~net ~clock client ~pr_number ~base

    let set_draft ~pr_number ~draft =
      set_draft ~net ~clock client ~pr_number ~draft

    let merge_pr ~pr_number = merge_pr_choosing ~pr_number
    let enqueue_pr ~pr_number = enqueue_pr ~net ~clock client ~pr_number
    let dequeue_pr ~entry_id = dequeue_pr ~net ~clock client ~entry_id
    let check_repo_access () = check_repo_access_internal ~net ~clock client
  end in
  (module M)
