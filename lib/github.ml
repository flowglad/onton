open Base

type error =
  | Http_error of { meth : string; path : string; status : int; body : string }
  | Json_parse_error of string
  | Graphql_error of string list
  | Transport_error of { meth : string; path : string; msg : string }

(* Extract GitHub's "message" field from a JSON error body, falling back to the
   raw body (truncated) when parsing fails. GitHub 4xx responses always include
   this field with a human-readable explanation. *)
let extract_github_message body =
  let truncate s =
    if String.length s <= 200 then s else String.sub s ~pos:0 ~len:200 ^ "…"
  in
  try
    let json = Yojson.Safe.from_string body in
    match Yojson.Safe.Util.(json |> member "message" |> to_string_option) with
    | Some msg -> msg
    | None -> truncate body
  with _ -> truncate body

(* Returns [true] if any [errors[].message] in a GitHub 422 validation response
   body contains [substring] (case-insensitive). 422 covers many distinct
   validation cases (no commits between head/base, head doesn't exist, branch
   already has PR, etc.) — callers use this to discriminate which one. Pure;
   safe on malformed input (returns false). *)
let response_error_message_contains body ~substring =
  try
    let json = Yojson.Safe.from_string body in
    let errors = Yojson.Safe.Util.(json |> member "errors" |> to_list) in
    let needle = String.lowercase substring in
    List.exists errors ~f:(fun err ->
        match
          Yojson.Safe.Util.(err |> member "message" |> to_string_option)
        with
        | None -> false
        | Some msg ->
            String.is_substring (String.lowercase msg) ~substring:needle)
  with _ -> false

let%test "response_error_message_contains: empty body returns false" =
  not (response_error_message_contains "" ~substring:"already exists")

let%test "response_error_message_contains: matches errors[].message substring" =
  response_error_message_contains
    {|{"message":"Validation Failed","errors":[{"resource":"PullRequest","code":"custom","message":"A pull request already exists for foo/bar:branch."}]}|}
    ~substring:"pull request already exists"

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
  | Json_parse_error msg -> Printf.sprintf "GitHub API JSON parse error: %s" msg
  | Graphql_error msgs ->
      Printf.sprintf "GitHub GraphQL error: %s" (String.concat ~sep:"; " msgs)

type t = { token : string; owner : string; repo : string }

let create ~token ~owner ~repo = { token; owner; repo }

let graphql_query =
  {|query($owner: String!, $repo: String!, $number: Int!) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $number) {
      state
      isDraft
      mergeable
      mergeStateStatus
      headRefName
      headRefOid
      baseRefName
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
      ]
  in
  `Assoc [ ("query", `String graphql_query); ("variables", variables) ]
  |> Yojson.Safe.to_string

let parse_merge_state = function
  | "MERGEABLE" -> Pr_state.Mergeable
  | "CONFLICTING" -> Pr_state.Conflicting
  | _ -> Pr_state.Unknown

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
  (* If [field] is absent or null we return [None]; if it's present but the
     nested [oid] is missing/non-string, [to_string_option] also returns [None].
     That second case is a GraphQL-schema-change red flag, but there's no
     structured logger here — callers treat a [None] SHA as "no anchor info",
     which is the safest fallback. *)
  let oid_of field =
    match node |> member field with
    | `Null -> None
    | obj -> obj |> member "oid" |> to_string_option
  in
  let commit_sha = oid_of "commit" in
  let original_commit_sha = oid_of "originalCommit" in
  Types.Comment.
    { id; thread_id; body; path; line; commit_sha; original_commit_sha }

let parse_response_json ~owner json =
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
              match pr |> member "mergeable" |> to_string_option with
              | Some s -> parse_merge_state s
              | None -> Pr_state.Unknown
            in
            let is_draft =
              pr |> member "isDraft" |> to_bool_option
              |> Option.value ~default:false
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
                      (* Derive check_status from individual conclusions.
                         We deliberately do NOT use the GraphQL rollup
                         state — it conflates cancelled runs (e.g.
                         superseded by a newer commit) with real failures.
                         See [Pr_state.derive_check_status] for semantics. *)
                      let status =
                        let base = Pr_state.derive_check_status checks in
                        (* Passing is only reliable when we've seen every check.
                           If the list is truncated, treat as Pending to avoid
                           approving a merge that might have failures on page 2+. *)
                        if
                          truncated
                          && Pr_state.equal_check_status base Pr_state.Passing
                        then Pr_state.Pending
                        else base
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
            let head_oid = pr |> member "headRefOid" |> to_string_option in
            let base_branch =
              pr |> member "baseRefName" |> to_string_option
              |> Option.map ~f:Types.Branch.of_string
            in
            let is_fork =
              match
                pr
                |> member "headRepositoryOwner"
                |> member "login" |> to_string_option
              with
              | Some head_owner -> not (String.equal head_owner owner)
              | None -> false
            in
            Ok
              {
                Pr_state.status;
                is_draft;
                merge_state;
                merge_ready;
                check_status;
                ci_checks;
                ci_checks_truncated;
                comments;
                unresolved_comment_count;
                head_branch;
                head_oid;
                base_branch;
                is_fork;
              })
    | errors ->
        let msgs =
          errors |> to_list
          |> List.map ~f:(fun e -> e |> member "message" |> to_string)
        in
        Error (Graphql_error msgs)
  with Yojson.Safe.Util.Type_error (msg, _) -> Error (Json_parse_error msg)

let parse_response ~owner body =
  try parse_response_json ~owner (Yojson.Safe.from_string body)
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

(** Internal: execute an HTTP request against api.github.com. Returns the raw
    response body on 2xx, [Http_error] otherwise. *)
let meth_to_string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PATCH -> "PATCH"
  | `PUT -> "PUT"

let request ~net t ~meth ~path ?(query = []) ?body () =
  let meth_s = meth_to_string meth in
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
        else Error (Http_error { meth = meth_s; path; status; body = resp_str }))
  with exn ->
    Error (Transport_error { meth = meth_s; path; msg = Exn.to_string exn })

let pr_state ~net t pr =
  let body = build_request_body t pr in
  match request ~net t ~meth:`POST ~path:"/graphql" ~body () with
  | Ok resp_str -> parse_response ~owner:t.owner resp_str
  | Error _ as e -> e

(** Parse the REST response from [GET /repos/:owner/:repo/pulls]. Returns a list
    of [(pr_number, base_branch, merged)] for non-CLOSED PRs, newest first. Pure
    function — no I/O. *)
let parse_rest_pr_list body =
  try
    match Yojson.Safe.from_string body with
    | `List entries ->
        let prs =
          List.filter_map entries ~f:(fun entry ->
              let open Yojson.Safe.Util in
              let number = entry |> member "number" |> to_int in
              let state =
                entry |> member "state" |> to_string |> String.lowercase
              in
              let merged_at = entry |> member "merged_at" in
              let base_ref =
                entry |> member "base" |> member "ref" |> to_string
              in
              match (state, merged_at) with
              | "closed", `Null -> None (* truly closed, not merged *)
              | _ ->
                  let merged =
                    match merged_at with `Null -> false | _ -> true
                  in
                  Some
                    ( Types.Pr_number.of_int number,
                      Types.Branch.of_string base_ref,
                      merged ))
        in
        Ok prs
    | _ -> Error (Json_parse_error "expected JSON array from REST PR list")
  with
  | Yojson.Json_error msg -> Error (Json_parse_error msg)
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (Json_parse_error msg)

(** List PRs for a branch via REST API. Returns non-CLOSED PRs. *)
let list_prs ~net t ~branch ?(base = None) ~state () =
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
  match request ~net t ~meth:`GET ~path ~query () with
  | Ok body -> parse_rest_pr_list body
  | Error _ as e -> e

(** Update the body (description) of a PR via REST API. *)
let update_pr_body ~net t ~pr_number ~body =
  let path =
    Printf.sprintf "/repos/%s/%s/pulls/%d" t.owner t.repo
      (Types.Pr_number.to_int pr_number)
  in
  let req_body = `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string in
  match request ~net t ~meth:`PATCH ~path ~body:req_body () with
  | Ok _ -> Ok ()
  | Error _ as e -> e

(** Create a draft pull request via REST API. Returns the new PR number. *)
let create_pull_request ~net t ~title ~head ~base ~body ~draft =
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
  match request ~net t ~meth:`POST ~path ~body:req_body () with
  | Error _ as e -> e
  | Ok resp_str -> (
      try
        let json = Yojson.Safe.from_string resp_str in
        let number = Yojson.Safe.Util.(json |> member "number" |> to_int) in
        Ok (Types.Pr_number.of_int number)
      with
      | Yojson.Json_error msg -> Error (Json_parse_error msg)
      | Yojson.Safe.Util.Type_error (msg, _) -> Error (Json_parse_error msg))

(** Update the base (target) branch of a PR via REST API. *)
let update_pr_base ~net t ~pr_number ~base =
  let path =
    Printf.sprintf "/repos/%s/%s/pulls/%d" t.owner t.repo
      (Types.Pr_number.to_int pr_number)
  in
  let req_body =
    `Assoc [ ("base", `String (Types.Branch.to_string base)) ]
    |> Yojson.Safe.to_string
  in
  match request ~net t ~meth:`PATCH ~path ~body:req_body () with
  | Ok _ -> Ok ()
  | Error _ as e -> e

(** Fetch the GraphQL node ID for a PR via REST API. *)
let pr_node_id ~net t ~pr_number =
  let path =
    Printf.sprintf "/repos/%s/%s/pulls/%d" t.owner t.repo
      (Types.Pr_number.to_int pr_number)
  in
  match request ~net t ~meth:`GET ~path () with
  | Error _ as e -> e
  | Ok body -> (
      try
        let json = Yojson.Safe.from_string body in
        let node_id =
          Yojson.Safe.Util.(json |> member "node_id" |> to_string)
        in
        Ok node_id
      with
      | Yojson.Json_error msg -> Error (Json_parse_error msg)
      | Yojson.Safe.Util.Type_error (msg, _) -> Error (Json_parse_error msg))

(** Set or unset draft status on a PR via GraphQL mutation. REST API does not
    support changing the draft field. *)
let set_draft ~net t ~pr_number ~draft =
  Result.bind (pr_node_id ~net t ~pr_number) ~f:(fun node_id ->
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
      match request ~net t ~meth:`POST ~path:"/graphql" ~body:req_body () with
      | Ok resp -> (
          try
            let json = Yojson.Safe.from_string resp in
            let open Yojson.Safe.Util in
            match json |> member "errors" with
            | `Null | `List [] -> Ok ()
            | errors ->
                let msgs =
                  errors |> to_list
                  |> List.map ~f:(fun e -> e |> member "message" |> to_string)
                in
                Error (Graphql_error msgs)
          with
          | Yojson.Json_error msg -> Error (Json_parse_error msg)
          | Yojson.Safe.Util.Type_error (msg, _) -> Error (Json_parse_error msg)
          )
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

(** Interpret a 2xx body from [PUT /pulls/:n/merge].
    - Valid JSON with [merged = true] → [Merge_succeeded].
    - Valid JSON with [merged = false] → [Merge_queued msg].
    - Anything else (malformed JSON, non-JSON body, missing/non-bool [merged]) →
      [Merge_unconfirmed]. GitHub's documented shape always includes
      [merged : bool], so absence is suspicious and we let the poller confirm
      rather than guessing. *)
let interpret_merge_response body =
  try
    let json = Yojson.Safe.from_string body in
    match Yojson.Safe.Util.(member "merged" json |> to_bool_option) with
    | Some true -> Merge_succeeded
    | Some false ->
        let msg =
          Yojson.Safe.Util.(member "message" json |> to_string_option)
          |> Option.value ~default:"merged=false"
        in
        Merge_queued msg
    | None -> Merge_unconfirmed
  with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ ->
    Merge_unconfirmed

(** Merge a pull request via the REST API. Maps to
    [PUT /repos/:owner/:repo/pulls/:number/merge]. Returns the parsed
    [merge_result] on 2xx or an [error] on transport/4xx/5xx failures. *)
let merge_pr ~net t ~pr_number ~merge_method =
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
  match request ~net t ~meth:`PUT ~path ~body:req_body () with
  | Ok body -> Ok (interpret_merge_response body)
  | Error _ as e -> e

let owner t = t.owner

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

(* Static assertion: Github satisfies Forge.S *)
let (_ : (module Forge.S)) =
  (module struct
    type nonrec t = t
    type nonrec error = error

    let show_error = show_error
    let pr_state = pr_state
  end)
