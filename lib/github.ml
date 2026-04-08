open Base

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
      isDraft
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

(** Internal: execute an HTTP request against api.github.com. Returns the raw
    response body on 2xx, [Http_error] otherwise. *)
let request ~net t ~meth ~path ?(query = []) ?body () =
  try
    Mirage_crypto_rng_unix.use_default ();
    Result.bind
      (Result.map_error (https_config ()) ~f:(fun msg -> Transport_error msg))
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
        in
        let status = Http.Response.status resp |> Http.Status.to_int in
        let resp_str =
          Eio.Buf_read.(
            of_flow ~max_size:max_response_size resp_body |> take_all)
        in
        if status >= 200 && status < 300 then Ok resp_str
        else Error (Http_error (status, resp_str)))
  with exn -> Error (Transport_error (Exn.to_string exn))

let pr_state ~net t pr =
  let body = build_request_body t pr in
  match request ~net t ~meth:`POST ~path:"/graphql" ~body () with
  | Ok resp_str -> parse_response resp_str
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
      ("per_page", [ "10" ]);
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

(* Static assertion: Github satisfies Forge.S *)
let (_ : (module Forge.S)) =
  (module struct
    type nonrec t = t
    type nonrec error = error

    let show_error = show_error
    let pr_state = pr_state
  end)
