(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)

open Base
open Types

type error =
  | Http_error of { status : int; body : string }
  | Api_error of string
  | Timeout of float
  | Transport_error of string
  | Git_error of string
  | Unsupported of string

let show_error = function
  | Http_error { status; body } ->
      Printf.sprintf "builds.sr.ht HTTP %d: %s" status
        (String.prefix (String.strip body) 500)
  | Api_error message -> "builds.sr.ht API error: " ^ message
  | Timeout seconds ->
      Printf.sprintf "SourceHut operation timed out after %.0fs" seconds
  | Transport_error message -> "builds.sr.ht transport error: " ^ message
  | Git_error message -> "git.sr.ht operation failed: " ^ message
  | Unsupported operation ->
      Printf.sprintf "SourceHut does not support %s" operation

let https_config () =
  match Ca_certs.authenticator () with
  | Error (`Msg message) -> Error message
  | Ok authenticator -> (
      match Tls.Config.client ~authenticator () with
      | Ok config -> Ok config
      | Error (`Msg message) -> Error message)

let https_fun tls_config uri flow =
  let host =
    Uri.host uri
    |> Option.bind ~f:(fun value ->
        match Domain_name.of_string value with
        | Error _ -> None
        | Ok domain -> Result.ok (Domain_name.host domain))
  in
  (Tls_eio.client_of_flow tls_config ?host flow :> _ Eio.Flow.two_way)

let request_timeout = 30.0
let git_timeout = 60.0
let max_response_size = 2_000_000

let graphql ~net ~clock ~token ~query ~variables =
  let request_body =
    `Assoc [ ("query", `String query); ("variables", variables) ]
    |> Yojson.Safe.to_string
  in
  let perform () =
    try
      Mirage_crypto_rng_unix.use_default ();
      match https_config () with
      | Error message -> Error (Transport_error message)
      | Ok tls_config ->
          let client =
            Cohttp_eio.Client.make ~https:(Some (https_fun tls_config)) net
          in
          let headers =
            Http.Header.of_list
              [
                ("Authorization", "Bearer " ^ token);
                ("Content-Type", "application/json");
                ("Accept", "application/json");
                ("User-Agent", "onton/0.1.0");
              ]
          in
          Eio.Switch.run @@ fun sw ->
          let body = Cohttp_eio.Body.of_string request_body in
          let response, response_body =
            Cohttp_eio.Client.post client ~sw ~headers ~body
              (Uri.of_string "https://builds.sr.ht/query")
          in
          let status = Http.Response.status response |> Http.Status.to_int in
          let body =
            Eio.Buf_read.(
              of_flow ~max_size:max_response_size response_body |> take_all)
          in
          if status >= 200 && status < 300 then Ok body
          else Error (Http_error { status; body })
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> Error (Transport_error (Exn.to_string exn))
  in
  match
    Eio.Time.with_timeout clock request_timeout (fun () -> Ok (perform ()))
  with
  | Ok result -> result
  | Error `Timeout -> Error (Timeout request_timeout)

type capture = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let setsid_exec () =
  let candidate =
    match Stdlib.Sys.getenv_opt "ONTON_SETSID_EXEC" with
    | Some "" -> None
    | Some path -> Some path
    | None ->
        Some
          (Stdlib.Filename.concat
             (Stdlib.Filename.dirname Stdlib.Sys.executable_name)
             "onton-setsid-exec")
  in
  Option.filter candidate ~f:Stdlib.Sys.file_exists

let run_process ?(timeout = git_timeout) ~clock ~process_mgr command =
  let stdout = Buffer.create 256 in
  let stderr = Buffer.create 256 in
  let setsid_exec = setsid_exec () in
  let command =
    match setsid_exec with Some path -> path :: command | None -> command
  in
  try
    let outcome =
      Eio.Switch.run @@ fun sw ->
      let child =
        Eio.Process.spawn ~sw process_mgr ~env:(Git_env.clean_env ())
          ~stdout:(Eio.Flow.buffer_sink stdout)
          ~stderr:(Eio.Flow.buffer_sink stderr)
          command
      in
      let pid = Eio.Process.pid child in
      let kill_tree () =
        match setsid_exec with
        | Some _ -> (
            try Unix.kill (-pid) Stdlib.Sys.sigkill
            with Unix.Unix_error ((ESRCH | EPERM), _, _) -> ())
        | None -> (
            try Eio.Process.signal child Stdlib.Sys.sigkill with _ -> ())
      in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          kill_tree ();
          try ignore (Eio.Process.await child) with _ -> ())
        (fun () ->
          Eio.Time.with_timeout clock timeout (fun () ->
              Ok (Eio.Process.await child)))
    in
    match outcome with
    | Error `Timeout -> Error (Timeout timeout)
    | Ok status ->
        let status =
          match status with
          | `Exited code -> Unix.WEXITED code
          | `Signaled signal -> Unix.WSIGNALED signal
        in
        Ok
          {
            status;
            stdout = String.strip (Buffer.contents stdout);
            stderr = String.strip (Buffer.contents stderr);
          }
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (Git_error (Exn.to_string exn))

let run_git ~clock ~process_mgr ~repo_root args =
  run_process ~clock ~process_mgr ("git" :: "-C" :: repo_root :: args)

let is_timeout = function
  | Timeout _ -> true
  | Http_error _ | Api_error _ | Transport_error _ | Git_error _ | Unsupported _
    ->
      false

let%test_unit "SourceHut subprocess timeout is bounded" =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let started = Eio.Time.now clock in
  let result =
    run_process ~timeout:0.05 ~clock
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      [ "/bin/sleep"; "5" ]
  in
  let elapsed = Eio.Time.now clock -. started in
  assert (Option.value_map (Result.error result) ~default:false ~f:is_timeout);
  assert (Float.(elapsed < 1.0))

let process_succeeded = function
  | Unix.WEXITED 0 -> true
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> false

let git_success ~clock ~process_mgr ~repo_root args =
  Result.map (run_git ~clock ~process_mgr ~repo_root args) ~f:(fun capture ->
      process_succeeded capture.status)

let git_stdout ~clock ~process_mgr ~repo_root args =
  match run_git ~clock ~process_mgr ~repo_root args with
  | Ok capture
    when process_succeeded capture.status
         && not (String.is_empty capture.stdout) ->
      Ok capture.stdout
  | Ok
      ({ status = Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _; _ } as
       capture) ->
      Error
        (Git_error
           (String.concat ~sep:"\n" [ capture.stderr; capture.stdout ]
           |> String.strip))
  | Error _ as error -> error

type merge_tree_result = Clean of string | Conflicting

let merge_tree ~clock ~process_mgr ~repo_root ~base_sha ~head_sha =
  match
    run_git ~clock ~process_mgr ~repo_root
      [ "merge-tree"; "--write-tree"; base_sha; head_sha ]
  with
  | Ok { status = Unix.WEXITED 0; stdout; _ } when not (String.is_empty stdout)
    ->
      Ok (Clean stdout)
  | Ok { status = Unix.WEXITED 1; _ } -> Ok Conflicting
  | Ok
      ({ status = Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _; _ } as
       capture) ->
      Error
        (Git_error
           (String.concat ~sep:"\n" [ capture.stderr; capture.stdout ]
           |> String.strip))
  | Error _ as error -> error

let ref_name branch = "refs/remotes/origin/" ^ Branch.to_string branch

let resolve_ref ~clock ~process_mgr ~repo_root branch =
  git_stdout ~clock ~process_mgr ~repo_root
    [ "rev-parse"; "--verify"; ref_name branch ]

let jobs_query =
  {|query Jobs($cursor: Cursor) {
      jobs(cursor: $cursor) {
        results { id status note tags created manifest owner { canonicalName } }
        cursor
      }
    }|}

let job_query =
  {|query Job($id: Int!) {
      job(id: $id) {
        id status note tags created manifest visibility
        owner { canonicalName }
        log { last128KiB }
        tasks { name status log { last128KiB } }
      }
    }|}

let fetch_jobs ~net ~clock ~token =
  let rec loop cursor pages acc =
    if pages = 0 then Ok acc
    else
      let variables =
        `Assoc
          [
            ( "cursor",
              match cursor with None -> `Null | Some value -> `String value );
          ]
      in
      match graphql ~net ~clock ~token ~query:jobs_query ~variables with
      | Error _ as error -> error
      | Ok body -> (
          match Sourcehut_builds.jobs_of_response body with
          | Error message -> Error (Api_error message)
          | Ok (jobs, next) -> (
              let acc = acc @ jobs in
              match next with
              | None -> Ok acc
              | Some _ when List.is_empty jobs -> Ok acc
              | Some next -> loop (Some next) (pages - 1) acc))
  in
  loop None 8 []

let fetch_job ~net ~clock ~token id =
  match
    graphql ~net ~clock ~token ~query:job_query
      ~variables:(`Assoc [ ("id", `Int id) ])
  with
  | Error _ as error -> error
  | Ok body ->
      Result.map_error (Sourcehut_builds.job_of_response body)
        ~f:(fun message -> Api_error message)

let submit_query =
  {|mutation Rerun($manifest: String!, $tags: [String!], $note: String,
                    $visibility: Visibility) {
      submit(manifest: $manifest, tags: $tags, note: $note,
             visibility: $visibility) { id }
    }|}

let make ~net ~clock ~process_mgr ~token ~owner ~repo ~repo_root ~main_branch
    ~changes:initial_changes : (module Forge.S with type error = error) =
  let run_git = run_git ~clock ~process_mgr in
  let git_success = git_success ~clock ~process_mgr in
  let git_stdout = git_stdout ~clock ~process_mgr in
  let merge_tree = merge_tree ~clock ~process_mgr in
  let resolve_ref = resolve_ref ~clock ~process_mgr in
  let initial_registry, initial_error =
    match Sourcehut_target.restore_changes initial_changes with
    | Ok registry -> (registry, None)
    | Error message ->
        (Sourcehut_target.empty_registry, Some (Api_error message))
  in
  let changes = ref initial_registry in
  let initialization_error = ref initial_error in
  let record_change ~preferred_id ~branch ~base =
    match
      Sourcehut_target.register_change !changes ~preferred_id ~branch ~base
    with
    | Error message -> Error (Api_error message)
    | Ok (updated, id) ->
        changes := updated;
        Ok id
  in
  let find_change pr_number =
    match !initialization_error with
    | Some error -> Error error
    | None -> (
        match Sourcehut_target.find_change !changes pr_number with
        | Some change -> Ok change
        | None -> Error (Api_error "unknown local SourceHut change identifier"))
  in
  let unsupported operation = Error (Unsupported operation) in
  let module M = struct
    type nonrec error = error

    let name = "SourceHut"
    let show_error = show_error

    let poll_error = function
      | Timeout seconds -> Poll_outcome.Timed_out { seconds }
      | Transport_error msg | Git_error msg ->
          Poll_outcome.Transport_failed { msg }
      | Http_error { status; body } ->
          Poll_outcome.Http_failed
            { status; msg = String.prefix (String.strip body) 500 }
      | Api_error msg | Unsupported msg -> Poll_outcome.Json_parse_failed msg

    let is_duplicate_change_error _ = false

    let is_permanent_error = function
      | Http_error { status; _ } -> status >= 400 && status < 500
      | Unsupported _ -> true
      | Api_error _ | Timeout _ | Transport_error _ | Git_error _ -> false

    let is_merge_queue_required_error _ = false
    let supports_reviews = false
    let owner = owner

    let change_url pr_number =
      Result.ok (find_change pr_number)
      |> Option.map ~f:(fun (branch, _) ->
          Sourcehut_target.branch_url ~owner ~repo branch)

    type merge_result =
      | Merge_succeeded
      | Merge_queued of string
      | Merge_unconfirmed

    type enqueue_result =
      | Enqueued of Pr_state.merge_queue_entry
      | Already_enqueued of Pr_state.merge_queue_entry

    let pr_state pr_number =
      match find_change pr_number with
      | Error _ as error -> error
      | Ok (head, base) -> (
          match (resolve_ref ~repo_root head, resolve_ref ~repo_root base) with
          | Error error, _ -> Error error
          | Ok _, Error error -> Error error
          | Ok head_sha, Ok base_sha -> (
              match
                git_success ~repo_root
                  [ "merge-base"; "--is-ancestor"; head_sha; base_sha ]
              with
              | Error _ as error -> error
              | Ok merged -> (
                  let merge_state =
                    if merged then Ok Pr_state.Mergeable
                    else
                      Result.map (merge_tree ~repo_root ~base_sha ~head_sha)
                        ~f:(function
                        | Clean _ -> Pr_state.Mergeable
                        | Conflicting -> Pr_state.Conflicting)
                  in
                  match merge_state with
                  | Error _ as error -> error
                  | Ok merge_state -> (
                      match fetch_jobs ~net ~clock ~token with
                      | Error _ as error -> error
                      | Ok jobs ->
                          let ci_checks =
                            Sourcehut_builds.checks_for_commit ~owner ~repo
                              ~branch:head ~sha:head_sha jobs
                          in
                          let check_status =
                            Pr_state.derive_check_status ci_checks
                          in
                          let merge_ready =
                            Pr_state.merge_ready_of ~merge_state ~check_status
                              ~review_decision:None
                          in
                          Ok
                            {
                              Pr_state.status =
                                (if merged then Merged else Open);
                              is_draft = false;
                              merge_state;
                              merge_ready;
                              merge_ready_divergence = None;
                              review_decision = None;
                              check_status;
                              ci_checks;
                              ci_checks_truncated = false;
                              comments = [];
                              unresolved_comment_count = 0;
                              findings = [];
                              node_id = None;
                              merge_queue_required = false;
                              merge_queue_entry = None;
                              head_branch = Some head;
                              head_oid = Some head_sha;
                              merge_commit_sha =
                                (if merged then Some base_sha else None);
                              base_branch = Some base;
                              is_fork = false;
                            }))))

    let merge_queue_removal_checks ~pr_number:_ = Ok []

    let check_failure_details ~check =
      match check.Ci_check.id with
      | None -> Ok { Ci_log_digest.annotations = []; log = None }
      | Some id ->
          Result.map
            (fetch_job ~net ~clock ~token id)
            ~f:Sourcehut_builds.log_source

    let rerun_failed_jobs_for_check ~check =
      match check.Ci_check.id with
      | None -> Error (Api_error "SourceHut build has no job id")
      | Some id -> (
          match fetch_job ~net ~clock ~token id with
          | Error _ as error -> error
          | Ok job ->
              let visibility =
                match String.uppercase job.visibility with
                | ("PUBLIC" | "UNLISTED" | "PRIVATE") as value -> `String value
                | _ -> `Null
              in
              let variables =
                `Assoc
                  [
                    ("manifest", `String job.manifest);
                    ( "tags",
                      `List (List.map job.tags ~f:(fun tag -> `String tag)) );
                    ("note", `String job.note);
                    ("visibility", visibility);
                  ]
              in
              Result.bind
                (graphql ~net ~clock ~token ~query:submit_query ~variables)
                ~f:(fun body ->
                  Result.map_error (Sourcehut_builds.submit_id_of_response body)
                    ~f:(fun message -> Api_error message)
                  |> Result.map ~f:(fun _ -> ())))

    let list_prs ~branch ?base ~state () =
      let base =
        match (base, Sourcehut_target.find_branch !changes branch) with
        | Some base, _ -> base
        | None, Some (_, base) -> base
        | None, None -> main_branch
      in
      match record_change ~preferred_id:None ~branch ~base with
      | Error _ as error -> error
      | Ok id -> (
          match resolve_ref ~repo_root branch with
          | Error error -> if is_timeout error then Error error else Ok []
          | Ok head_sha -> (
              let merged_result =
                match resolve_ref ~repo_root base with
                | Error error ->
                    if is_timeout error then Error error else Ok false
                | Ok base_sha ->
                    git_success ~repo_root
                      [ "merge-base"; "--is-ancestor"; head_sha; base_sha ]
              in
              match merged_result with
              | Error _ as error -> error
              | Ok merged ->
                  if Poly.equal state `Open && merged then Ok []
                  else Ok [ (id, base, merged) ]))

    let update_pr_body ~pr_number:_ ~body:_ = Ok ()

    let reply_to_review_comment ~pr_number:_ ~comment_id:_ ~body:_ =
      unsupported "review comments"

    let resolve_review_thread ~thread_id:_ = unsupported "review threads"
    let viewer_login () = None

    let create_pull_request ~title:_ ~head ~base ~body:_ ~draft:_ =
      record_change ~preferred_id:None ~branch:head ~base

    let update_pr_base ~pr_number ~base =
      match find_change pr_number with
      | Error _ as error -> error
      | Ok (head, _) ->
          Result.map
            (record_change ~preferred_id:(Some pr_number) ~branch:head ~base)
            ~f:(fun _ -> ())

    let request_review ~pr_number:_ ~team_slug:_ = unsupported "code review"
    let set_draft ~pr_number:_ ~draft:_ = Ok ()

    let merge_pr ~pr_number =
      match find_change pr_number with
      | Error _ as error -> error
      | Ok (head, base) -> (
          match (resolve_ref ~repo_root head, resolve_ref ~repo_root base) with
          | Error error, _ | _, Error error -> Error error
          | Ok head_sha, Ok base_sha -> (
              match
                git_success ~repo_root
                  [ "merge-base"; "--is-ancestor"; base_sha; head_sha ]
              with
              | Error _ as error -> error
              | Ok base_is_ancestor -> (
                  let target =
                    if base_is_ancestor then Ok head_sha
                    else
                      match merge_tree ~repo_root ~base_sha ~head_sha with
                      | Error _ as error -> error
                      | Ok Conflicting ->
                          Error
                            (Git_error
                               (Printf.sprintf
                                  "%s does not merge cleanly into %s"
                                  (Branch.to_string head)
                                  (Branch.to_string base)))
                      | Ok (Clean tree) ->
                          git_stdout ~repo_root
                            [
                              "-c";
                              "user.name=onton";
                              "-c";
                              "user.email=onton@localhost";
                              "commit-tree";
                              tree;
                              "-p";
                              base_sha;
                              "-p";
                              head_sha;
                              "-m";
                              Printf.sprintf "Merge %s" (Branch.to_string head);
                            ]
                  in
                  match target with
                  | Error _ as error -> error
                  | Ok target_sha -> (
                      match
                        run_git ~repo_root
                          [
                            "push";
                            Printf.sprintf "--force-with-lease=refs/heads/%s:%s"
                              (Branch.to_string base) base_sha;
                            "origin";
                            Printf.sprintf "%s:refs/heads/%s" target_sha
                              (Branch.to_string base);
                          ]
                      with
                      | Ok capture when process_succeeded capture.status ->
                          Ok Merge_succeeded
                      | Ok capture ->
                          Error
                            (Git_error
                               (String.concat ~sep:"\n"
                                  [ capture.stderr; capture.stdout ]))
                      | Error _ as error -> error))))

    let enqueue_pr ~pr_number:_ = unsupported "merge queues"
    let dequeue_pr ~pr_number:_ = unsupported "merge queues"

    let check_repo_access () =
      if String.is_empty (String.strip token) then
        Error
          (Api_error
             "SRHT_TOKEN is required (JOBS:RW, LOGS:RO, and PROFILE:RO scopes)")
      else
        match resolve_ref ~repo_root main_branch with
        | Ok _ -> (
            match
              graphql ~net ~clock ~token ~query:jobs_query
                ~variables:(`Assoc [ ("cursor", `Null) ])
            with
            | Error _ as error -> error
            | Ok body -> (
                match Sourcehut_builds.jobs_of_response body with
                | Ok _ -> Ok ()
                | Error message -> Error (Api_error message)))
        | Error _ as error -> error
  end in
  (module M)
