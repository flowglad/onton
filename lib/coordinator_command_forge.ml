(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)
open Base
open Types

[@@@warning "-4"]

let make ~helper ~owner =
  let call operation fields decode =
    let input =
      Yojson.Safe.to_string
        (`Assoc
           (("version", `Int 1) :: ("operation", `String operation) :: fields))
      ^ "\n"
    in
    match Resource_client.subprocess ~argv:[ helper ] ~input with
    | Error why -> Error why
    | Ok (0, bytes, _) -> (
        try
          match Yojson.Safe.from_string bytes with
          | `Assoc [ ("ok", `Bool true); ("value", value) ] -> Ok (decode value)
          | _ ->
              Error
                "forge helper did not return a confirmed current-schema result"
        with _ -> Error "malformed forge helper result")
    | Ok _ -> Error "forge helper failed"
  in
  let number pr = ("pr", `Int (Pr_number.to_int pr)) in
  let unit_value = function `Null -> () | _ -> failwith "expected null" in
  let mutate operation fields = call operation fields unit_value in
  (module struct
    type error = string

    let name = "command"
    let owner = owner
    let show_error e = e
    let poll_error e = Poll_outcome.Transport_failed { msg = e }
    let is_duplicate_change_error _ = false
    let is_permanent_error _ = false
    let is_merge_queue_required_error _ = false
    let supports_reviews = false
    let supports_pull_request_changes = true
    let supports_branch_changes = false

    type merge_result =
      | Merge_succeeded
      | Merge_queued of string
      | Merge_unconfirmed

    type enqueue_result =
      | Enqueued of Pr_state.merge_queue_entry
      | Already_enqueued of Pr_state.merge_queue_entry

    let change_url _ = None

    let branch_state ?known_state:_ _ =
      Error "branch-only observations are not supported"

    let pr_state pr = call "pr-state" [ number pr ] Pr_state.t_of_yojson

    let merge_queue_removal_checks ~pr_number =
      call "queue-removal"
        [ number pr_number ]
        (function
          | `List checks -> List.map checks ~f:Ci_check.t_of_yojson
          | _ -> failwith "checks")

    let check_failure_details ~check:_ = Error "unsupported"
    let rerun_failed_jobs_for_check ~check:_ = Error "unsupported"

    let list_prs ~branch ?base ~state () =
      call "list-prs"
        ([
           ("branch", `String (Branch.to_string branch));
           ( "state",
             `String (match state with `Open -> "open" | `All -> "all") );
         ]
        @ Option.to_list
            (Option.map base ~f:(fun b ->
                 ("base", `String (Branch.to_string b)))))
        (function
          | `List rows ->
              List.map rows ~f:(function
                | `List [ `Int n; `String url; `Bool merged ] when n > 0 ->
                    (Pr_number.of_int n, Branch.of_string url, merged)
                | _ -> failwith "PR list")
          | _ -> failwith "PR list")

    let update_pr_body ~pr_number ~body =
      mutate "set-body" [ number pr_number; ("body", `String body) ]

    let reply_to_review_comment ~pr_number:_ ~comment_id:_ ~body:_ =
      Error "unsupported"

    let resolve_review_thread ~thread_id:_ = Error "unsupported"
    let viewer_login () = None

    let create_pull_request ~title ~head ~base ~body ~draft =
      call "create"
        [
          ("title", `String title);
          ("head", `String (Branch.to_string head));
          ("base", `String (Branch.to_string base));
          ("body", `String body);
          ("draft", `Bool draft);
        ]
        (function
          | `Int n when n > 0 -> Pr_number.of_int n | _ -> failwith "PR number")

    let update_pr_base ~pr_number ~base =
      mutate "set-base"
        [ number pr_number; ("base", `String (Branch.to_string base)) ]

    let request_review ~pr_number:_ ~team_slug:_ = Error "unsupported"

    let set_draft ~pr_number ~draft =
      mutate "set-draft" [ number pr_number; ("draft", `Bool draft) ]

    let merge_pr ~pr_number =
      call "merge"
        [ number pr_number ]
        (function
          | `String "succeeded" -> Merge_succeeded
          | `String "unconfirmed" -> Merge_unconfirmed
          | `List [ `String "queued"; `String id ] -> Merge_queued id
          | _ -> failwith "merge result")

    let enqueue_pr ~pr_number =
      call "enqueue"
        [ number pr_number ]
        (fun json -> Enqueued (Pr_state.merge_queue_entry_of_yojson json))

    let dequeue_pr ~pr_number = mutate "dequeue" [ number pr_number ]
    let check_repo_access () = mutate "check-access" []
  end : Forge.S
    with type error = string)
