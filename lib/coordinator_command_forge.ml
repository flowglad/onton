(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)
open Base
open Types

[@@@warning "-4"]

type helper_error = {
  message : string;
  duplicate_change : bool;
  permanent : bool;
  merge_queue_required : bool;
}

let helper_error ?(duplicate_change = false) ?(permanent = false)
    ?(merge_queue_required = false) message =
  { message; duplicate_change; permanent; merge_queue_required }

let exact_fields expected fields =
  List.length fields = List.length expected
  && List.for_all expected ~f:(fun key ->
      List.count fields ~f:(fun (name, _) -> String.equal name key) = 1)

let make ~helper ~owner =
  let call operation fields decode =
    let input =
      Yojson.Safe.to_string
        (`Assoc
           (("version", `Int 1) :: ("operation", `String operation) :: fields))
      ^ "\n"
    in
    let parse_error = function
      | `Assoc values
        when exact_fields
               [
                 "ok";
                 "error";
                 "duplicate_change";
                 "permanent";
                 "merge_queue_required";
               ]
               values -> (
          match
            ( List.Assoc.find values "ok" ~equal:String.equal,
              List.Assoc.find values "error" ~equal:String.equal,
              List.Assoc.find values "duplicate_change" ~equal:String.equal,
              List.Assoc.find values "permanent" ~equal:String.equal,
              List.Assoc.find values "merge_queue_required" ~equal:String.equal
            )
          with
          | ( Some (`Bool false),
              Some (`String message),
              Some (`Bool duplicate_change),
              Some (`Bool permanent),
              Some (`Bool merge_queue_required) ) ->
              Some
                (helper_error ~duplicate_change ~permanent ~merge_queue_required
                   message)
          | _ -> None)
      | _ -> None
    in
    let decode_reply status bytes stderr =
      try
        match Yojson.Safe.from_string bytes with
        | `Assoc values -> (
            match
              ( List.Assoc.find values "ok" ~equal:String.equal,
                List.Assoc.find values "value" ~equal:String.equal )
            with
            | Some (`Bool true), Some value
              when status = 0 && exact_fields [ "ok"; "value" ] values ->
                Ok (decode value)
            | _ -> (
                match parse_error (`Assoc values) with
                | Some error -> Error error
                | None ->
                    Error
                      (helper_error
                         (if String.is_empty (String.strip stderr) then
                            "forge helper returned an invalid current-schema \
                             result"
                          else String.strip stderr))))
        | json -> (
            match parse_error json with
            | Some error -> Error error
            | None ->
                Error
                  (helper_error
                     (if String.is_empty (String.strip stderr) then
                        "forge helper returned an invalid current-schema result"
                      else String.strip stderr)))
      with _ ->
        Error
          (helper_error
             (if String.is_empty (String.strip stderr) then
                "malformed forge helper result"
              else String.strip stderr))
    in
    match Resource_client.subprocess ~argv:[ helper ] ~input with
    | Error why -> Error (helper_error why)
    | Ok (status, bytes, stderr) -> decode_reply status bytes stderr
  in
  let number pr = ("pr", `Int (Pr_number.to_int pr)) in
  let unit_value = function `Null -> () | _ -> failwith "expected null" in
  let mutate operation fields = call operation fields unit_value in
  (module struct
    type error = helper_error

    let name = "command"
    let owner = owner
    let show_error e = e.message
    let poll_error e = Poll_outcome.Transport_failed { msg = e.message }
    let is_duplicate_change_error e = e.duplicate_change
    let is_permanent_error e = e.permanent
    let is_merge_queue_required_error e = e.merge_queue_required
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
      Error (helper_error "branch-only observations are not supported")

    let pr_state pr = call "pr-state" [ number pr ] Pr_state.t_of_yojson

    let merge_queue_removal_checks ~pr_number =
      call "queue-removal"
        [ number pr_number ]
        (function
          | `List checks -> List.map checks ~f:Ci_check.t_of_yojson
          | _ -> failwith "checks")

    let check_failure_details ~check:_ = Error (helper_error "unsupported")

    let rerun_failed_jobs_for_check ~check:_ =
      Error (helper_error "unsupported")

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
      Error (helper_error "unsupported")

    let resolve_review_thread ~thread_id:_ = Error (helper_error "unsupported")
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

    let request_review ~pr_number:_ ~team_slug:_ =
      Error (helper_error "unsupported")

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
    with type error = helper_error)
