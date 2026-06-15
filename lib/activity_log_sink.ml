(* @archlint.module shell
   @archlint.domain activity-log *)

open Base
open Types

let assoc_fields = function `Assoc fields -> fields | _ -> []
let member fields name = List.Assoc.find fields ~equal:String.equal name

let bool_member fields name =
  match member fields name with Some (`Bool value) -> value | _ -> false

let int_member fields name =
  match member fields name with Some (`Int value) -> Some value | _ -> None

let string_member fields name =
  match member fields name with Some (`String value) -> Some value | _ -> None

let operation_kind_of_json json =
  match Json.try_of_yojson Operation_kind.t_of_yojson_compat json with
  | Ok op -> Some op
  | Error _ -> None

let operation_list_member fields name =
  match member fields name with
  | Some (`List values) -> List.filter_map values ~f:operation_kind_of_json
  | _ -> []

let current_op_member fields =
  match member fields "current_op" with
  | Some `Null | None -> None
  | Some json -> operation_kind_of_json json

let has_pr fields =
  match member fields "pr_status" with
  | Some (`Assoc pr_fields) -> (
      match string_member pr_fields "kind" with
      | Some ("present" | "missing") -> true
      | Some "absent" | Some _ | None -> false)
  | _ -> Option.is_some (int_member fields "pr_number")

let is_pr_missing fields =
  match member fields "pr_status" with
  | Some (`Assoc pr_fields) -> (
      match string_member pr_fields "kind" with
      | Some "missing" -> true
      | Some _ | None -> false)
  | _ -> false

let session_given_up fields =
  match member fields "session_fallback" with
  | Some (`String "Given_up") | Some (`List [ `String "Given_up" ]) -> true
  | Some _ | None -> false

let needs_intervention fields =
  if bool_member fields "merged" then false
  else
    let queue = operation_list_member fields "queue" in
    let human_in_queue =
      List.mem queue Operation_kind.Human ~equal:Operation_kind.equal
    in
    if session_given_up fields then true
    else if is_pr_missing fields then true
    else if human_in_queue then false
    else
      Option.value (int_member fields "ci_failure_count") ~default:0 >= 3
      || (not (has_pr fields))
         && Option.value
              (int_member fields "start_attempts_without_pr")
              ~default:0
            >= 2
      || Option.value (int_member fields "conflict_noop_count") ~default:0 >= 2
      || Option.value (int_member fields "no_commits_push_count") ~default:0
         >= 2
      || Option.value (int_member fields "context_exhaustion_count") ~default:0
         >= 2
      || Option.value (int_member fields "push_failure_count") ~default:0 >= 3
      || Option.value
           (int_member fields "pr_body_artifact_miss_count")
           ~default:0
         >= 2

let display_status_of_agent_json ~main_branch json =
  let fields = assoc_fields json in
  let patch_id =
    Option.value (string_member fields "patch_id") ~default:""
    |> Patch_id.of_string
  in
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_merged ~patch_id ~value:(bool_member fields "merged")
    |> State.Patch_ctx.set_needs_intervention ~patch_id
         ~value:(needs_intervention fields)
    |> State.Patch_ctx.set_approved ~patch_id
         ~value:(bool_member fields "satisfies")
    |> State.Patch_ctx.set_busy ~patch_id ~value:(bool_member fields "busy")
    |> State.Patch_ctx.set_has_pr ~patch_id ~value:(has_pr fields)
  in
  let ctx =
    match string_member fields "base_branch" with
    | Some branch ->
        State.Patch_ctx.set_base_branch ctx ~patch_id
          ~branch:(Branch.of_string branch)
    | None -> ctx
  in
  let ctx =
    List.fold (operation_list_member fields "queue") ~init:ctx
      ~f:(fun ctx kind ->
        State.Patch_ctx.set_queued ctx ~patch_id ~kind ~value:true)
  in
  Display_status.derive ctx ~patch_id ~current_op:(current_op_member fields)
    ~main_branch

let transition_action ~default_kind payload =
  let fields = assoc_fields payload in
  match string_member fields "event_log_kind" with
  | Some kind -> kind
  | None -> (
      match string_member fields "action" with
      | Some action -> action
      | None -> (
          match string_member fields "result" with
          | Some result -> default_kind ^ ": " ^ result
          | None -> default_kind))

let transition_of_payload ~main_branch ~timestamp ~patch_id ~default_kind
    payload =
  let fields = assoc_fields payload in
  match (member fields "agent_before", member fields "agent_after") with
  | Some before_json, Some after_json ->
      let from_status = display_status_of_agent_json ~main_branch before_json in
      let to_status = display_status_of_agent_json ~main_branch after_json in
      if Display_status.equal from_status to_status then None
      else
        Some
          (Activity_log.Transition_entry.create ~timestamp ~patch_id
             ~from_status ~to_status
             ~action:(transition_action ~default_kind payload))
  | _ -> None

let sink ~main_branch ~update () =
  let consume = function
    | Telemetry.Event.Free_form { patch_id; message; _ } ->
        let timestamp = Unix.gettimeofday () in
        update (fun log ->
            Activity_log.add_event log
              (Activity_log.Event.create ~timestamp ?patch_id message))
    | Stream { patch_id; raw; channel; _ } -> (
        match Activity_log.stream_kind_of_raw ~channel raw with
        | None -> ()
        | Some kind ->
            let timestamp = Unix.gettimeofday () in
            update (fun log ->
                Activity_log.add_stream_entry log
                  (Activity_log.Stream_entry.create ~timestamp ~patch_id ~kind))
        )
    | Poll { patch_id; payload } ->
        let timestamp = Unix.gettimeofday () in
        Option.iter
          (transition_of_payload ~main_branch ~timestamp ~patch_id
             ~default_kind:"poll" payload) ~f:(fun transition ->
            update (fun log -> Activity_log.add_transition log transition))
    | Action { patch_id; payload; _ } ->
        let timestamp = Unix.gettimeofday () in
        Option.iter
          (transition_of_payload ~main_branch ~timestamp ~patch_id
             ~default_kind:"action" payload) ~f:(fun transition ->
            update (fun log -> Activity_log.add_transition log transition))
    | Complete { patch_id; payload; _ } ->
        let timestamp = Unix.gettimeofday () in
        Option.iter
          (transition_of_payload ~main_branch ~timestamp ~patch_id
             ~default_kind:"complete" payload) ~f:(fun transition ->
            update (fun log -> Activity_log.add_transition log transition))
    | Spawn_started _ | Spawn_finalized _ -> ()
  in
  {
    Telemetry.Sink.name = "activity_log";
    interested_in =
      (function
      | Telemetry.Event.Free_form _ | Telemetry.Event.Stream _
      | Telemetry.Event.Poll _ | Telemetry.Event.Action _
      | Telemetry.Event.Complete _ ->
          true
      | Spawn_started _ | Spawn_finalized _ -> false);
    consume;
  }
