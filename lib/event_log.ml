open Base
open Types

type t = { path : string }

let max_file_size = 50 * 1024 * 1024 (* 50 MB *)

let create ~path =
  (try
     let stats = Unix.stat path in
     if stats.Unix.st_size > max_file_size then (
       (try Stdlib.Sys.remove (path ^ ".1") with Sys_error _ -> ());
       Stdlib.Sys.rename path (path ^ ".1"))
   with Unix.Unix_error _ | Sys_error _ -> ());
  { path }

let timestamp () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let agent_json (a : Patch_agent.t) = Persistence.patch_agent_to_yojson a
let poll_json (p : Poller.t) = Poller.yojson_of_t p

let write_entry t (json : Yojson.Safe.t) =
  try
    let oc =
      Stdlib.open_out_gen
        [ Stdlib.Open_append; Stdlib.Open_creat; Stdlib.Open_wronly ]
        0o644 t.path
    in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_out_noerr oc)
      (fun () ->
        Yojson.Safe.to_string json |> Stdlib.output_string oc;
        Stdlib.output_char oc '\n';
        Stdlib.flush oc)
  with Sys_error _ | Unix.Unix_error _ -> ()

let patch_id_json pid = Patch_id.yojson_of_t pid
let string_list_json strings = `List (List.map strings ~f:(fun s -> `String s))

let assoc_fields = function
  | `Assoc fields -> fields
  | payload -> [ ("payload", payload) ]

let field_string fields name =
  List.find_map fields ~f:(function
    | key, `String value when String.equal key name -> Some value
    | _ -> None)

let drop_internal_fields fields =
  List.filter fields ~f:(fun (key, _) ->
      not (String.equal key "event_log_kind"))

let entry ?kind_override t ~default_kind ~patch_id ~payload extra =
  let payload_fields = assoc_fields payload in
  let kind =
    match kind_override with
    | Some kind -> kind
    | None ->
        Option.value
          (field_string payload_fields "event_log_kind")
          ~default:default_kind
  in
  write_entry t
    (`Assoc
       ([ ("ts", `String (timestamp ())); ("kind", `String kind) ]
       @ (match patch_id with
         | Some patch_id -> [ ("patch_id", patch_id_json patch_id) ]
         | None -> [])
       @ extra
       @ drop_internal_fields payload_fields))

let interested_in = function
  | Telemetry.Event.Poll _ | Action _ | Complete _ -> true
  | Free_form _ | Stream _ | Spawn_started _ | Spawn_finalized _ -> false

let failure_subkind_of_session_result (result : Orchestrator.session_result) =
  match result with
  | Session_ok -> Failure_subkind.Ok
  | Session_process_error _ -> Failure_subkind.Process_error
  | Session_no_resume -> Failure_subkind.No_session_to_resume
  | Session_failed { detail = None; _ } -> Failure_subkind.Empty_response
  | Session_failed { detail = Some detail; _ }
    when String.is_empty detail || String.equal detail "(no error details)" ->
      Failure_subkind.Empty_response
  | Session_failed { detail = Some _; _ } ->
      (* This facade does not receive the init/stdout/stderr tails needed for
         detailed Failure_subkind.classify_session_failed classification.
         Keep the subkind structured and put raw detail only in the payload. *)
      Failure_subkind.Other "session_failed"
  | Session_give_up -> Failure_subkind.Other "session_give_up"
  | Session_worktree_missing -> Failure_subkind.Process_error
  | Session_push_failed -> Failure_subkind.Process_error
  | Session_no_commits -> Failure_subkind.Other "session_no_commits"

let sink t =
  {
    Telemetry.Sink.name = "event_log";
    interested_in;
    consume =
      (function
      | Telemetry.Event.Poll { patch_id; payload } ->
          entry t ~default_kind:"poll" ~patch_id:(Some patch_id) ~payload []
      | Action { patch_id; session_uuid; payload } ->
          let extra =
            match session_uuid with
            | Some uuid -> [ ("onton_session_uuid", `String uuid) ]
            | None -> []
          in
          entry t ~default_kind:"action" ~patch_id:(Some patch_id) ~payload
            extra
      | Complete { patch_id; session_uuid; subkind; payload } ->
          let extra =
            (match session_uuid with
              | Some uuid -> [ ("onton_session_uuid", `String uuid) ]
              | None -> [])
            @ [ ("subkind", Failure_subkind.yojson_of_t subkind) ]
          in
          entry t ~default_kind:"complete" ~patch_id:(Some patch_id) ~payload
            extra
      | Free_form _ -> ()
      | Stream _ | Spawn_started _ | Spawn_finalized _ -> ());
  }

let log_poll t ~patch_id ~poll_result ~agent_before ~agent_after ~logs =
  ignore t;
  Telemetry_dispatch.emit
    (Telemetry.Event.Poll
       {
         patch_id;
         payload =
           `Assoc
             [
               ("poll_result", poll_json poll_result);
               ("agent_before", agent_json agent_before);
               ("agent_after", agent_json agent_after);
               ("logs", string_list_json logs);
             ];
       })

let action_patch_id (action : Orchestrator.action) =
  match action with Start (pid, _) | Respond (pid, _) | Rebase (pid, _) -> pid

let log_action t ~action ~agent_before =
  ignore t;
  Telemetry_dispatch.emit
    (Telemetry.Event.Action
       {
         patch_id = action_patch_id action;
         session_uuid = None;
         payload =
           `Assoc
             [
               ("action", `String (Orchestrator.show_action action));
               ("agent_before", agent_json agent_before);
             ];
       })

let log_complete t ~patch_id ~result ~agent_before ~agent_after =
  ignore t;
  Telemetry_dispatch.emit
    (Telemetry.Event.Complete
       {
         patch_id;
         session_uuid = None;
         subkind = failure_subkind_of_session_result result;
         payload =
           `Assoc
             [
               ("result", `String (Orchestrator.show_session_result result));
               ("agent_before", agent_json agent_before);
               ("agent_after", agent_json agent_after);
             ];
       })

let log_force_complete t ~patch_id ~reason ~agent_before ~agent_after =
  ignore t;
  Telemetry_dispatch.emit
    (Telemetry.Event.Complete
       {
         patch_id;
         session_uuid = None;
         subkind = Failure_subkind.Process_error;
         payload =
           `Assoc
             [
               ("event_log_kind", `String "force_complete");
               ( "reason",
                 `String (Orchestrator.show_force_complete_reason reason) );
               ("agent_before", agent_json agent_before);
               ("agent_after", agent_json agent_after);
             ];
       })

let log_conflict_rebase t ~patch_id ~result ~decision ~agent_before ~agent_after
    =
  ignore t;
  Telemetry_dispatch.emit
    (Telemetry.Event.Action
       {
         patch_id;
         session_uuid = None;
         payload =
           `Assoc
             [
               ("event_log_kind", `String "conflict_rebase");
               ("result", `String (Worktree.show_rebase_result result));
               ( "decision",
                 `String (Orchestrator.show_conflict_rebase_decision decision)
               );
               ("agent_before", agent_json agent_before);
               ("agent_after", agent_json agent_after);
             ];
       })

let log_conflict_delivery t ~patch_id ~path ~rebase_in_progress ~git_status
    ~git_diff =
  ignore t;
  Telemetry_dispatch.emit
    (Telemetry.Event.Action
       {
         patch_id;
         session_uuid = None;
         payload =
           `Assoc
             [
               ("event_log_kind", `String "conflict_delivery");
               ("path", `String path);
               ("rebase_in_progress", `Bool rebase_in_progress);
               ("git_status", `String git_status);
               ("git_diff_len", `Int (String.length git_diff));
             ];
       })

let log_rebase t ~patch_id ~result ~agent_before ~agent_after =
  ignore t;
  Telemetry_dispatch.emit
    (Telemetry.Event.Action
       {
         patch_id;
         session_uuid = None;
         payload =
           `Assoc
             [
               ("event_log_kind", `String "rebase");
               ("result", `String (Worktree.show_rebase_result result));
               ("agent_before", agent_json agent_before);
               ("agent_after", agent_json agent_after);
             ];
       })
