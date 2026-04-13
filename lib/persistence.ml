open Base
open Types

(* ---------- helpers ---------- *)

let string_member key json = Yojson.Safe.Util.(member key json |> to_string)

let string_member_opt key json =
  Yojson.Safe.Util.(member key json |> to_string_option)

let int_member key json = Yojson.Safe.Util.(member key json |> to_int)
let bool_member key json = Yojson.Safe.Util.(member key json |> to_bool)
let list_member key json = Yojson.Safe.Util.(member key json |> to_list)

let int_member_opt key json =
  match Yojson.Safe.Util.member key json with
  | `Null -> None
  | v -> Some (Yojson.Safe.Util.to_int v)

let bool_member_opt key json =
  match Yojson.Safe.Util.member key json with
  | `Null -> None
  | v -> Some (Yojson.Safe.Util.to_bool v)

let result_all xs =
  List.fold_right xs ~init:(Ok []) ~f:(fun x acc ->
      Result.bind acc ~f:(fun tl -> Result.map x ~f:(fun hd -> hd :: tl)))

(** Wrap a raising ppx_yojson_conv deserializer into a Result.t. *)
let try_of_yojson f json =
  try Ok (f json) with
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
      Error (Stdlib.Printexc.to_string exn)
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Printf.sprintf "malformed json: %s" msg)

(* ---------- Patch_agent ---------- *)

let patch_agent_to_yojson (a : Patch_agent.t) =
  `Assoc
    [
      ("patch_id", Patch_id.yojson_of_t a.patch_id);
      ("branch", Branch.yojson_of_t a.branch);
      ( "pr_number",
        match a.pr_number with
        | None -> `Null
        | Some n -> Pr_number.yojson_of_t n );
      ("has_session", `Bool a.has_session);
      ("busy", `Bool a.busy);
      ("merged", `Bool a.merged);
      ("queue", `List (List.map a.queue ~f:Operation_kind.yojson_of_t));
      ("satisfies", `Bool a.satisfies);
      ("changed", `Bool a.changed);
      ("has_conflict", `Bool a.has_conflict);
      ( "base_branch",
        match a.base_branch with
        | None -> `Null
        | Some b -> Branch.yojson_of_t b );
      ( "notified_base_branch",
        match a.notified_base_branch with
        | None -> `Null
        | Some b -> Branch.yojson_of_t b );
      ("ci_failure_count", `Int a.ci_failure_count);
      ( "session_fallback",
        Patch_agent.yojson_of_session_fallback a.session_fallback );
      ( "human_messages",
        `List (List.map a.human_messages ~f:(fun s -> `String s)) );
      ( "inflight_human_messages",
        `List (List.map a.inflight_human_messages ~f:(fun s -> `String s)) );
      ("ci_checks", `List (List.map a.ci_checks ~f:Ci_check.yojson_of_t));
      ("merge_ready", `Bool a.merge_ready);
      ("is_draft", `Bool a.is_draft);
      ("pr_body_delivered", `Bool a.pr_body_delivered);
      ("implementation_notes_delivered", `Bool a.implementation_notes_delivered);
      ("start_attempts_without_pr", `Int a.start_attempts_without_pr);
      ("conflict_noop_count", `Int a.conflict_noop_count);
      ("no_commits_push_count", `Int a.no_commits_push_count);
      ("checks_passing", `Bool a.checks_passing);
      ( "current_op",
        match a.current_op with
        | None -> `Null
        | Some op -> Operation_kind.yojson_of_t op );
      ( "current_message_id",
        match a.current_message_id with
        | None -> `Null
        | Some id -> Message_id.yojson_of_t id );
      ("generation", `Int a.generation);
      ( "worktree_path",
        match a.worktree_path with None -> `Null | Some p -> `String p );
      ("branch_blocked", `Bool a.branch_blocked);
      ( "llm_session_id",
        match a.llm_session_id with None -> `Null | Some s -> `String s );
    ]

let patch_agent_of_yojson ~gameplan json =
  let ( let* ) r f = Result.bind r ~f in
  let* queue =
    result_all
      (List.map (list_member "queue" json) ~f:(fun j ->
           try_of_yojson Operation_kind.t_of_yojson j))
  in
  let human_messages =
    match Yojson.Safe.Util.member "human_messages" json with
    | `List items ->
        List.filter_map items ~f:(fun j -> Yojson.Safe.Util.to_string_option j)
    | _ -> []
  in
  let inflight_human_messages =
    match Yojson.Safe.Util.member "inflight_human_messages" json with
    | `List items ->
        List.filter_map items ~f:(fun j -> Yojson.Safe.Util.to_string_option j)
    | _ -> []
  in
  let* session_fallback =
    match Yojson.Safe.Util.member "session_fallback" json with
    | `Null -> Error "patch_agent: missing session_fallback"
    | v -> try_of_yojson Patch_agent.session_fallback_of_yojson v
  in
  let ci_checks_raw = list_member "ci_checks" json in
  let* ci_checks =
    result_all
      (List.map ci_checks_raw ~f:(fun j -> try_of_yojson Ci_check.t_of_yojson j))
  in
  let has_session = bool_member "has_session" json in
  Ok
    (Patch_agent.restore
       ~patch_id:(Patch_id.of_string (string_member "patch_id" json))
       ~branch:
         (let pid = string_member "patch_id" json in
          (* Backward compat: old ad-hoc agents stored a synthetic "adhoc-N"
             branch and the real branch in head_branch. Prefer head_branch
             when present to migrate to the unified branch field. *)
          let raw = string_member_opt "branch" json in
          let head = string_member_opt "head_branch" json in
          (* Treat synthetic "adhoc-N" raw values as unresolvable so they
             fall through to the gameplan/pid default instead of creating
             ghost worktrees that silently block worktree creation. *)
          let resolved =
            match head with
            | Some _ -> head
            | None -> (
                match raw with
                | Some r
                  when String.is_prefix r ~prefix:"adhoc-"
                       && String.for_all (String.drop_prefix r 6)
                            ~f:Char.is_digit ->
                    None
                | _ -> raw)
          in
          Branch.of_string
            (Option.value resolved
               ~default:
                 (match
                    List.find gameplan.Gameplan.patches ~f:(fun p ->
                        String.equal (Patch_id.to_string p.Patch.id) pid)
                  with
                 | Some p -> Branch.to_string p.Patch.branch
                 | None -> pid)))
       ~pr_number:
         (int_member_opt "pr_number" json |> Option.map ~f:Pr_number.of_int)
       ~has_session ~busy:(bool_member "busy" json)
       ~merged:(bool_member "merged" json)
       ~queue
       ~satisfies:(bool_member "satisfies" json)
       ~changed:(bool_member "changed" json)
       ~has_conflict:(bool_member "has_conflict" json)
       ~base_branch:
         (string_member_opt "base_branch" json |> Option.map ~f:Branch.of_string)
       ~notified_base_branch:
         (match string_member_opt "notified_base_branch" json with
         | Some s -> Some (Branch.of_string s)
         | None ->
             (* Backward compat: only infer "already notified" for agents with
                an active/established session. *)
             if has_session then
               string_member_opt "base_branch" json
               |> Option.map ~f:Branch.of_string
             else None)
       ~ci_failure_count:(int_member "ci_failure_count" json)
       ~session_fallback ~human_messages ~inflight_human_messages ~ci_checks
       ~merge_ready:(bool_member "merge_ready" json)
       ~is_draft:(bool_member "is_draft" json)
       ~pr_body_delivered:
         (Option.value (bool_member_opt "pr_body_delivered" json) ~default:true)
       ~implementation_notes_delivered:
         (bool_member "implementation_notes_delivered" json)
       ~start_attempts_without_pr:(int_member "start_attempts_without_pr" json)
       ~conflict_noop_count:
         (Option.value (int_member_opt "conflict_noop_count" json) ~default:0)
       ~no_commits_push_count:
         (Option.value (int_member_opt "no_commits_push_count" json) ~default:0)
       ~checks_passing:(bool_member "checks_passing" json)
       ~current_op:
         (match Yojson.Safe.Util.member "current_op" json with
         | `Null -> None
         | v -> (
             match try_of_yojson Operation_kind.t_of_yojson v with
             | Ok op -> Some op
             | Error _ -> None))
       ~current_message_id:
         (string_member_opt "current_message_id" json
         |> Option.map ~f:Message_id.of_string)
       ~generation:(int_member "generation" json)
       ~worktree_path:(string_member_opt "worktree_path" json)
       ~branch_blocked:(bool_member "branch_blocked" json)
       ~llm_session_id:(string_member_opt "llm_session_id" json))

(* ---------- Activity_log ---------- *)

let activity_log_to_yojson (log : Activity_log.t) =
  let transitions = Activity_log.recent_transitions log ~limit:Int.max_value in
  let events = Activity_log.recent_events log ~limit:Int.max_value in
  `Assoc
    [
      ( "transitions",
        `List
          (List.map transitions ~f:Activity_log.Transition_entry.yojson_of_t) );
      ("events", `List (List.map events ~f:Activity_log.Event.yojson_of_t));
    ]

let activity_log_of_yojson json =
  Result.bind
    (result_all
       (List.map (list_member "transitions" json) ~f:(fun j ->
            try_of_yojson Activity_log.Transition_entry.t_of_yojson j)))
    ~f:(fun transitions ->
      Result.map
        (result_all
           (List.map (list_member "events" json) ~f:(fun j ->
                try_of_yojson Activity_log.Event.t_of_yojson j)))
        ~f:(fun events ->
          (* Entries are stored newest-first; restore by folding in reverse *)
          let log =
            List.fold_right transitions ~init:Activity_log.empty
              ~f:(fun entry acc -> Activity_log.add_transition acc entry)
          in
          List.fold_right events ~init:log ~f:(fun entry acc ->
              Activity_log.add_event acc entry)))

(* ---------- Orchestrator ---------- *)

let orchestrator_to_yojson (o : Orchestrator.t) =
  let agents =
    Orchestrator.agents_map o |> Map.to_alist
    |> List.map ~f:(fun (pid, agent) ->
        (Patch_id.to_string pid, patch_agent_to_yojson agent))
  in
  let outbox =
    Orchestrator.all_messages o
    |> List.map ~f:(fun (msg : Orchestrator.patch_agent_message) ->
        ( Message_id.to_string msg.message_id,
          `Assoc
            [
              ("patch_id", Patch_id.yojson_of_t msg.patch_id);
              ("generation", `Int msg.generation);
              ( "action",
                match msg.action with
                | Orchestrator.Start (patch_id, base_branch) ->
                    `Assoc
                      [
                        ("kind", `String "start");
                        ("patch_id", Patch_id.yojson_of_t patch_id);
                        ("base_branch", Branch.yojson_of_t base_branch);
                      ]
                | Orchestrator.Respond (patch_id, operation_kind) ->
                    `Assoc
                      [
                        ("kind", `String "respond");
                        ("patch_id", Patch_id.yojson_of_t patch_id);
                        ( "operation_kind",
                          Operation_kind.yojson_of_t operation_kind );
                      ]
                | Orchestrator.Rebase (patch_id, base_branch) ->
                    `Assoc
                      [
                        ("kind", `String "rebase");
                        ("patch_id", Patch_id.yojson_of_t patch_id);
                        ("base_branch", Branch.yojson_of_t base_branch);
                      ] );
              ("payload_hash", `String msg.payload_hash);
              ( "status",
                `String
                  (match msg.status with
                  | Orchestrator.Pending -> "Pending"
                  | Orchestrator.Acked -> "Acked"
                  | Orchestrator.Completed -> "Completed"
                  | Orchestrator.Obsolete -> "Obsolete") );
            ] ))
  in
  `Assoc
    [
      ("main_branch", Branch.yojson_of_t (Orchestrator.main_branch o));
      ("agents", `Assoc agents);
      ("outbox", `Assoc outbox);
    ]

let action_of_yojson json =
  let ( let* ) r f = Result.bind r ~f in
  match string_member "kind" json with
  | "start" ->
      Ok
        (Orchestrator.Start
           ( Patch_id.of_string (string_member "patch_id" json),
             Branch.of_string (string_member "base_branch" json) ))
  | "respond" ->
      let* op_kind =
        try_of_yojson Operation_kind.t_of_yojson
          (Yojson.Safe.Util.member "operation_kind" json)
      in
      Ok
        (Orchestrator.Respond
           (Patch_id.of_string (string_member "patch_id" json), op_kind))
  | "rebase" ->
      Ok
        (Orchestrator.Rebase
           ( Patch_id.of_string (string_member "patch_id" json),
             Branch.of_string (string_member "base_branch" json) ))
  | other -> Error (Printf.sprintf "unknown action kind: %s" other)

let message_status_of_string = function
  | "Pending" -> Ok Orchestrator.Pending
  | "Acked" -> Ok Orchestrator.Acked
  | "Completed" -> Ok Orchestrator.Completed
  | "Obsolete" -> Ok Orchestrator.Obsolete
  | other -> Error (Printf.sprintf "unknown message status: %s" other)

let orchestrator_of_yojson ~gameplan json =
  try
    let ( let* ) r f = Result.bind r ~f in
    let graph = Graph.of_patches gameplan.Gameplan.patches in
    let main_branch = Branch.of_string (string_member "main_branch" json) in
    Result.bind
      (result_all
         (Yojson.Safe.Util.member "agents" json
         |> Yojson.Safe.Util.to_assoc
         |> List.map ~f:(fun (key, value) ->
             Result.bind (patch_agent_of_yojson ~gameplan value)
               ~f:(fun agent ->
                 let payload_id = Patch_id.to_string agent.patch_id in
                 if String.equal key payload_id then
                   Ok (Patch_id.of_string key, agent)
                 else
                   Error
                     (Printf.sprintf
                        "agent key/payload mismatch: key=%s payload=%s" key
                        payload_id)))))
      ~f:(fun agents ->
        let agents_map =
          List.fold agents
            ~init:(Map.empty (module Patch_id))
            ~f:(fun acc (k, v) -> Map.set acc ~key:k ~data:v)
        in
        let outbox =
          Yojson.Safe.Util.member "outbox" json
          |> Yojson.Safe.Util.to_assoc
          |> List.fold
               ~init:(Ok (Map.empty (module Message_id)))
               ~f:(fun acc_result (key, value) ->
                 let* acc = acc_result in
                 let patch_id =
                   Patch_id.of_string (string_member "patch_id" value)
                 in
                 let generation = int_member "generation" value in
                 let* status =
                   message_status_of_string (string_member "status" value)
                 in
                 let* action =
                   action_of_yojson (Yojson.Safe.Util.member "action" value)
                 in
                 let msg_id = Message_id.of_string key in
                 let message =
                   Orchestrator.
                     {
                       message_id = msg_id;
                       patch_id;
                       generation;
                       action;
                       payload_hash = string_member "payload_hash" value;
                       status;
                     }
                 in
                 match Map.add acc ~key:msg_id ~data:message with
                 | `Ok acc -> Ok acc
                 | `Duplicate ->
                     Error
                       (Printf.sprintf "duplicate outbox message_id: %s" key))
        in
        let* outbox = outbox in
        let graph_pids =
          Graph.all_patch_ids graph |> Set.of_list (module Patch_id)
        in
        let agent_pids = Map.keys agents_map |> Set.of_list (module Patch_id) in
        let missing_agent_pids = Set.diff graph_pids agent_pids in
        if not (Set.is_empty missing_agent_pids) then
          Error "snapshot missing agent state for one or more gameplan patches"
        else
          let graph =
            Set.fold
              (Set.diff agent_pids graph_pids)
              ~init:graph ~f:Graph.add_patch
          in
          Ok
            (Orchestrator.restore ~graph ~agents:agents_map ~outbox ~main_branch))
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Printf.sprintf "malformed orchestrator: %s" msg)
  | Invalid_argument msg ->
      Error (Printf.sprintf "malformed orchestrator: %s" msg)

(* ---------- Snapshot ---------- *)

let transcripts_to_yojson (t : (Patch_id.t, string) Hashtbl.t) =
  `Assoc
    (Hashtbl.fold t ~init:[] ~f:(fun ~key ~data acc ->
         (Patch_id.to_string key, `String data) :: acc))

let transcripts_of_yojson json =
  let t = Hashtbl.create (module Patch_id) in
  (match json with
  | `Assoc fields ->
      List.iter fields ~f:(fun (key, value) ->
          match Yojson.Safe.Util.to_string_option value with
          | Some s -> Hashtbl.set t ~key:(Patch_id.of_string key) ~data:s
          | None -> ())
  | _ -> ());
  t

let snapshot_to_yojson (snap : Runtime.snapshot) =
  `Assoc
    [
      ("version", `Int 1);
      ("orchestrator", orchestrator_to_yojson snap.orchestrator);
      ("activity_log", activity_log_to_yojson snap.activity_log);
      ("gameplan", Gameplan.yojson_of_t snap.gameplan);
      ("transcripts", transcripts_to_yojson snap.transcripts);
    ]

let snapshot_of_yojson json =
  try
    let version = int_member "version" json in
    if version <> 1 then
      Error (Printf.sprintf "unsupported version: %d" version)
    else
      Result.bind
        (try_of_yojson Gameplan.t_of_yojson
           (Yojson.Safe.Util.member "gameplan" json))
        ~f:(fun gameplan ->
          Result.bind
            (orchestrator_of_yojson ~gameplan
               (Yojson.Safe.Util.member "orchestrator" json))
            ~f:(fun orchestrator ->
              Result.map
                (activity_log_of_yojson
                   (Yojson.Safe.Util.member "activity_log" json))
                ~f:(fun activity_log ->
                  let transcripts =
                    match Yojson.Safe.Util.member "transcripts" json with
                    | `Null -> Hashtbl.create (module Patch_id)
                    | j -> transcripts_of_yojson j
                  in
                  { Runtime.orchestrator; activity_log; gameplan; transcripts })))
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Printf.sprintf "malformed snapshot: %s" msg)
  | Invalid_argument msg -> Error (Printf.sprintf "malformed snapshot: %s" msg)

(* ---------- File I/O ---------- *)

let save ~path (snap : Runtime.snapshot) =
  let dir = Stdlib.Filename.dirname path in
  let base = Stdlib.Filename.basename path in
  let tmp_path = Stdlib.Filename.temp_file ~temp_dir:dir (base ^ ".") ".tmp" in
  try
    let json = snapshot_to_yojson snap in
    let content = Yojson.Safe.pretty_to_string json in
    let oc = Stdlib.open_out_bin tmp_path in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_out oc)
      (fun () ->
        Stdlib.output_string oc content;
        Stdlib.flush oc);
    Stdlib.Sys.rename tmp_path path;
    Ok ()
  with exn ->
    (try Stdlib.Sys.remove tmp_path with _ -> ());
    Error (Stdlib.Printexc.to_string exn)

let load ~path =
  try
    let ic = Stdlib.open_in path in
    let content =
      Stdlib.Fun.protect
        ~finally:(fun () -> Stdlib.close_in_noerr ic)
        (fun () -> Stdlib.In_channel.input_all ic)
    in
    let json = Yojson.Safe.from_string content in
    let result = snapshot_of_yojson json in
    result
  with exn -> Error (Stdlib.Printexc.to_string exn)
