(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)

open Base
open Types

type t = {
  run_id : string;
  patch_id : string;
  workspace_id : string;
  context : string;
  version : int64;
  operation : Worker_protocol.operation option;
  guidance : string;
  agent : Yojson.Safe.t;
  terminal : (Worker_protocol.result_status * string * string option) option;
  attention_id : string option;
}

let status_name = function
  | Worker_protocol.Completed_ok -> "completed_ok"
  | Completed_no_commits -> "completed_no_commits"
  | Failed -> "failed"
  | Cancelled -> "cancelled"
  | Retryable -> "retryable"
  | Attention -> "attention"
  | Merged_observed -> "merged_observed"

let status_of_name = function
  | "completed_ok" -> Some Worker_protocol.Completed_ok
  | "completed_no_commits" -> Some Completed_no_commits
  | "failed" -> Some Failed
  | "cancelled" -> Some Cancelled
  | "retryable" -> Some Retryable
  | "attention" -> Some Attention
  | "merged_observed" -> Some Merged_observed
  | _ -> None

let string x = `String x
let counter x = string (Int64.to_string x)
let option = function None -> `Null | Some x -> string x

let encode t =
  let operation =
    match t.operation with
    | None -> `Null
    | Some op ->
        `Assoc
          [
            ("operation_id", string op.operation_id);
            ("attempt_id", string op.attempt_id);
            ("assignment_seq", counter op.assignment_seq);
          ]
  in
  let terminal =
    match t.terminal with
    | None -> `Null
    | Some (status, diagnostics, renewal_id) ->
        `Assoc
          [
            ("status", string (status_name status));
            ("diagnostics", string diagnostics);
            ("renewal_id", option renewal_id);
          ]
  in
  `Assoc
    [
      ("version", `Int 1);
      ("run_id", string t.run_id);
      ("patch_id", string t.patch_id);
      ("workspace_id", string t.workspace_id);
      ("context", string t.context);
      ("state_version", counter t.version);
      ("operation", operation);
      ("guidance", string t.guidance);
      ("agent", t.agent);
      ("terminal", terminal);
      ("attention_id", option t.attention_id);
    ]

let to_string t = Yojson.Safe.to_string (encode t)

let fields expected = function
  | `Assoc values
    when List.length values = List.length expected
         && List.for_all expected ~f:(fun name ->
             Int.equal 1
               (List.count values ~f:(fun (key, _) -> String.equal key name)))
    ->
      Some values
  | _ -> None

let get values name = List.Assoc.find values name ~equal:String.equal

let text values name =
  match get values name with
  | Some (`String x)
    when String.length x <= 256 * 1024 && not (String.contains x '\000') ->
      Some x
  | _ -> None

let id values name =
  match text values name with
  | Some x when String.length x > 0 && String.length x <= 256 -> Some x
  | _ -> None

let optional_id values name =
  match get values name with
  | Some `Null -> Some None
  | Some (`String x)
    when String.length x > 0
         && String.length x <= 256
         && not (String.contains x '\000') ->
      Some (Some x)
  | _ -> None

let read_counter = function
  | `String x -> (
      match Int64.of_string_opt x with
      | Some n when Int64.(n >= 0L) && String.equal x (Int64.to_string n) ->
          Some n
      | _ -> None)
  | _ -> None

let rec unique_keys depth (json : Yojson.Safe.t) =
  match json with
  | _ when depth > 64 -> false
  | `Assoc values ->
      List.for_all values ~f:(fun (name, value) ->
          Int.equal 1
            (List.count values ~f:(fun (key, _) -> String.equal key name))
          && unique_keys (depth + 1) value)
  | `List values -> List.for_all values ~f:(unique_keys (depth + 1))
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ -> true

(* The general snapshot reader accepts older schemas and supplies defaults.
   A worker capsule is a current-version authority record, so every field the
   current writer emits must be present before invoking that reader. *)
let agent_fields =
  [
    "branch_reconcile";
    "session_completion";
    "patch_id";
    "branch";
    "pr_status";
    "complexity";
    "pr_number";
    "has_session";
    "busy";
    "merged";
    "queue";
    "satisfies";
    "changed";
    "base_branch";
    "notified_base_branch";
    "ci_failure_count";
    "max_ci_failures";
    "session_fallback";
    "wontdo_reason";
    "human_messages";
    "inflight_human_messages";
    "ci_checks";
    "merge_ready";
    "merge_queue_required";
    "merge_queue_entry";
    "merge_queue_ejection";
    "head_oid";
    "review_decision";
    "unresolved_comment_count";
    "mergeability_unknown";
    "scheduler_recovery";
    "scheduler_repair";
    "native_stack";
    "native_stack_absent_polls";
    "is_draft";
    "pr_body_delivered";
    "pr_body_refresh_pending";
    "pr_body_refresh_version";
    "pr_body_artifact_miss_count";
    "review_unresolved_cycle_count";
    "start_attempts_without_pr";
    "no_commits_push_count";
    "context_exhaustion_count";
    "merge_commit_sha";
    "base_contains_merged_siblings";
    "checks_passing";
    "current_op";
    "current_op_state";
    "current_message_id";
    "generation";
    "worktree_path";
    "branch_blocked";
    "intervention_reason";
    "llm_session_id";
    "automerge_enabled";
    "automerge_deadline";
    "review_requested_for_oid";
    "review_request_inflight";
    "automerge_failure_count";
    "delivered_ci_run_ids";
  ]

let complete_agent = function
  | `Assoc _ as json -> Option.is_some (fields agent_fields json)
  | _ -> false

let decode json =
  let invalid = Error "invalid patch-execution capsule" in
  if not (unique_keys 0 json) then invalid
  else
    match
      fields
        [
          "version";
          "run_id";
          "patch_id";
          "workspace_id";
          "context";
          "state_version";
          "operation";
          "guidance";
          "agent";
          "terminal";
          "attention_id";
        ]
        json
    with
    | None -> invalid
    | Some values -> (
        let operation =
          match get values "operation" with
          | Some `Null -> Some None
          | Some json -> (
              match
                fields [ "operation_id"; "attempt_id"; "assignment_seq" ] json
              with
              | None -> None
              | Some op -> (
                  match
                    ( id op "operation_id",
                      id op "attempt_id",
                      Option.bind (get op "assignment_seq") ~f:read_counter )
                  with
                  | Some operation_id, Some attempt_id, Some assignment_seq
                    when Int64.(assignment_seq > 0L) ->
                      Some
                        (Some
                           Worker_protocol.
                             { operation_id; attempt_id; assignment_seq })
                  | _ -> None))
          | None -> None
        in
        let terminal =
          match get values "terminal" with
          | Some `Null -> Some None
          | Some json -> (
              match fields [ "status"; "diagnostics"; "renewal_id" ] json with
              | None -> None
              | Some term -> (
                  match
                    ( Option.bind (text term "status") ~f:status_of_name,
                      text term "diagnostics",
                      optional_id term "renewal_id" )
                  with
                  | Some status, Some diagnostics, Some renewal_id
                    when Bool.equal
                           (Poly.equal status Worker_protocol.Attention)
                           (Option.is_some renewal_id) ->
                      Some (Some (status, diagnostics, renewal_id))
                  | _ -> None))
          | None -> None
        in
        match
          ( get values "version",
            id values "run_id",
            id values "patch_id",
            id values "workspace_id",
            text values "context",
            Option.bind (get values "state_version") ~f:read_counter,
            operation,
            text values "guidance",
            get values "agent",
            terminal,
            optional_id values "attention_id" )
        with
        | ( Some (`Int 1),
            Some run_id,
            Some patch_id,
            Some workspace_id,
            Some context,
            Some version,
            Some operation,
            Some guidance,
            Some (`Assoc _ as agent),
            Some terminal,
            Some attention_id )
          when complete_agent agent
               && (Option.is_none operation && Int64.equal version 0L
                   && Option.is_none terminal
                   && Option.is_none attention_id
                  || Option.is_some operation
                     && Int64.(version > 0L)
                     && Option.equal String.equal attention_id
                          (match terminal with
                          | Some (Worker_protocol.Attention, _, receipt) ->
                              receipt
                          | None -> None
                          | Some
                              ( ( Worker_protocol.Completed_ok
                                | Completed_no_commits | Failed | Cancelled
                                | Retryable | Merged_observed ),
                                _,
                                _ ) ->
                              None)) ->
            Ok
              {
                run_id;
                patch_id;
                workspace_id;
                context;
                version;
                operation;
                guidance;
                agent;
                terminal;
                attention_id;
              }
        | _ -> invalid)

let initial ~run_id ~patch_id ~workspace_id ~context ~agent =
  {
    run_id;
    patch_id = Patch_id.to_string patch_id;
    workspace_id;
    context;
    version = 0L;
    operation = None;
    guidance = "";
    agent = Persistence.patch_agent_to_yojson agent;
    terminal = None;
    attention_id = None;
  }

let validate_binding t ~run_id ~patch_id ~context ~state_version ~attention_id
    ~gameplan ~main_branch =
  if
    not
      (String.equal t.run_id run_id
      && String.equal t.patch_id (Patch_id.to_string patch_id)
      && String.equal t.context context
      && Int64.equal t.version state_version
      && Option.equal String.equal t.attention_id attention_id)
  then Error "capsule binding mismatch"
  else if
    not
      (List.exists gameplan.Gameplan.patches ~f:(fun patch ->
           Patch_id.equal patch.Patch.id patch_id))
  then Error "assigned patch absent from gameplan"
  else
    let required =
      [
        "patch_id";
        "branch";
        "branch_reconcile";
        "session_completion";
        "session_fallback";
        "llm_session_id";
      ]
    in
    match t.agent with
    | `Assoc values
      when List.for_all required ~f:(fun name ->
               Int.equal 1
                 (List.count values ~f:(fun (key, _) -> String.equal key name)))
      ->
        Result.bind
          (Persistence.patch_agent_of_yojson ~snapshot_version:2 ~main_branch
             ~gameplan t.agent) ~f:(fun agent ->
            if
              Patch_id.equal agent.patch_id patch_id
              && Branch.equal agent.branch
                   (Gameplan.branch_of_id gameplan patch_id)
              && Yojson.Safe.equal
                   (Persistence.patch_agent_to_yojson agent)
                   t.agent
            then Ok agent
            else
              Error "capsule agent is noncanonical or has wrong patch identity")
    | _ -> Error "capsule agent missing required fields"

let version t = t.version
let workspace_id t = t.workspace_id
let context t = t.context
let attention_id t = t.attention_id
let latest_operation t = t.operation
let terminal t = t.terminal

let advance t ~operation ~guidance ~workspace_id ~agent ~terminal =
  let attention_id =
    match terminal with
    | Some (Worker_protocol.Attention, _, receipt) -> receipt
    | Some
        ( ( Worker_protocol.Completed_ok | Completed_no_commits | Failed
          | Cancelled | Retryable | Merged_observed ),
          _,
          _ ) ->
        None
    | None ->
        if Poly.equal t.operation (Some operation) then t.attention_id else None
  in
  if Int64.equal t.version Int64.max_value then
    Error "capsule state version exhausted"
  else
    Ok
      {
        t with
        version = Int64.succ t.version;
        operation = Some operation;
        guidance;
        workspace_id = Option.value workspace_id ~default:t.workspace_id;
        agent = Persistence.patch_agent_to_yojson agent;
        terminal;
        attention_id;
      }
