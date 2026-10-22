(* @archlint.module state
   @archlint.domain worker-checkpoint *)

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
          when Option.is_none operation && Int64.equal version 0L
               && Option.is_none terminal
               && Option.is_none attention_id
               || Option.is_some operation ->
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
            then Ok agent
            else Error "capsule patch identity mismatch")
    | _ -> Error "capsule agent missing required fields"

let version t = t.version
let workspace_id t = t.workspace_id
let context t = t.context
let attention_id t = t.attention_id
let latest_operation t = t.operation
let terminal t = t.terminal

let advance t ~version ~operation ~guidance ~workspace_id ~agent ~terminal =
  let attention_id =
    match terminal with
    | Some (Worker_protocol.Attention, _, receipt) -> receipt
    | Some
        ( ( Worker_protocol.Completed_ok | Completed_no_commits | Failed
          | Cancelled | Retryable | Merged_observed ),
          _,
          _ ) ->
        None
    | None -> t.attention_id
  in
  {
    t with
    version;
    operation = Some operation;
    guidance;
    workspace_id = Option.value workspace_id ~default:t.workspace_id;
    agent = Persistence.patch_agent_to_yojson agent;
    terminal;
    attention_id;
  }
