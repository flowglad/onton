open Base
open Types

(* ---------- helpers ---------- *)

let string_member key json = Yojson.Safe.Util.(member key json |> to_string)

let string_member_opt key json =
  Yojson.Safe.Util.(member key json |> to_string_option)

let int_member key json = Yojson.Safe.Util.(member key json |> to_int)
let bool_member key json = Yojson.Safe.Util.(member key json |> to_bool)

let bool_member_opt key json =
  match Yojson.Safe.Util.member key json with
  | `Null -> None
  | v -> Some (Yojson.Safe.Util.to_bool v)

let float_member key json = Yojson.Safe.Util.(member key json |> to_float)
let list_member key json = Yojson.Safe.Util.(member key json |> to_list)

let int_member_opt key json =
  match Yojson.Safe.Util.member key json with
  | `Null -> None
  | v -> Some (Yojson.Safe.Util.to_int v)

let nullable_string = function None -> `Null | Some s -> `String s
let nullable_int = function None -> `Null | Some n -> `Int n

let result_all xs =
  List.fold_right xs ~init:(Ok []) ~f:(fun x acc ->
      Result.bind acc ~f:(fun tl -> Result.map x ~f:(fun hd -> hd :: tl)))

(* ---------- Operation_kind ---------- *)

let operation_kind_to_yojson = function
  | Operation_kind.Rebase -> `String "Rebase"
  | Human -> `String "Human"
  | Merge_conflict -> `String "Merge_conflict"
  | Ci -> `String "Ci"
  | Review_comments -> `String "Review_comments"

let operation_kind_of_yojson json =
  match Yojson.Safe.Util.to_string json with
  | "Rebase" -> Ok Operation_kind.Rebase
  | "Human" -> Ok Human
  | "Merge_conflict" -> Ok Merge_conflict
  | "Ci" -> Ok Ci
  | "Review_comments" -> Ok Review_comments
  | s -> Error (Printf.sprintf "unknown operation kind: %s" s)

(* ---------- display_status ---------- *)

let display_status_to_yojson = function
  | Tui.Merged -> `String "Merged"
  | Needs_help -> `String "Needs_help"
  | Approved_idle -> `String "Approved_idle"
  | Approved_running -> `String "Approved_running"
  | Fixing_ci -> `String "Fixing_ci"
  | Addressing_review -> `String "Addressing_review"
  | Resolving_conflict -> `String "Resolving_conflict"
  | Responding_to_human -> `String "Responding_to_human"
  | Rebasing -> `String "Rebasing"
  | Starting -> `String "Starting"
  | Ci_queued -> `String "Ci_queued"
  | Review_queued -> `String "Review_queued"
  | Awaiting_ci -> `String "Awaiting_ci"
  | Awaiting_review -> `String "Awaiting_review"
  | Pending -> `String "Pending"

let display_status_of_yojson json =
  match Yojson.Safe.Util.to_string json with
  | "Merged" -> Ok Tui.Merged
  | "Needs_help" -> Ok Needs_help
  | "Approved_idle" -> Ok Approved_idle
  | "Approved_running" -> Ok Approved_running
  | "Fixing_ci" -> Ok Fixing_ci
  | "Addressing_review" -> Ok Addressing_review
  | "Resolving_conflict" -> Ok Resolving_conflict
  | "Responding_to_human" -> Ok Responding_to_human
  | "Rebasing" -> Ok Rebasing
  | "Starting" -> Ok Starting
  | "Ci_queued" -> Ok Ci_queued
  | "Review_queued" -> Ok Review_queued
  | "Awaiting_ci" -> Ok Awaiting_ci
  | "Awaiting_review" -> Ok Awaiting_review
  | "Pending" -> Ok Pending
  | s -> Error (Printf.sprintf "unknown display status: %s" s)

(* ---------- Comment ---------- *)

let comment_to_yojson (c : Comment.t) =
  `Assoc
    [
      ("id", `Int (Comment_id.to_int c.id));
      ("body", `String c.body);
      ("path", nullable_string c.path);
      ("line", nullable_int c.line);
    ]

let comment_of_yojson json =
  try
    let open Yojson.Safe.Util in
    Ok
      {
        Comment.id =
          member "id" json |> to_int_option |> Option.value ~default:0
          |> Comment_id.of_int;
        body = member "body" json |> to_string;
        path = member "path" json |> to_string_option;
        line = member "line" json |> to_int_option;
      }
  with Yojson.Safe.Util.Type_error (msg, _) ->
    Error (Printf.sprintf "malformed comment: %s" msg)

(* ---------- pending_comment ---------- *)

let pending_comment_to_yojson (pc : Patch_agent.pending_comment) =
  `Assoc
    [ ("comment", comment_to_yojson pc.comment); ("valid", `Bool pc.valid) ]

let pending_comment_of_yojson json =
  try
    Result.map
      (comment_of_yojson (Yojson.Safe.Util.member "comment" json))
      ~f:(fun comment ->
        Patch_agent.restore_pending_comment ~comment
          ~valid:(bool_member "valid" json))
  with Yojson.Safe.Util.Type_error (msg, _) ->
    Error (Printf.sprintf "malformed pending_comment: %s" msg)

(* ---------- Patch_agent ---------- *)

let patch_agent_to_yojson (a : Patch_agent.t) =
  `Assoc
    [
      ("patch_id", `String (Patch_id.to_string a.patch_id));
      ("has_pr", `Bool a.has_pr);
      ( "pr_number",
        match a.pr_number with
        | None -> `Null
        | Some n -> `Int (Pr_number.to_int n) );
      ("has_session", `Bool a.has_session);
      ("busy", `Bool a.busy);
      ("merged", `Bool a.merged);
      ("needs_intervention", `Bool a.needs_intervention);
      ("queue", `List (List.map a.queue ~f:operation_kind_to_yojson));
      ("satisfies", `Bool a.satisfies);
      ("changed", `Bool a.changed);
      ("has_conflict", `Bool a.has_conflict);
      ( "base_branch",
        match a.base_branch with
        | None -> `Null
        | Some b -> `String (Branch.to_string b) );
      ("ci_failure_count", `Int a.ci_failure_count);
      ("session_failed", `Bool a.session_failed);
      ( "pending_comments",
        `List (List.map a.pending_comments ~f:pending_comment_to_yojson) );
      ( "last_session_id",
        match a.last_session_id with
        | None -> `Null
        | Some id -> `String (Session_id.to_string id) );
      ("tried_fresh", `Bool a.tried_fresh);
    ]

let patch_agent_of_yojson json =
  Result.bind
    (result_all
       (List.map (list_member "queue" json) ~f:operation_kind_of_yojson))
    ~f:(fun queue ->
      Result.map
        (result_all
           (List.map
              (list_member "pending_comments" json)
              ~f:pending_comment_of_yojson))
        ~f:(fun pending_comments ->
          Patch_agent.restore
            ~patch_id:(Patch_id.of_string (string_member "patch_id" json))
            ~has_pr:(bool_member "has_pr" json)
            ~pr_number:
              (int_member_opt "pr_number" json |> Option.map ~f:Pr_number.of_int)
            ~has_session:(bool_member "has_session" json)
            ~busy:(bool_member "busy" json)
            ~merged:(bool_member "merged" json)
            ~needs_intervention:(bool_member "needs_intervention" json)
            ~queue
            ~satisfies:(bool_member "satisfies" json)
            ~changed:(bool_member "changed" json)
            ~has_conflict:(bool_member "has_conflict" json)
            ~base_branch:
              (string_member_opt "base_branch" json
              |> Option.map ~f:Branch.of_string)
            ~ci_failure_count:(int_member "ci_failure_count" json)
            ~session_failed:(bool_member "session_failed" json)
            ~pending_comments
            ~last_session_id:
              (string_member_opt "last_session_id" json
              |> Option.map ~f:Session_id.of_string)
            ~tried_fresh:
              (bool_member_opt "tried_fresh" json |> Option.value ~default:false)))

(* ---------- Transition_entry ---------- *)

let transition_entry_to_yojson (e : Activity_log.Transition_entry.t) =
  `Assoc
    [
      ("timestamp", `Float e.timestamp);
      ("patch_id", `String (Patch_id.to_string e.patch_id));
      ("from_status", display_status_to_yojson e.from_status);
      ("to_status", display_status_to_yojson e.to_status);
      ("action", `String e.action);
    ]

let transition_entry_of_yojson json =
  Result.bind
    (display_status_of_yojson (Yojson.Safe.Util.member "from_status" json))
    ~f:(fun from_status ->
      Result.map
        (display_status_of_yojson (Yojson.Safe.Util.member "to_status" json))
        ~f:(fun to_status ->
          Activity_log.Transition_entry.create
            ~timestamp:(float_member "timestamp" json)
            ~patch_id:(Patch_id.of_string (string_member "patch_id" json))
            ~from_status ~to_status
            ~action:(string_member "action" json)))

(* ---------- Event ---------- *)

let event_to_yojson (e : Activity_log.Event.t) =
  `Assoc
    [
      ("timestamp", `Float e.timestamp);
      ( "patch_id",
        match e.patch_id with
        | None -> `Null
        | Some pid -> `String (Patch_id.to_string pid) );
      ("message", `String e.message);
    ]

let event_of_yojson json =
  Ok
    (Activity_log.Event.create
       ~timestamp:(float_member "timestamp" json)
       ?patch_id:
         (string_member_opt "patch_id" json |> Option.map ~f:Patch_id.of_string)
       (string_member "message" json))

(* ---------- Activity_log ---------- *)

let activity_log_to_yojson (log : Activity_log.t) =
  let transitions = Activity_log.recent_transitions log ~limit:Int.max_value in
  let events = Activity_log.recent_events log ~limit:Int.max_value in
  `Assoc
    [
      ("transitions", `List (List.map transitions ~f:transition_entry_to_yojson));
      ("events", `List (List.map events ~f:event_to_yojson));
    ]

let activity_log_of_yojson json =
  Result.bind
    (result_all
       (List.map (list_member "transitions" json) ~f:transition_entry_of_yojson))
    ~f:(fun transitions ->
      Result.map
        (result_all (List.map (list_member "events" json) ~f:event_of_yojson))
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
  `Assoc
    [
      ("main_branch", `String (Branch.to_string (Orchestrator.main_branch o)));
      ("agents", `Assoc agents);
    ]

let orchestrator_of_yojson ~gameplan json =
  try
    let graph = Graph.of_patches gameplan.Gameplan.patches in
    let main_branch = Branch.of_string (string_member "main_branch" json) in
    Result.bind
      (result_all
         (Yojson.Safe.Util.member "agents" json
         |> Yojson.Safe.Util.to_assoc
         |> List.map ~f:(fun (key, value) ->
             Result.bind (patch_agent_of_yojson value) ~f:(fun agent ->
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
        let graph_pids =
          Graph.all_patch_ids graph |> Set.of_list (module Patch_id)
        in
        let agent_pids = Map.keys agents_map |> Set.of_list (module Patch_id) in
        if not (Set.equal graph_pids agent_pids) then
          Error
            "agent/gameplan mismatch: persisted patch IDs differ from gameplan"
        else Ok (Orchestrator.restore ~graph ~agents:agents_map ~main_branch))
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Printf.sprintf "malformed orchestrator: %s" msg)
  | Invalid_argument msg ->
      Error (Printf.sprintf "malformed orchestrator: %s" msg)

(* ---------- Gameplan ---------- *)

let patch_to_yojson (p : Patch.t) =
  `Assoc
    [
      ("id", `String (Patch_id.to_string p.id));
      ("title", `String p.title);
      ("branch", `String (Branch.to_string p.branch));
      ( "dependencies",
        `List
          (List.map p.dependencies ~f:(fun d -> `String (Patch_id.to_string d)))
      );
    ]

let gameplan_to_yojson (g : Gameplan.t) =
  `Assoc
    [
      ("project_name", `String g.project_name);
      ("problem_statement", `String g.problem_statement);
      ("solution_summary", `String g.solution_summary);
      ("patches", `List (List.map g.patches ~f:patch_to_yojson));
    ]

(* ---------- Snapshot ---------- *)

let snapshot_to_yojson (snap : Runtime.snapshot) =
  `Assoc
    [
      ("version", `Int 1);
      ("orchestrator", orchestrator_to_yojson snap.orchestrator);
      ("activity_log", activity_log_to_yojson snap.activity_log);
      ("gameplan", gameplan_to_yojson snap.gameplan);
    ]

let snapshot_of_yojson ~gameplan json =
  try
    let version = int_member "version" json in
    if version <> 1 then
      Error (Printf.sprintf "unsupported version: %d" version)
    else
      Result.bind
        (orchestrator_of_yojson ~gameplan
           (Yojson.Safe.Util.member "orchestrator" json))
        ~f:(fun orchestrator ->
          Result.map
            (activity_log_of_yojson
               (Yojson.Safe.Util.member "activity_log" json))
            ~f:(fun activity_log ->
              { Runtime.orchestrator; activity_log; gameplan }))
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

let load ~path ~gameplan =
  try
    let ic = Stdlib.open_in path in
    let content =
      Stdlib.Fun.protect
        ~finally:(fun () -> Stdlib.close_in_noerr ic)
        (fun () -> Stdlib.In_channel.input_all ic)
    in
    let json = Yojson.Safe.from_string content in
    snapshot_of_yojson ~gameplan json
  with exn -> Error (Stdlib.Printexc.to_string exn)
