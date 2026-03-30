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

let list_member key json = Yojson.Safe.Util.(member key json |> to_list)

let int_member_opt key json =
  match Yojson.Safe.Util.member key json with
  | `Null -> None
  | v -> Some (Yojson.Safe.Util.to_int v)

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

let session_fallback_of_legacy ~session_failed ~tried_fresh =
  match (session_failed, tried_fresh) with
  | true, false -> Patch_agent.Tried_fresh
  | true, true -> Patch_agent.Given_up
  | false, true -> Patch_agent.Tried_fresh
  | false, false -> Patch_agent.Fresh_available

let patch_agent_to_yojson (a : Patch_agent.t) =
  `Assoc
    [
      ("patch_id", Patch_id.yojson_of_t a.patch_id);
      ("has_pr", `Bool a.has_pr);
      ( "pr_number",
        match a.pr_number with
        | None -> `Null
        | Some n -> Pr_number.yojson_of_t n );
      ("has_session", `Bool a.has_session);
      ("busy", `Bool a.busy);
      ("merged", `Bool a.merged);
      ("needs_intervention", `Bool a.needs_intervention);
      ("queue", `List (List.map a.queue ~f:Operation_kind.yojson_of_t));
      ("satisfies", `Bool a.satisfies);
      ("changed", `Bool a.changed);
      ("has_conflict", `Bool a.has_conflict);
      ( "base_branch",
        match a.base_branch with
        | None -> `Null
        | Some b -> Branch.yojson_of_t b );
      ("ci_failure_count", `Int a.ci_failure_count);
      ("ci_fix_running", `Bool a.ci_fix_running);
      ( "session_fallback",
        Patch_agent.yojson_of_session_fallback a.session_fallback );
      ( "human_messages",
        `List (List.map a.human_messages ~f:(fun s -> `String s)) );
      ("ci_checks", `List (List.map a.ci_checks ~f:Ci_check.yojson_of_t));
      ("mergeable", `Bool a.mergeable);
      ("merge_ready", `Bool a.merge_ready);
      ("checks_passing", `Bool a.checks_passing);
      ("no_unresolved_comments", `Bool a.no_unresolved_comments);
      ( "worktree_path",
        match a.worktree_path with None -> `Null | Some p -> `String p );
      ( "head_branch",
        match a.head_branch with
        | None -> `Null
        | Some b -> Branch.yojson_of_t b );
      ("branch_blocked", `Bool a.branch_blocked);
    ]

let list_member_opt key json =
  match Yojson.Safe.Util.member key json with
  | `Null -> Ok None
  | `List _ as v -> Ok (Some (Yojson.Safe.Util.to_list v))
  | other ->
      Error
        (Printf.sprintf "%s: expected list, got %s" key
           (Yojson.Safe.to_string other))

let patch_agent_of_yojson json =
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
  let* session_fallback =
    match json with
    | `Assoc fields -> (
        match List.Assoc.find fields ~equal:String.equal "session_fallback" with
        | None ->
            (* backward compat: derive from legacy bools *)
            let session_failed =
              bool_member_opt "session_failed" json
              |> Option.value ~default:false
            in
            let tried_fresh =
              bool_member_opt "tried_fresh" json |> Option.value ~default:false
            in
            Ok (session_fallback_of_legacy ~session_failed ~tried_fresh)
        | Some v -> try_of_yojson Patch_agent.session_fallback_of_yojson v)
    | _ -> Error "patch_agent: expected JSON object"
  in
  let* ci_checks_raw = list_member_opt "ci_checks" json in
  let ci_checks_raw = Option.value ci_checks_raw ~default:[] in
  let* ci_checks =
    result_all
      (List.map ci_checks_raw ~f:(fun j -> try_of_yojson Ci_check.t_of_yojson j))
  in
  Ok
    (Patch_agent.restore
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
         (string_member_opt "base_branch" json |> Option.map ~f:Branch.of_string)
       ~ci_failure_count:(int_member "ci_failure_count" json)
       ~ci_fix_running:
         (bool_member_opt "ci_fix_running" json |> Option.value ~default:false)
       ~session_fallback ~human_messages ~ci_checks
       ~mergeable:
         (bool_member_opt "mergeable" json |> Option.value ~default:false)
       ~merge_ready:
         (bool_member_opt "merge_ready" json |> Option.value ~default:false)
       ~checks_passing:
         (bool_member_opt "checks_passing" json |> Option.value ~default:false)
       ~no_unresolved_comments:
         (bool_member_opt "no_unresolved_comments" json
         |> Option.value ~default:false)
       ~worktree_path:(string_member_opt "worktree_path" json)
       ~head_branch:
         (string_member_opt "head_branch" json |> Option.map ~f:Branch.of_string)
       ~branch_blocked:
         (bool_member_opt "branch_blocked" json |> Option.value ~default:false))

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
  `Assoc
    [
      ("main_branch", Branch.yojson_of_t (Orchestrator.main_branch o));
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
        (* Filter out agents that were marked removed in old snapshots.
           The removed field no longer exists — ad-hoc patches are now
           truly deleted. We detect legacy removed agents by checking the
           raw JSON. *)
        let legacy_removed =
          Yojson.Safe.Util.member "agents" json
          |> Yojson.Safe.Util.to_assoc
          |> List.filter_map ~f:(fun (key, value) ->
              if bool_member_opt "removed" value |> Option.value ~default:false
              then Some (Patch_id.of_string key)
              else None)
          |> Set.of_list (module Patch_id)
        in
        let agents =
          List.filter agents ~f:(fun (k, _) -> not (Set.mem legacy_removed k))
        in
        let agents_map =
          List.fold agents
            ~init:(Map.empty (module Patch_id))
            ~f:(fun acc (k, v) -> Map.set acc ~key:k ~data:v)
        in
        let graph_pids =
          Graph.all_patch_ids graph |> Set.of_list (module Patch_id)
        in
        let agent_pids = Map.keys agents_map |> Set.of_list (module Patch_id) in
        (* Gameplan patches must all be present in the snapshot. *)
        let missing_from_agents = Set.diff graph_pids agent_pids in
        if not (Set.is_empty missing_from_agents) then
          Error
            "agent/gameplan mismatch: persisted patch IDs differ from gameplan"
        else
          (* Ad-hoc agents (not in the gameplan) are added to the graph with no
             dependencies, mirroring Orchestrator.add_agent. *)
          let graph =
            Set.fold
              (Set.diff agent_pids graph_pids)
              ~init:graph ~f:Graph.add_patch
          in
          Ok (Orchestrator.restore ~graph ~agents:agents_map ~main_branch))
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
              let transcripts =
                match Yojson.Safe.Util.member "transcripts" json with
                | `Null -> Hashtbl.create (module Patch_id)
                | j -> transcripts_of_yojson j
              in
              { Runtime.orchestrator; activity_log; gameplan; transcripts }))
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
    let result = snapshot_of_yojson ~gameplan json in
    result
  with exn -> Error (Stdlib.Printexc.to_string exn)
