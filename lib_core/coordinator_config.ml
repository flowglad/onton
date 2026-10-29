(* @archlint.module core
   @archlint.domain coordinator *)
open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

[@@@warning "-4"]

type t = {
  version : int;
  capacity : int;
  repo_root : string;
  main_branch : string;
  worker_executable : string;
  backend : string;
  execution_mode : string;
}
[@@deriving yojson]

let decode bytes =
  try
    if String.length bytes > 16384 then
      Error "coordinator configuration exceeds 16 KiB"
    else
      let json = Yojson.Safe.from_string bytes in
      match json with
      | `Assoc fields
        when List.length fields = 7
             && List.length
                  (List.dedup_and_sort (List.map fields ~f:fst)
                     ~compare:String.compare)
                = 7 ->
          let t = t_of_yojson json in
          let safe s =
            (not (String.is_empty s))
            && String.length s <= 4096
            && not
                 (String.exists s ~f:(fun c ->
                      Char.equal c '\000' || Char.equal c '\n'
                      || Char.equal c '\r'))
          in
          if
            t.version <> 3 || t.capacity <= 0 || t.capacity > 256
            || (not
                  (List.for_all
                     [ t.repo_root; t.main_branch; t.worker_executable ]
                     ~f:safe))
            || (not
                  (List.mem [ "codex"; "claude" ] t.backend ~equal:String.equal))
            || not
                 (List.mem [ "mainline"; "feature" ] t.execution_mode
                    ~equal:String.equal)
          then Error "invalid coordinator configuration values"
          else Ok t
      | _ -> Error "expected the exact current coordinator configuration schema"
  with _ -> Error "invalid coordinator configuration JSON"

let work_json = function
  | Worker_protocol_v3.Reconcile -> `Assoc [ ("type", `String "reconcile") ]
  | Integrate_revision { contributor; revision } ->
      `Assoc
        [
          ("type", `String "integrate");
          ("contributor", `String contributor);
          ("revision", `String revision);
        ]
  | Session (phase, kind, spec) ->
      `Assoc
        [
          ("type", `String "session");
          ( "phase",
            `String
              (match phase with
              | Worker_protocol.Start -> "start"
              | Respond -> "respond") );
          ( "kind",
            match kind with
            | None -> `Null
            | Some k -> `String (Types.Operation_kind.to_label k) );
          ("spec", `String spec);
        ]

let exact_fields names fields =
  List.length fields = List.length names
  && List.length
       (List.dedup_and_sort (List.map fields ~f:fst) ~compare:String.compare)
     = List.length names
  && List.for_all names ~f:(fun name ->
      List.count fields ~f:(fun (key, _) -> String.equal key name) = 1)

let work bytes =
  try
    if String.length bytes > 262144 then Error "work exceeds bound"
    else
      match Yojson.Safe.from_string bytes with
      | `Assoc [ ("type", `String "reconcile") ] ->
          Ok Worker_protocol_v3.Reconcile
      | `Assoc fields -> (
          let field k = List.Assoc.find fields k ~equal:String.equal in
          match (field "type", field "phase", field "kind", field "spec") with
          | Some (`String "integrate"), _, _, _
            when exact_fields [ "type"; "contributor"; "revision" ] fields -> (
              match (field "contributor", field "revision") with
              | Some (`String contributor), Some (`String revision)
                when String.length contributor > 0
                     && String.length contributor <= 256
                     && not (String.contains contributor '\000') -> (
                  match Branch_reconcile.Commit.make revision with
                  | Some _ ->
                      Ok
                        (Worker_protocol_v3.Integrate_revision
                           { contributor; revision })
                  | None -> Error "invalid revision")
              | _ -> Error "invalid integration")
          | ( Some (`String "session"),
              Some (`String phase),
              kind,
              Some (`String spec) )
            when exact_fields [ "type"; "phase"; "kind"; "spec" ] fields -> (
              let phase =
                match phase with
                | "start" -> Some Worker_protocol.Start
                | "respond" -> Some Respond
                | _ -> None
              in
              let kind =
                match kind with
                | Some `Null -> Some None
                | Some (`String s) ->
                    Option.map (Types.Operation_kind.of_label s) ~f:Option.some
                | _ -> None
              in
              match (phase, kind) with
              | Some p, Some k -> Ok (Worker_protocol_v3.Session (p, k, spec))
              | _ -> Error "invalid work")
          | _ -> Error "invalid work")
      | _ -> Error "invalid work"
  with _ -> Error "invalid work"

let body_delivered entries (agent : Patch_agent.t) =
  List.exists entries ~f:(fun (entry : Coordinator_journal.entry) ->
      match entry.phase with
      | Applied result -> (
          try
            let scope, patch, created =
              match Yojson.Safe.from_string entry.scope with
              | `List [ scope; `String patch; `String _; `String _; `Bool _; _ ]
                ->
                  (Some scope, patch, true)
              | `List [ scope; `String patch; `String action ]
                when String.is_prefix action ~prefix:"body:" ->
                  (Some scope, patch, false)
              | _ -> (None, "", false)
            in
            match scope with
            | None -> false
            | Some json ->
                let scope = Coordinator_observation.scope_of_yojson json in
                String.equal patch (Types.Patch_id.to_string agent.patch_id)
                && Int.equal scope.generation agent.generation
                && Option.equal String.equal agent.head_oid
                     (Some scope.head_revision)
                && Option.equal Types.Branch.equal agent.base_branch
                     (Some scope.base_branch)
                && Option.value_map (Patch_agent.pr_number agent) ~default:false
                     ~f:(fun pr ->
                       if created then
                         Option.equal Int.equal (Int.of_string_opt result)
                           (Some (Types.Pr_number.to_int pr))
                       else Types.Pr_number.equal pr scope.pr)
          with _ -> false)
      | Pending | Attention -> false)

let reconcile (view : Scheduler_decision.input) =
  let views =
    List.map view.agents ~f:(fun a ->
        Reconciler.
          {
            id = a.Patch_agent.patch_id;
            has_pr = Patch_agent.is_pr_present a;
            merged = a.merged;
            busy = a.busy;
            needs_intervention = Patch_agent.needs_intervention a;
            branch_blocked = a.branch_blocked;
            in_merge_queue = Option.is_some a.merge_queue_entry;
            queue = a.queue;
            base_branch = Option.value a.base_branch ~default:view.main;
            branch_rebased_onto = Patch_agent.branch_rebased_onto a;
            base_contains_merged_siblings = a.base_contains_merged_siblings;
            sibling_rebase_target = None;
          })
  in
  let actions =
    Reconciler.detect_notified_base_drift views
    @ Reconciler.detect_stale_bases view.graph views
        ~has_merged:(fun id ->
          List.exists view.agents ~f:(fun a ->
              Types.Patch_id.equal a.patch_id id && a.merged))
        ~branch_of:(fun id ->
          Option.value_map
            (List.find view.agents ~f:(fun a ->
                 Types.Patch_id.equal a.patch_id id))
            ~default:view.main
            ~f:(fun a -> a.branch))
        ~main:view.main
  in
  let agents =
    List.map view.agents ~f:(fun agent ->
        if
          List.exists actions ~f:(function
            | Reconciler.Enqueue_rebase id ->
                Types.Patch_id.equal id agent.patch_id
            | _ -> false)
        then Patch_agent.enqueue agent Types.Operation_kind.Rebase
        else agent)
  in
  { view with agents }

let work_of_decision ~spec = function
  | Scheduler_decision.Start _ ->
      Worker_protocol_v3.Session (Worker_protocol.Start, None, spec)
  | Respond (_, kind) -> Session (Worker_protocol.Respond, Some kind, spec)
  | Rebase _ | Reconcile_branch _ -> Reconcile
