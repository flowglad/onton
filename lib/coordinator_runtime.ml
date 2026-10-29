(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)
open Base
open Types

[@@@warning "-4"]

module S = Coordinator_state
module V = Worker_protocol_v3
module P = Worker_protocol
module J = Coordinator_journal

let get = function Ok x -> x | Error why -> failwith why

let get_protocol = function
  | Ok x -> x
  | Error (P.Invalid why | P.Incompatible why) -> failwith why

let wire frame = get (V.encode_host frame)
let json bytes = Yojson.Safe.from_string bytes

let next_id () =
  let bytes = Bytes.create 16 in
  let ch = Stdlib.open_in_bin "/dev/urandom" in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in ch)
    (fun () -> Stdlib.really_input ch bytes 0 16);
  "coord-" ^ Stdlib.Digest.to_hex (Stdlib.Digest.bytes bytes)

let run (type e) ~system ~client ~store ~(config : Coordinator_config.t)
    ~gameplan ~(forge : (module Forge.S with type error = e)) ~control_path =
  let clock = Eio.Stdenv.clock system in
  let main = Branch.of_string config.main_branch in
  let graph = Graph.of_gameplan gameplan in
  let mode =
    if String.equal config.execution_mode "feature" then
      get (Execution_mode.infer_gameplan gameplan)
    else Execution_mode.mainline
  in
  let io =
    Branch_reconcile_executor.make_io
      ~process_mgr:(Eio.Stdenv.process_mgr system)
      ~clock ~path:config.repo_root
  in
  let now () = Int64.of_float (Eio.Time.now clock) in
  let attached = Hashtbl.create (module String) in
  let polled = Hashtbl.create (module String) in
  let forge_cursor = ref 0 in
  let diagnostics = Hashtbl.create (module String) in
  let report_blocked = ref (fun _ -> ()) in
  let stop_request ~resource_id =
    let run_id, coordinator_id, epoch, revision =
      Coordinator_store.authority store
    in
    Resource_protocol.make_request
      (Yojson.Safe.to_string
         (`Assoc
            [
              ("version", `String "resource-v1");
              ("op", `String "stop");
              ("run_id", `String run_id);
              ("coordinator_id", `String coordinator_id);
              ("owner_epoch", `String (Int64.to_string epoch));
              ("revision", `String (Int64.to_string revision));
              ("request_id", `String (next_id ()));
              ("resource_id", `String resource_id);
            ]))
  in
  let request op fields =
    let run_id, coordinator_id, epoch, revision =
      Coordinator_store.authority store
    in
    get
      (Resource_protocol.make_request
         (Yojson.Safe.to_string
            (`Assoc
               ([
                  ("version", `String "resource-v1");
                  ("op", `String op);
                  ("run_id", `String run_id);
                  ("coordinator_id", `String coordinator_id);
                  ("owner_epoch", `String (Int64.to_string epoch));
                  ("revision", `String (Int64.to_string revision));
                  ("request_id", `String (next_id ()));
                ]
               @ fields))))
  in
  let stopped_cleanly = ref false in
  let source envelope =
    let state = Coordinator_store.state envelope in
    let assignments = S.assignments state in
    let latest patch =
      List.find_map (S.capsules state) ~f:(fun c ->
          List.find assignments ~f:(fun a ->
              String.equal a.id c.assignment && Patch_id.equal a.patch patch)
          |> Option.map ~f:(fun _ -> c))
    in
    let capsule_agent patch =
      match latest patch with
      | None -> Ok None
      | Some c -> (
          try
            Result.bind
              (Worker_checkpoint.decode (json c.bytes))
              ~f:(fun cap ->
                Result.map
                  (Worker_checkpoint.validate_binding cap
                     ~run_id:(S.run_id state) ~patch_id:patch
                     ~context:(Worker_checkpoint.context cap)
                     ~state_version:c.version
                     ~attention_id:(Worker_checkpoint.attention_id cap)
                     ~gameplan ~main_branch:main)
                  ~f:Option.some)
          with _ -> Error "malformed persisted owner capsule")
    in
    let capsule_results =
      List.map gameplan.patches ~f:(fun p -> (p.id, capsule_agent p.id))
    in
    let corrupt_patch patch =
      List.exists capsule_results ~f:(fun (id, result) ->
          Patch_id.equal id patch && Result.is_error result)
    in
    let agents =
      List.map gameplan.patches ~f:(fun p ->
          match List.Assoc.find capsule_results p.id ~equal:Patch_id.equal with
          | Some (Ok (Some a)) -> a
          | Some (Ok None) | None -> Patch_agent.create ~branch:p.branch p.id
          | Some (Error _) ->
              Patch_agent.create ~branch:p.branch p.id
              |> Patch_agent.set_branch_blocked)
    in
    let capsule_attention =
      List.find_map assignments ~f:(fun assignment ->
          if not (corrupt_patch assignment.patch) then None
          else
            List.find (S.operations state) ~f:(fun (op : S.operation) ->
                String.equal op.id assignment.operation && not op.attention)
            |> Option.map ~f:(fun (op : S.operation) -> op.id))
    in
    let view =
      Coordinator_config.reconcile
        (S.scheduling_view state
           { Scheduler_decision.graph; mode; agents; main })
    in
    if
      Option.equal String.equal
        (Stdlib.Sys.getenv_opt "ONTON_COORDINATOR_TRACE_POLICY")
        (Some "1")
    then
      List.iter view.agents ~f:(fun a ->
          let id = Patch_id.to_string a.Patch_agent.patch_id in
          let row =
            Printf.sprintf
              "policy %s pr=%b body=%b pair=%b checks=%b draft=%b rebased=%b \
               unsettled=%b queue=%d generation=%d"
              id
              (Patch_agent.is_pr_present a)
              a.pr_body_delivered
              (Patch_agent.forge_revision_pair_confirmed a)
              a.checks_passing a.is_draft
              (Option.is_some (Patch_agent.branch_rebased_onto a))
              (Branch_reconcile.is_unsettled a.branch_reconcile)
              (List.length a.queue) a.generation
          in
          if
            not
              (Option.equal String.equal
                 (Hashtbl.find diagnostics id)
                 (Some row))
          then (
            Hashtbl.set diagnostics ~key:id ~data:row;
            Stdlib.prerr_endline row));
    let agent patch =
      List.find view.agents ~f:(fun a ->
          Patch_id.equal a.Patch_agent.patch_id patch)
    in
    let stream (a : S.assignment) =
      match Coordinator_store.stream envelope a.S.id with
      | Some s -> Some s
      | None ->
          List.find_map assignments ~f:(fun old ->
              if
                String.equal old.resource a.resource
                && old.cleanup && not old.retired
              then Coordinator_store.stream envelope old.id
              else None)
    in
    let context (op : S.operation) (a : Patch_agent.t) =
      let selected_base =
        Option.value
          (Scheduler_decision.selected_base view a.patch_id)
          ~default:main
      in
      let dependencies =
        List.map (Graph.deps graph a.patch_id) ~f:(fun id ->
            match agent id with
            | None -> (id, false, "")
            | Some dep ->
                ( id,
                  true,
                  Option.value
                    (if dep.merged then dep.merge_commit_sha else dep.head_oid)
                    ~default:"" ))
      in
      let input_version =
        List.fold (S.inputs state) ~init:0L ~f:(fun v (_, patch, n, _, _) ->
            if String.equal patch (Patch_id.to_string a.patch_id) then
              Int64.max v n
            else v)
      in
      let guidance_ids =
        S.inputs state
        |> List.filter_map ~f:(fun (id, p, _, _, used) ->
            if (not used) && String.equal p (Patch_id.to_string a.patch_id) then
              Some id
            else None)
      in
      S.execution_context state view
        {
          S.run_id = S.run_id state;
          config_id = S.config_id state;
          plan_id = S.plan_id state;
          message_id = op.S.id;
          generation = Int64.of_int a.generation;
          patch_id = a.patch_id;
          dependencies;
          selected_base;
          mode =
            (if Option.is_some (Execution_mode.root mode) then "feature"
             else "mainline");
          root = Execution_mode.is_root mode a.patch_id;
          evidence_id =
            Option.value
              (Scheduler_decision.evidence_id view a.patch_id)
              ~default:"";
          guidance_ids;
          input_version;
        }
    in
    let schedule (op : S.operation) =
      J.find (S.journal state) ("schedule:" ^ op.S.id)
      |> Option.bind ~f:(fun entry ->
          Result.ok (Coordinator_config.work entry.J.scope))
      |> fun work -> if op.maintenance then Some V.Reconcile else work
    in
    let provision (op : S.operation) (predecessor : S.assignment option) =
      let resource = next_id () in
      let attempt =
        match predecessor with
        | Some old when not old.S.cleanup -> old.request
        | _ -> next_id ()
      in
      let assignment : S.assignment =
        {
          id = next_id ();
          operation = op.S.id;
          patch = op.patch;
          resource;
          request = attempt;
          reserved = true;
          retired = false;
          result_committed = false;
          cleanup = false;
        }
      in
      let manifest =
        get
          (Resource_client.worker_manifest ~executable:config.worker_executable
             ~args:
               [
                 "--gameplan";
                 "inputs/gameplan.yaml";
                 "--repo-root";
                 "repo";
                 "--project-name";
                 gameplan.project_name;
                 "--main-branch";
                 config.main_branch;
                 "--backend";
                 config.backend;
                 "--ack-timeout";
                 "60";
               ]
             ~inputs:[ ("gameplan.yaml", S.plan_bytes state) ])
      in
      let fields =
        [
          ("chain_id", `String ("patch:" ^ Patch_id.to_string op.patch));
          ("patch_id", `String (Patch_id.to_string op.patch));
          ("resource_id", `String resource);
          ( "launch_digest",
            `String
              (Stdlib.Digest.to_hex
                 (Stdlib.Digest.string (Yojson.Safe.to_string manifest))) );
          ("manifest", manifest);
        ]
      in
      let req =
        match predecessor with
        | None -> request "launch" fields
        | Some old ->
            request "replace"
              (("predecessor_id", `String old.resource) :: fields)
      in
      let run_id, coordinator_id, owner_epoch, revision =
        Coordinator_store.authority store
      in
      Coordinator.Provision
        {
          request = req;
          assignment;
          connection =
            {
              run_id;
              coordinator_id;
              owner_epoch;
              revision;
              request_id = next_id ();
              resource_id = resource;
            };
          recovery =
            Option.bind predecessor ~f:(fun old ->
                Option.map (Coordinator_store.stream envelope old.id)
                  ~f:(fun _ -> (old.id, resource, resource)));
        }
    in
    let active =
      List.find_map assignments ~f:(fun assignment ->
          if
            (not assignment.reserved) || assignment.retired
            || corrupt_patch assignment.patch
            || List.mem (S.lost_resources state) assignment.resource
                 ~equal:String.equal
          then None
          else
            match
              ( List.find (S.operations state) ~f:(fun o ->
                    String.equal o.id assignment.operation),
                agent assignment.patch )
            with
            | Some op, Some a -> (
                match stream assignment with
                | None when Hashtbl.mem attached assignment.resource -> (
                    match context op a with
                    | Error _ -> None
                    | Ok context ->
                        let bind =
                          get_protocol
                            (P.decode
                               (Yojson.Safe.to_string
                                  (`Assoc
                                     [
                                       ("version", `Int 2);
                                       ("type", `String "bind");
                                       ("run_id", `String (S.run_id state));
                                       ( "patch_id",
                                         `String (Patch_id.to_string a.patch_id)
                                       );
                                       ("worker_id", `String assignment.resource);
                                       ( "workspace_id",
                                         `String assignment.resource );
                                       ( "context",
                                         `String gameplan.problem_statement );
                                       ("capsule", `String "{}");
                                       ("state_version", `String "0");
                                       ("attention_id", `Null);
                                       ( "admission",
                                         `Assoc [ ("mode", `String "fresh") ] );
                                     ])
                               ^ "\n"))
                        in
                        Some
                          (Coordinator.Command
                             (assignment.id, wire (V.Bind (bind, context)))))
                | None -> (
                    match
                      List.Assoc.find (S.resources state) assignment.resource
                        ~equal:String.equal
                    with
                    | None -> None
                    | Some bytes -> (
                        let req = get (Resource_protocol.make_request bytes) in
                        match
                          J.find (S.journal state)
                            (Coordinator_resource.journal_id assignment.resource)
                        with
                        | Some { phase = J.Pending; _ } ->
                            let run_id, coordinator_id, owner_epoch, revision =
                              Coordinator_store.authority store
                            in
                            Some
                              (Coordinator.Provision
                                 {
                                   request = req;
                                   assignment;
                                   connection =
                                     {
                                       run_id;
                                       coordinator_id;
                                       owner_epoch;
                                       revision;
                                       request_id = next_id ();
                                       resource_id = assignment.resource;
                                     };
                                   recovery = None;
                                 })
                        | _ -> None))
                | Some s
                  when Worker_stream.ready s
                       && not
                            (Option.value_map (Worker_stream.operation s)
                               ~default:false ~f:(fun o ->
                                 String.equal o.operation_id op.id
                                 && String.equal o.attempt_id assignment.request))
                  -> (
                    match (context op a, schedule op) with
                    | Ok context, Some work -> (
                        let input =
                          Coordinator_inputs.pending state a.patch_id
                            ~selected_base:(V.selected_base context)
                        in
                        match input with
                        | Ok (Some input)
                          when not
                                 (Option.value_map (Worker_stream.input s)
                                    ~default:false ~f:(fun old ->
                                      Int64.(old.version >= input.version))) ->
                            Some
                              (Coordinator.Command
                                 ( assignment.id,
                                   wire
                                     (V.Input
                                        ( {
                                            run_id = S.run_id state;
                                            patch_id =
                                              Patch_id.to_string a.patch_id;
                                          },
                                          input )) ))
                        | Error reason ->
                            !report_blocked reason;
                            Some
                              (Coordinator.Intent (S.Operation_attention op.id))
                        | Ok None | Ok (Some _) ->
                            let version =
                              match Worker_stream.capsule s with
                              | None | Some "{}" -> 0L
                              | Some bytes ->
                                  Worker_checkpoint.version
                                    (get
                                       (Worker_checkpoint.decode (json bytes)))
                            in
                            let sequence =
                              Option.value_map (Worker_stream.operation s)
                                ~default:1L ~f:(fun old ->
                                  if
                                    Int64.equal old.assignment_seq
                                      Int64.max_value
                                  then failwith "assignment sequence exhausted"
                                  else Int64.succ old.assignment_seq)
                            in
                            let work_kind =
                              match work with
                              | V.Session _ ->
                                  P.Session
                                    { phase = Start; kind = None; turn = "" }
                              | _ -> P.Reconcile
                            in
                            let execute : V.execute =
                              {
                                request =
                                  {
                                    identity =
                                      {
                                        run_id = S.run_id state;
                                        patch_id = Patch_id.to_string a.patch_id;
                                      };
                                    operation =
                                      {
                                        operation_id = op.id;
                                        attempt_id = assignment.request;
                                        assignment_seq = sequence;
                                      };
                                    expected_state_version = version;
                                    work = work_kind;
                                    guidance = "";
                                    recovery_mode =
                                      (match Worker_stream.capsule s with
                                      | Some bytes
                                        when not (String.equal bytes "{}") ->
                                          let cap =
                                            get
                                              (Worker_checkpoint.decode
                                                 (json bytes))
                                          in
                                          if
                                            String.equal
                                              (Worker_checkpoint.workspace_id
                                                 cap)
                                              assignment.resource
                                          then P.Retained_workspace
                                          else Lost_workspace
                                      | _ -> P.Retained_workspace);
                                    renewal = Ordinary;
                                  };
                                work;
                                context;
                                input_version = V.input_version context;
                              }
                            in
                            Some
                              (Coordinator.Command
                                 (assignment.id, wire (V.Execute execute))))
                    | _ -> None)
                | _ -> None)
            | _ -> None)
    in
    let retry =
      List.find_map (S.operations state) ~f:(fun op ->
          if
            corrupt_patch op.patch
            || S.reserved_count state >= config.capacity
            || not (S.can_recover state op ~now:(now ()))
          then None
          else
            let previous =
              List.find assignments ~f:(fun a ->
                  Patch_id.equal a.patch op.patch)
            in
            match previous with
            | Some old
              when String.equal old.operation op.id
                   && old.cleanup && (not op.maintenance)
                   && List.exists (S.results state) ~f:(fun (id, status) ->
                       String.equal id old.id
                       && List.mem [ "retryable"; "failed" ] status
                            ~equal:String.equal) -> (
                match schedule op with
                | None -> None
                | Some work ->
                    let id = next_id () in
                    Some
                      (Coordinator.Intent_batch
                         [
                           S.Continue (op.id, id);
                           S.Journal_attempt
                             ( "schedule:" ^ id,
                               Yojson.Safe.to_string
                                 (Coordinator_config.work_json work) );
                         ]))
            | Some old
              when (not old.retired) && old.cleanup
                   && Hashtbl.mem attached old.resource ->
                let a =
                  {
                    old with
                    S.id = next_id ();
                    operation = op.id;
                    request = next_id ();
                    reserved = true;
                    retired = false;
                    result_committed = false;
                    cleanup = false;
                  }
                in
                Some (Coordinator.Intent (S.Assign (a, now ())))
            | Some old when old.retired -> Some (provision op (Some old))
            | None -> Some (provision op None)
            | _ -> None)
    in
    let maintenance =
      List.find_map gameplan.patches ~f:(fun p ->
          match
            List.find assignments ~f:(fun a -> Patch_id.equal a.patch p.id)
          with
          | Some a
            when (not (corrupt_patch p.id))
                 && a.retired && a.cleanup && a.result_committed
                 && (not
                       (List.mem (S.terminal_patches state) p.id
                          ~equal:Patch_id.equal))
                 && not
                      (List.exists (S.operations state) ~f:(fun op ->
                           Patch_id.equal op.patch p.id
                           && (op.unfinished || op.attention))) ->
              Some (Coordinator.Intent (S.Resume_idle p.id))
          | _ -> None)
    in
    let scheduling_decisions =
      let decisions = Scheduler_decision.plan view in
      match Execution_mode.root mode with
      | None -> decisions
      | Some root ->
          List.filter decisions ~f:(fun (id, _) -> Patch_id.equal id root)
          @ List.filter decisions ~f:(fun (id, _) ->
              not (Patch_id.equal id root))
    in
    let root_integration =
      match Execution_mode.root mode with
      | None -> None
      | Some root_id
        when S.reserved_count state >= config.capacity
             || List.exists (S.operations state) ~f:(fun op ->
                 Patch_id.equal op.patch root_id
                 && (op.unfinished || op.attention)) ->
          None
      | Some root_id -> (
          match agent root_id with
          | Some root_agent
            when Execution_mode.integration_root_available root_agent
                 &&
                 match Patch_agent.worktree_state root_agent with
                 | Patch_agent.Materialized _ -> true
                 | Patch_agent.Unmaterialized -> false ->
              let root_branch = root_agent.branch in
              List.find_map view.agents ~f:(fun contributor ->
                  let revision = contributor.Patch_agent.head_oid in
                  if
                    Patch_id.equal contributor.patch_id root_id
                    || not
                         (Execution_mode.integration_ready mode
                            ~construction_open:true ~ignore_inflight:false
                            ~max_failures:3 ~terminal:root_branch contributor)
                  then None
                  else
                    Option.bind revision ~f:(fun revision ->
                        let already_integrated =
                          List.exists
                            (Branch_reconcile.contributor_integrations
                               root_agent.branch_reconcile) ~f:(fun receipt ->
                              Option.equal String.equal receipt.contributor
                                (Some (Patch_id.to_string contributor.patch_id))
                              && String.equal
                                   (Branch_reconcile.Commit.to_string
                                      receipt.capture.target_revision)
                                   revision)
                        in
                        if already_integrated then None
                        else
                          Option.map (Branch_reconcile.Commit.make revision)
                            ~f:(fun _ ->
                              (root_id, contributor.patch_id, revision))))
          | Some _ | None -> None)
    in
    let root_integration_admission =
      Option.map root_integration ~f:(fun (root_id, contributor, revision) ->
          let op : S.operation =
            {
              id = next_id ();
              patch = root_id;
              used = 0;
              budget = 3;
              retry_after = 0L;
              attention = false;
              unfinished = true;
              maintenance = false;
            }
          in
          let work =
            V.Integrate_revision
              { contributor = Patch_id.to_string contributor; revision }
          in
          Coordinator.Intent_batch
            [
              S.Admit op;
              S.Journal_attempt
                ( "schedule:" ^ op.id,
                  Yojson.Safe.to_string (Coordinator_config.work_json work) );
            ])
    in
    let admit =
      match root_integration_admission with
      | Some _ -> root_integration_admission
      | None ->
          List.find_map scheduling_decisions ~f:(fun (id, decision) ->
              if
                S.reserved_count state >= config.capacity
                || List.exists (S.operations state) ~f:(fun op ->
                    Patch_id.equal op.patch id && (op.unfinished || op.attention))
              then None
              else
                Option.bind decision ~f:(fun decision ->
                    if
                      match (decision, agent id) with
                      | Scheduler_decision.Start _, Some a ->
                          not
                            (List.is_empty
                               (Branch_reconcile.publications a.branch_reconcile))
                      | _ -> false
                    then None
                    else
                      let patch =
                        List.find gameplan.patches ~f:(fun p ->
                            Patch_id.equal p.id id)
                      in
                      Option.map patch ~f:(fun p ->
                          let op : S.operation =
                            {
                              id = next_id ();
                              patch = id;
                              used = 0;
                              budget = 3;
                              retry_after = 0L;
                              attention = false;
                              unfinished = true;
                              maintenance = false;
                            }
                          in
                          let work =
                            Coordinator_config.work_of_decision
                              ~spec:(p.description ^ "\n" ^ p.spec)
                              decision
                          in
                          Coordinator.Intent_batch
                            [
                              S.Admit op;
                              S.Journal_attempt
                                ( "schedule:" ^ op.id,
                                  Yojson.Safe.to_string
                                    (Coordinator_config.work_json work) );
                            ])))
    in
    let forge_action () =
      let ordered =
        List.drop view.agents !forge_cursor
        @ List.take view.agents !forge_cursor
      in
      let result =
        List.find_map ordered ~f:(fun a ->
            if
              corrupt_patch a.patch_id || a.Patch_agent.merged
              || List.exists assignments ~f:(fun assignment ->
                  Patch_id.equal assignment.patch a.patch_id
                  && assignment.reserved)
            then None
            else
              match
                List.hd (Branch_reconcile.publications a.branch_reconcile)
              with
              | None -> None
              | Some publication ->
                  let key =
                    Stdlib.Digest.to_hex
                      (Stdlib.Digest.string (Patch_id.to_string a.patch_id))
                  in
                  let create_id = "runtime-create-" ^ key in
                  let entry =
                    J.find (S.journal state) ("effect:" ^ create_id)
                  in
                  let pr =
                    match Patch_agent.pr_number a with
                    | Some n -> Some n
                    | None ->
                        Option.bind entry ~f:(fun e ->
                            match e.phase with
                            | Applied s ->
                                Option.bind (Int.of_string_opt s) ~f:(fun n ->
                                    if n > 0 then Some (Pr_number.of_int n)
                                    else None)
                            | _ -> None)
                  in
                  let base_branch = Option.value a.base_branch ~default:main in
                  let head_revision =
                    Branch_reconcile.Commit.to_string
                      publication.published_revision
                  in
                  let scope =
                    match
                      Branch_reconcile_executor.remote_base io
                        ~branch:(Branch.to_string base_branch)
                    with
                    | Ok (Some base) ->
                        let code, destination, _ =
                          io.git [ "remote"; "get-url"; "--push"; "origin" ]
                        in
                        let destination = String.strip destination in
                        if
                          code <> 0
                          || Result.is_error
                               (Branch_reconcile.check_observation_destination
                                  a.branch_reconcile
                                  ~observed:
                                    (Branch_reconcile.Remote_id.of_destination
                                       destination))
                        then None
                        else
                          Some
                            {
                              Coordinator_forge.ticket = next_id ();
                              pr = Option.value pr ~default:(Pr_number.of_int 1);
                              head_branch = a.branch;
                              base_branch;
                              head_revision;
                              base_revision =
                                Branch_reconcile.Commit.to_string base;
                              push_destination = destination;
                              generation = a.generation;
                            }
                    | _ -> None
                  in
                  Option.bind scope ~f:(fun scope ->
                      let body () =
                        let projections =
                          match latest a.patch_id with
                          | None -> []
                          | Some c ->
                              let cap =
                                get (Worker_checkpoint.decode (json c.bytes))
                              in
                              [
                                get
                                  (Worker_checkpoint.project cap
                                     ~run_id:(S.run_id state)
                                     ~patch_id:a.patch_id
                                     ~context:(Worker_checkpoint.context cap)
                                     ~state_version:c.version
                                     ~attention_id:
                                       (Worker_checkpoint.attention_id cap)
                                     ~gameplan ~main_branch:main);
                              ]
                        in
                        Coordinator_forge.render_notes ~gameplan
                          ~patches:
                            (List.filter gameplan.patches ~f:(fun p ->
                                 Patch_id.equal p.id a.patch_id))
                          ~publications:[ (a.patch_id, head_revision) ]
                          ~projections
                      in
                      let applied id =
                        match J.find (S.journal state) ("effect:" ^ id) with
                        | Some { phase = Applied _; _ } -> true
                        | _ -> false
                      in
                      let draft_id =
                        "runtime-draft-" ^ key ^ "-" ^ head_revision
                      in
                      let change effect_id action =
                        let captured =
                          match
                            J.find (S.journal state) ("effect:" ^ effect_id)
                          with
                          | None -> scope
                          | Some e -> (
                              try
                                match json e.scope with
                                | `List (saved :: _) ->
                                    Coordinator_observation.scope_of_yojson
                                      saved
                                | _ -> scope
                              with _ -> scope)
                        in
                        Some
                          (Coordinator.Forge_change
                             {
                               patch = a.patch_id;
                               scope = captured;
                               effect_id;
                               view;
                               renewal_id = None;
                               action;
                             })
                      in
                      match pr with
                      | None when a.busy -> None
                      | None ->
                          let scope =
                            match entry with
                            | None -> scope
                            | Some entry -> (
                                try
                                  match json entry.scope with
                                  | `List (saved :: _) ->
                                      Coordinator_observation.scope_of_yojson
                                        saved
                                  | _ -> scope
                                with _ -> scope)
                          in
                          Some
                            (Coordinator.Forge_create
                               {
                                 patch = a.patch_id;
                                 scope;
                                 effect_id = create_id;
                                 renewal_id = None;
                                 title =
                                   gameplan.project_name ^ " / "
                                   ^ Patch_id.to_string a.patch_id;
                                 body = body ();
                                 draft = true;
                               })
                      | Some _ ->
                          let elapsed = Eio.Time.now clock in
                          let previous =
                            Option.value (Hashtbl.find polled key) ~default:0.
                          in
                          if a.busy then
                            if Float.(elapsed -. previous >= 30.) then (
                              Hashtbl.set polled ~key ~data:elapsed;
                              Some (Coordinator.Observe (a.patch_id, scope)))
                            else None
                          else if not a.pr_body_delivered then
                            let body = body () in
                            change
                              ("runtime-body-" ^ key ^ "-" ^ head_revision ^ "-"
                             ^ Int.to_string a.generation ^ "-"
                              ^ Stdlib.Digest.to_hex (Stdlib.Digest.string body)
                              )
                              (Set_body body)
                          else if
                            Scheduler_decision.draft_allowed view a.patch_id
                            && not (applied draft_id)
                          then change draft_id (Set_draft false)
                          else if Float.(elapsed -. previous >= 30.) then (
                            Hashtbl.set polled ~key ~data:elapsed;
                            Some (Coordinator.Observe (a.patch_id, scope)))
                          else if
                            Option.value_map a.automerge_deadline ~default:false
                              ~f:(fun deadline ->
                                Float.(deadline <= Eio.Time.now clock))
                          then
                            Some
                              (Coordinator.Forge_change
                                 {
                                   patch = a.patch_id;
                                   scope;
                                   effect_id =
                                     "runtime-merge-" ^ key ^ "-"
                                     ^ head_revision;
                                   view;
                                   renewal_id = None;
                                   action =
                                     (if a.merge_queue_required then Enqueue
                                      else Merge);
                                 })
                          else None))
      in
      if Option.is_some result then
        forge_cursor := (!forge_cursor + 1) % List.length view.agents;
      result
    in
    match capsule_attention with
    | Some operation ->
        Some (Coordinator.Intent (S.Operation_attention operation))
    | None -> (
        match S.lifecycle state with
        | S.Cancelling _ when S.cancellation_complete state ->
            stopped_cleanly := true;
            Some Coordinator.Shutdown
        | Cancelling _
          when List.for_all (S.resource_owners state) ~f:(fun (r, _) ->
                   List.mem (S.retired_resources state) r ~equal:String.equal)
          ->
            Some (Coordinator.Close_run (request "close" []))
        | Sleeping _ | Cancelling _ | Draining _ -> None
        | Running when S.policy_tick_needed state view ~now:(now ()) ->
            Some (Coordinator.Intent (S.Policy_tick (view, now ())))
        | Running -> (
            match active with
            | Some _ -> active
            | None -> (
                match retry with
                | Some _ -> retry
                | None -> (
                    match admit with
                    | Some _ -> admit
                    | None -> (
                        match maintenance with
                        | Some _ -> maintenance
                        | None -> forge_action ())))))
  in
  let env : Coordinator.env =
    {
      store;
      client;
      next_write_id = next_id;
      next_event =
        (fun () ->
          Eio.Time.sleep clock 0.1;
          Some (Coordinator.Tick source));
      now;
      stream_timeout = 10.;
      mono_clock = Eio.Stdenv.mono_clock system;
      attached =
        (fun ~resource_id ~readable ->
          Hashtbl.set attached ~key:resource_id ~data:readable);
      gameplan;
      main_branch = main;
      stop_request;
      outcome =
        (function S.Cancel_complete -> stopped_cleanly := true | _ -> ());
      blocked = (fun why -> Stdlib.prerr_endline ("onton-coordinator: " ^ why));
    }
  in
  report_blocked := env.blocked;
  Coordinator.serve ~forge ~git:io ~write_status:Stdlib.print_endline
    ~net:(Eio.Stdenv.net system) ~clock ~control_path env;
  if not !stopped_cleanly then
    failwith "coordinator loop stopped without confirmed terminal cleanup"
