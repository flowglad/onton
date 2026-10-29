(* @archlint.module shell
   @archlint.domain worker-protocol *)

open Onton_core

type env = {
  input : Eio_unix.source_ty Eio.Resource.t;
  output : Eio_unix.sink_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Time.clock;
  process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  gameplan : Types.Gameplan.t;
  main_branch : Types.Branch.t;
  repo_root : string;
  project_name : string;
  backend : Llm_backend.t;
  observe_forge :
    branch:Types.Branch.t ->
    (Branch_reconcile.workspace_forge * Types.Pr_number.t option, string) result;
  ack_timeout : float;
  protocol_version : int;
}

exception Host_lost of string
exception Cancel_requested

let with_lock mutex f =
  Eio.Mutex.lock mutex;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock mutex) f

let agent snapshot patch_id =
  Orchestrator.agent snapshot.Runtime.orchestrator patch_id

let runtime_of_agent env patch_id restored context input =
  let runtime =
    Runtime.create ~gameplan:env.gameplan ~main_branch:env.main_branch ()
  in
  let snapshot = Runtime.read runtime Fun.id in
  let agents =
    Base.Map.set
      (Orchestrator.agents_map snapshot.orchestrator)
      ~key:patch_id ~data:restored
  in
  let orchestrator =
    match context with
    | None ->
        Ok
          (Orchestrator.restore
             ~graph:(Orchestrator.graph snapshot.orchestrator)
             ~agents
             ~outbox:(Base.Map.empty (module Types.Message_id))
             ~main_branch:env.main_branch ())
    | Some context ->
        Orchestrator.restore_worker ~gameplan:env.gameplan
          ~main_branch:env.main_branch ~context ?input ~agent:restored ()
  in
  Result.map
    (fun orchestrator ->
      Runtime.update runtime (fun snapshot ->
          { snapshot with Runtime.orchestrator });
      runtime)
    orchestrator

let status_of_session (result : Session_driver.run_result) =
  match result.disposition with
  | `Ok -> Worker_protocol.Completed_ok
  | `No_commits -> Completed_no_commits
  | `Failed -> Failed
  | `Retry_push -> Retryable

let status_of_recovery = function
  | Session_driver.Recovery_session result ->
      (status_of_session result, "", None)
  | Recovery_wait reason -> (Worker_protocol.Retryable, reason, None)
  | Recovery_attention { reason; renewal_id } ->
      (Attention, reason, Some renewal_id)
  | Recovery_merged pr ->
      ( Merged_observed,
        (match pr with
        | None -> ""
        | Some number -> string_of_int (Types.Pr_number.to_int number)),
        None )

let short_error exn =
  let message = Printexc.to_string exn in
  if String.length message <= 4096 then message else String.sub message 0 4096

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let capsule_matches_admission capsule receipt =
  let entries =
    match member "entries" receipt with
    | Some (`List entries) -> entries
    | _ -> []
  in
  let same_operation request op =
    let same field value = member field request = Some (`String value) in
    same "operation_id" op.Worker_protocol.operation_id
    && same "attempt_id" op.attempt_id
    && same "assignment_seq" (Int64.to_string op.assignment_seq)
  in
  let terminal_matches receipt =
    match (receipt, Worker_checkpoint.terminal capsule) with
    | Some (`Assoc values), Some (status, diagnostics, renewal_id) ->
        List.assoc_opt "status" values
        = Some (`String (Worker_checkpoint.status_name status))
        && List.assoc_opt "diagnostics" values = Some (`String diagnostics)
        && List.assoc_opt "renewal_id" values
           = Some
               (match renewal_id with
               | None -> `Null
               | Some value -> `String value)
    | Some `Null, None -> true
    | _ -> false
  in
  match List.rev entries with
  | entry :: predecessors -> (
      match (member "request" entry, member "terminal" entry) with
      | Some request, terminal -> (
          let latest = Worker_checkpoint.latest_operation capsule in
          match latest with
          | Some op when same_operation request op ->
              terminal = Some `Null || terminal_matches terminal
          | _ -> (
              match (terminal, predecessors, latest) with
              | Some `Null, predecessor :: _, Some op ->
                  (match member "request" predecessor with
                    | Some previous_request ->
                        same_operation previous_request op
                    | None -> false)
                  && terminal_matches (member "terminal" predecessor)
                  && member "expected_state_version" request
                     = Some
                         (`String
                            (Int64.to_string
                               (Worker_checkpoint.version capsule)))
              | Some `Null, [], None ->
                  Int64.equal (Worker_checkpoint.version capsule) 0L
              | _ -> false))
      | _ -> false)
  | [] -> Int64.equal (Worker_checkpoint.version capsule) 0L

let run env =
  let event_log_path = Filename.temp_file "onton-worker-" ".jsonl" in
  let protocol = ref (Worker_protocol.initial ()) in
  let v3 = ref (Worker_protocol_v3.initial ()) in
  let v3_assignment = ref None in
  let pending_input = ref None in
  let is_v3 = env.protocol_version = 3 in
  if env.protocol_version <> 2 && not is_v3 then
    invalid_arg "unsupported protocol version";
  let protocol_mutex = Eio.Mutex.create () in
  let output_mutex = Eio.Mutex.create () in
  let ack_mailbox = Eio.Stream.create 2 in
  let active_cancel = ref None in
  let host_alive = ref true in
  let shutdown_requested = ref false in
  let binding = ref None in
  let stopped = ref false in
  let send message =
    with_lock output_mutex (fun () ->
        let bytes =
          if is_v3 then
            match Worker_protocol_v3.encode_worker message with
            | Ok bytes -> bytes
            | Error reason -> failwith reason
          else Worker_protocol.encode message
        in
        Eio.Flow.copy_string bytes env.output)
  in
  let convert_v3 outputs =
    List.concat_map
      (function
        | Worker_protocol_v3.Send message -> [ Worker_protocol.Send message ]
        | Begin e ->
            v3_assignment := Some e;
            [
              Worker_protocol.Begin
                (Option.get (Worker_protocol.execute_message e.request));
            ]
        | Recover e ->
            v3_assignment := Some e;
            [
              Worker_protocol.Recover
                (Option.get (Worker_protocol.execute_message e.request));
            ]
        | Cancel_active -> [ Worker_protocol.Cancel_active ]
        | Stop -> [ Worker_protocol.Stop ]
        | Reject reason ->
            [
              Worker_protocol.Send
                (Worker_protocol.Protocol_error { identity = None; reason });
            ]
        | Apply_input input ->
            pending_input := Some input;
            [])
      outputs
  in
  let transition_v3 event =
    with_lock protocol_mutex (fun () ->
        let next, output = Worker_protocol_v3.step !v3 event in
        v3 := next;
        convert_v3 output)
  in
  let transition event =
    if is_v3 then transition_v3 (Worker_protocol_v3.Worker_event event)
    else
      with_lock protocol_mutex (fun () ->
          let next, output = Worker_protocol.step !protocol event in
          protocol := next;
          output)
  in
  let state_version () =
    if is_v3 then Worker_protocol_v3.state_version !v3
    else Worker_protocol.state_version !protocol
  in
  let is_stopped () =
    if is_v3 then Worker_protocol_v3.is_stopped !v3
    else Worker_protocol.is_stopped !protocol
  in
  let recovery_pending () =
    if is_v3 then Worker_protocol_v3.recovery_pending !v3
    else Worker_protocol.recovery_pending !protocol
  in
  let admission_receipt () =
    if is_v3 then Worker_protocol_v3.admission_receipt !v3
    else Worker_protocol.admission_receipt !protocol
  in
  let host_message msg =
    if is_v3 then transition_v3 (Worker_protocol_v3.Host msg)
    else
      match msg with
      | Worker_protocol_v3.Legacy_control msg ->
          transition (Worker_protocol.Host msg)
      | Worker_protocol_v3.Bind _ | Execute _ | Input _ ->
          failwith "v3 frame in v2 worker"
  in
  let rec emit = function
    | [] -> ()
    | Worker_protocol.Send message :: rest ->
        send message;
        emit rest
    | Worker_protocol.Stop :: rest ->
        stopped := true;
        emit rest
    | Worker_protocol.Cancel_active :: rest ->
        Option.iter
          (fun context -> Eio.Cancel.cancel context Cancel_requested)
          !active_cancel;
        emit rest
    | (Worker_protocol.Begin _ | Recover _) :: rest -> emit rest
  in
  let apply event = emit (transition event) in
  let fail_host reason =
    host_alive := false;
    apply Worker_protocol.Host_lost;
    raise (Host_lost reason)
  in
  let wait_for_ack () =
    match
      Eio.Time.with_timeout env.clock env.ack_timeout (fun () ->
          Ok (Eio.Stream.take ack_mailbox))
    with
    | Ok true -> ()
    | Ok false -> fail_host "checkpoint rejected"
    | Error `Timeout -> fail_host "checkpoint acknowledgement timed out"
  in
  let execute (request : Worker_protocol.execute_request) ~interrupted =
    let {
      Worker_protocol.identity;
      operation;
      work;
      guidance;
      recovery_mode;
      renewal;
      _;
    } =
      request
    in
    let assigned =
      if is_v3 then
        match !v3_assignment with
        | Some e when e.request = request -> Some e
        | _ -> fail_host "missing exact v3 assignment"
      else None
    in
    let guidance =
      match (assigned, !pending_input) with
      | Some _, Some input -> input.guidance
      | Some _, None -> (
          match
            Worker_checkpoint.v3_input
              (let _, capsule, _ = Option.get !binding in
               !capsule)
          with
          | Some input -> input.guidance
          | None -> guidance)
      | None, _ -> guidance
    in
    let renewal_id =
      match renewal with Ordinary -> None | Renewed id -> Some id
    in
    let patch_id = Types.Patch_id.of_string identity.patch_id in
    let runtime, capsule, workspace_id =
      match !binding with
      | Some bound -> bound
      | None -> failwith "worker execution without binding"
    in
    if
      (not
         (String.equal (Worker_checkpoint.workspace_id !capsule) workspace_id))
      && recovery_mode <> Worker_protocol.Lost_workspace
      && not interrupted
    then fail_host "retained workspace identity is unconfirmed";
    let root_intent =
      match assigned with
      | Some
          {
            work =
              Worker_protocol_v3.Integrate_revision { contributor; revision };
            context;
            _;
          } -> (
          if not (String.equal (Worker_protocol_v3.root_role context) "root")
          then fail_host "integration requires root role";
          match Branch_reconcile.Commit.make revision with
          | None -> fail_host "invalid captured contributor revision"
          | Some revision ->
              Some
                Branch_reconcile.
                  {
                    base = Worker_protocol_v3.selected_base context;
                    policy = Preserve_ancestry;
                    purpose = Integrate_revision { contributor; revision };
                  })
      | Some { work = Session _ | Reconcile; _ } | None -> None
    in
    (* Freeze this turn's base with its admitted guidance. Inputs received while
       it runs become desired context only at the terminal safe checkpoint. *)
    let captured_base =
      match !pending_input with
      | Some input -> input.selected_base
      | None -> (
          match Worker_checkpoint.v3_input !capsule with
          | Some input -> input.selected_base
          | None -> (
              match assigned with
              | Some e -> Worker_protocol_v3.selected_base e.context
              | None -> Types.Branch.to_string env.main_branch))
    in
    let desired_base () = captured_base in
    let proposal_number = ref 0 in
    let terminal = ref None in
    let apply_input_at_boundary = ref true in
    let apply_pending_input orchestrator =
      match !pending_input with
      | Some input
        when is_v3
             && not
                  (Option.exists
                     (fun old ->
                       old.Worker_protocol_v3.version >= input.version)
                     (Worker_checkpoint.v3_input !capsule)) ->
          Orchestrator.apply_worker_input orchestrator patch_id input
      | Some _ | None -> orchestrator
    in
    let persist_unprotected snapshot =
      incr proposal_number;
      let version =
        match state_version () with
        | Some version when Int64.compare version Int64.max_int < 0 ->
            Int64.succ version
        | _ -> fail_host "checkpoint version exhausted"
      in
      let current = agent snapshot patch_id in
      let workspace_confirmed =
        (recovery_mode = Worker_protocol.Lost_workspace || interrupted)
        && Option.is_some
             (Branch_reconcile.workspace_outcome current.branch_reconcile)
        && Option.is_none current.worktree_path
        && Option.is_none current.llm_session_id
      in
      let candidate =
        match
          Worker_checkpoint.advance !capsule ~operation ~guidance
            ~workspace_id:
              (if workspace_confirmed then Some workspace_id else None)
            ~agent:current ~terminal:!terminal
        with
        | Ok candidate
          when Int64.equal (Worker_checkpoint.version candidate) version -> (
            match !pending_input with
            | Some input when !apply_input_at_boundary ->
                Worker_checkpoint.with_v3_input candidate input
            | Some _ | None -> candidate)
        | Ok _ -> fail_host "capsule and protocol state version differ"
        | Error reason -> fail_host reason
      in
      let bytes = Worker_checkpoint.to_string candidate in
      let proposal_id =
        Printf.sprintf "%s-%s-%d" operation.operation_id operation.attempt_id
          !proposal_number
      in
      let outputs =
        transition
          (Worker_protocol.Propose_checkpoint { proposal_id; capsule = bytes })
      in
      if
        not
          (List.exists
             (function
               | Worker_protocol.Send (Checkpoint _) -> true
               | Send
                   ( Ready _ | Accepted _ | Observation _ | Result _
                   | Protocol_error _ )
               | Begin _ | Recover _ | Cancel_active | Stop ->
                   false)
             outputs)
      then fail_host "checkpoint proposal refused";
      emit outputs;
      wait_for_ack ();
      capsule := candidate;
      Ok ()
    in
    let persist snapshot =
      Eio.Cancel.protect (fun () -> persist_unprotected snapshot)
    in
    (* Begin is only an internal dispatch. This first checkpoint freezes the
       assigned operation and the post-admission receipt before any checkout,
       hook, backend, or owner event can run. An accepted ACK attests that the
       host committed that receipt with these exact capsule bytes. *)
    (match
       Runtime.update_persisting runtime ~persist (fun snapshot ->
           let orchestrator =
             if is_v3 then
               Orchestrator.set_base_branch snapshot.orchestrator patch_id
                 (Types.Branch.of_string (desired_base ()))
             else snapshot.orchestrator
           in
           let orchestrator = apply_pending_input orchestrator in
           ({ snapshot with Runtime.orchestrator }, ()))
     with
    | Ok () -> apply_input_at_boundary := false
    | Error reason -> fail_host reason);
    let module C = struct
      let persist = persist

      let clear_session_id ~patch_id:id =
        if not (Types.Patch_id.equal id patch_id) then
          Error "session identity belongs to another patch"
        else
          Runtime.update_persisting runtime ~persist (fun snapshot ->
              ( {
                  snapshot with
                  Runtime.orchestrator =
                    Orchestrator.set_llm_session_id snapshot.orchestrator id
                      None;
                },
                () ))

      let record_session_id ~patch_id:id ~session_id =
        if not (Types.Patch_id.equal id patch_id) then
          Error "session identity belongs to another patch"
        else
          Runtime.update_persisting runtime ~persist (fun snapshot ->
              ( {
                  snapshot with
                  Runtime.orchestrator =
                    Orchestrator.set_llm_session_id snapshot.orchestrator id
                      (Some session_id);
                },
                () ))
    end in
    let module Real =
      (val Worktree.make ~fs:env.fs ~config:Worktree_lifecycle.git
             ~clock:env.clock ~process_mgr:env.process_mgr
             ~repo_root:env.repo_root)
    in
    let module Env = struct
      let runtime = runtime
      let fs = env.fs
      let clock = env.clock
      let worktree_mutex = Eio.Mutex.create ()
      let hook_mutex = Eio.Mutex.create ()
      let fetch_mutex = Eio.Mutex.create ()
      let project_name = env.project_name

      let user_config =
        User_config.load ~github_owner:env.gameplan.repo_owner
          ~github_repo:env.gameplan.repo_name

      let owner = env.gameplan.repo_owner
      let repo = env.gameplan.repo_name
      let transcripts = Stdlib.Hashtbl.create 1
      let transcript_updates = Stdlib.Hashtbl.create 1
      let event_log = Event_log.create ~path:event_log_path
    end in
    let module Driver = Session_driver.Make_with_checkpoint (C) (Real) (Env) in
    let module Setup = Worktree_setup.Make_with_checkpoint (C) (Real) (Env) in
    let request_id = operation.operation_id ^ ":" ^ operation.attempt_id in
    let now () = Eio.Time.now env.clock in
    let current_agent () =
      Runtime.read runtime (fun snapshot -> agent snapshot patch_id)
    in
    let observe_forge () =
      env.observe_forge ~branch:(current_agent ()).branch
    in
    let classify_setup = function
      | Worktree_setup.Recovery_ready _ ->
          ( Worker_protocol.Retryable,
            "workspace observed; submit a fresh assignment",
            None )
      | Recovery_wait reason -> (Retryable, reason, None)
      | Recovery_attention { reason; renewal_id } ->
          (Attention, reason, Some renewal_id)
      | Recovery_merged pr ->
          ( Merged_observed,
            (match pr with
            | None -> ""
            | Some number -> string_of_int (Types.Pr_number.to_int number)),
            None )
    in
    let recovery_context () =
      let current = current_agent () in
      let io =
        Branch_reconcile_executor.make_io ~process_mgr:env.process_mgr
          ~clock:env.clock ~path:env.repo_root
      in
      let patch_revision =
        match
          Branch_reconcile_executor.remote_head ~owner:current.branch_reconcile
            io
            ~branch:(Types.Branch.to_string current.branch)
        with
        | Ok (Some sha) ->
            Session_prompt.Revision (Branch_reconcile.Commit.to_string sha)
        | Ok None -> Session_prompt.Absent
        | Error _ -> Session_prompt.Unavailable
      in
      let base = Option.value current.base_branch ~default:env.main_branch in
      let base_revision =
        match
          Branch_reconcile_executor.remote_base io
            ~branch:(Types.Branch.to_string base)
        with
        | Ok (Some sha) -> Branch_reconcile.Commit.to_string sha
        | Ok None | Error _ -> "unavailable"
      in
      Session_prompt.create_recovery
        ~requirements:(Worker_checkpoint.context !capsule)
        ~operation:
          (match work with
          | Worker_protocol.Session { turn; _ } -> turn
          | Reconcile -> "Reconcile assigned patch branch")
        ~guidance:[ guidance ] ~patch_revision ~base_revision
    in
    let admit snapshot =
      let orchestrator =
        if interrupted then snapshot.Runtime.orchestrator
        else
          match work with
          | Worker_protocol.Reconcile -> snapshot.orchestrator
          | Worker_protocol.Session { phase = Start; _ } ->
              let current = agent snapshot patch_id in
              if current.busy then snapshot.orchestrator
              else if is_v3 then
                Orchestrator.start_assigned_worker snapshot.orchestrator
                  patch_id
                  ~base:(Types.Branch.of_string (desired_base ()))
              else
                Orchestrator.fire snapshot.orchestrator
                  (Start
                     ( patch_id,
                       Option.value current.base_branch ~default:env.main_branch
                     ))
          | Session { phase = Respond; kind; _ } -> (
              match kind with
              | None -> snapshot.orchestrator
              | Some kind ->
                  Orchestrator.fire snapshot.orchestrator
                    (Respond (patch_id, kind)))
      in
      ({ snapshot with Runtime.orchestrator }, ())
    in
    let run_session phase kind turn =
      Runtime.with_patch_ownership runtime ~patch_id (fun owner ->
          let current = current_agent () in
          if not current.busy then
            (Worker_protocol.Failed, "session not admitted by patch state", None)
          else
            let delivery_mode =
              match phase with
              | Worker_protocol.Start -> Patch_decision.Start
              | Respond -> Patch_decision.Respond
            in
            let prompt =
              Driver.create_prompt
                ~context:(fun ~worktree_path:_ ->
                  Worker_checkpoint.context !capsule ^ "\n" ^ guidance ^ "\n")
                ~turn
            in
            let on_pr_detected _ = () in
            if recovery_mode = Worker_protocol.Lost_workspace then
              Driver.run_recovery_owned ~write_owner:owner ~request:request_id
                ~renewal_id ~observe_forge ~recovery:(recovery_context ()) ~kind
                ~delivery_mode ~patch_id ~prompt ~on_pr_detected
                ~backend:env.backend ~complexity:current.complexity ()
              |> status_of_recovery
            else
              let result =
                Driver.run_owned ~write_owner:owner ~kind ~delivery_mode
                  ~patch_id ~prompt ~agent:current ~on_pr_detected
                  ~backend:env.backend ~complexity:current.complexity
              in
              (status_of_session result, "", None))
    in
    let run_reconcile () =
      let setup =
        Runtime.with_patch_ownership runtime ~patch_id (fun owner ->
            if interrupted || recovery_mode = Worker_protocol.Lost_workspace
            then
              Some
                (Setup.recover_owned ~owner ~request:request_id ~renewal_id
                   ?intent:root_intent ~observe_forge ())
            else None)
      in
      Option.iter
        (fun result ->
          let text =
            match result with
            | Worktree_setup.Recovery_ready _ -> "workspace ready"
            | Recovery_wait reason -> "workspace waiting: " ^ reason
            | Recovery_attention { reason; _ } ->
                "workspace needs renewed intent: " ^ reason
            | Recovery_merged _ -> "merge observed"
          in
          apply (Worker_protocol.Report_observation text))
        setup;
      match setup with
      | Some result when interrupted -> classify_setup result
      | Some (Worktree_setup.Recovery_wait reason) ->
          (Worker_protocol.Retryable, reason, None)
      | Some (Recovery_attention { reason; renewal_id }) ->
          (Attention, reason, Some renewal_id)
      | Some (Recovery_merged pr) ->
          ( Merged_observed,
            (match pr with
            | None -> ""
            | Some number -> string_of_int (Types.Pr_number.to_int number)),
            None )
      | Some (Recovery_ready _) | None -> (
          let outcome =
            Runtime.with_patch_ownership runtime ~patch_id (fun owner ->
                let current = current_agent () in
                let checkout =
                  match setup with
                  | Some (Recovery_ready path) -> Worktree_setup.Path path
                  | None -> Setup.ensure_owned ~owner ()
                  | Some
                      ( Recovery_wait _ | Recovery_attention _
                      | Recovery_merged _ ) ->
                      assert false
                in
                match checkout with
                | Worktree_setup.Unavailable failure ->
                    Error (Worktree_provision.message failure)
                | Path _ ->
                    let intent =
                      match root_intent with
                      | Some intent -> intent
                      | None ->
                          Branch_reconcile.
                            {
                              base =
                                Types.Branch.to_string
                                  (Option.value current.base_branch
                                     ~default:env.main_branch);
                              policy = Rewrite;
                              purpose = Reconcile_base;
                            }
                    in
                    Ok
                      (Branch_reconcile_runner.run_owned ~owner ~persist ~now
                         ~execute:(Setup.execute_reconciliation ~patch_id)
                         (Branch_reconcile.Request intent)))
          in
          let outcome =
            match outcome with
            | Error reason -> Error reason
            | Ok (Branch_reconcile_runner.Repair_needed token) ->
                let perform ~agent ~turn =
                  let path =
                    Option.value agent.Patch_agent.worktree_path
                      ~default:
                        (Worktree.worktree_dir ~project_name:env.project_name
                           ~patch_id)
                  in
                  Driver.run_repair ~patch_id ~agent ~backend:env.backend
                    ~complexity:agent.complexity
                    ~cwd:Eio.Path.(env.fs / path)
                    ~context:(Worker_checkpoint.context !capsule)
                    ~guidance:[ guidance ] ~turn
                    ~read_head:(fun () ->
                      Real.read_branch_sha ~path
                        ~ref_name:
                          ("refs/heads/" ^ Types.Branch.to_string agent.branch))
                in
                Ok
                  (Branch_reconcile_runner.run_repair ~runtime ~persist
                     ~patch_id
                     ~with_capacity:(fun run -> run ())
                     ~now
                     ~execute:(fun ~agent:_ ->
                       Setup.execute_reconciliation ~patch_id)
                     ~perform token)
            | Ok
                ((Idle | Waiting | Intervention _ | Checkpoint_failed _) as
                 result) ->
                Ok result
          in
          match outcome with
          | Error reason -> (Retryable, reason, None)
          | Ok Idle -> (Completed_ok, "", None)
          | Ok Waiting -> (Retryable, "reconciliation pending", None)
          | Ok (Repair_needed _) -> (Retryable, "repair pending", None)
          | Ok (Intervention reason) -> (Worker_protocol.Failed, reason, None)
          | Ok (Checkpoint_failed reason) -> fail_host reason)
    in
    let body () =
      match Runtime.update_persisting runtime ~persist admit with
      | Error reason -> fail_host reason
      | Ok () -> (
          match work with
          | Worker_protocol.Reconcile -> run_reconcile ()
          | Session _ when interrupted -> run_reconcile ()
          | Session { phase; kind; turn } -> run_session phase kind turn)
    in
    let status, diagnostics, renewal_id =
      try body () with
      | Host_lost _ as exn -> raise exn
      | Eio.Cancel.Cancelled _ ->
          if !host_alive then (Worker_protocol.Cancelled, "cancelled", None)
          else raise (Host_lost "host disconnected")
      | exn -> (Worker_protocol.Failed, short_error exn, None)
    in
    Eio.Cancel.protect (fun () ->
        (match assigned with
        | Some { work = Worker_protocol_v3.Session _; _ } ->
            let current =
              Runtime.read runtime (fun snapshot -> agent snapshot patch_id)
            in
            let publication =
              match Branch_reconcile.publications current.branch_reconcile with
              | receipt :: _ ->
                  Branch_reconcile.Commit.to_string receipt.published_revision
              | [] -> "unpublished"
            in
            let path =
              Project_store.pr_body_artifact_path ~project_name:env.project_name
                ~patch_id
            in
            let artifact =
              try
                let channel = Stdlib.open_in_bin path in
                Fun.protect
                  ~finally:(fun () -> Stdlib.close_in_noerr channel)
                  (fun () ->
                    let size = Stdlib.in_channel_length channel in
                    if size > 262144 then Worker_protocol_v3.Oversized
                    else
                      Worker_protocol_v3.note ~assignment:operation ~publication
                        (Ok (Stdlib.really_input_string channel size)))
              with Sys_error _ | End_of_file -> (
                match Unix.stat path with
                | _ -> Worker_protocol_v3.Unreadable
                | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                    Worker_protocol_v3.Missing
                | exception _ -> Worker_protocol_v3.Unreadable)
            in
            capsule := Worker_checkpoint.with_note !capsule artifact
        | Some { work = Reconcile | Integrate_revision _; _ } | None -> ());
        terminal := Some (status, diagnostics, renewal_id);
        (* A newer input received during the captured turn becomes desired
           context only at this checkpointed safe boundary. Its version and
           base travel in the same capsule as the updated owner projection. *)
        apply_input_at_boundary := true;
        (match
           Runtime.update_persisting runtime ~persist (fun snapshot ->
               let orchestrator =
                 match !pending_input with
                 | Some input when is_v3 ->
                     Orchestrator.set_base_branch snapshot.orchestrator patch_id
                       (Types.Branch.of_string input.selected_base)
                 | Some _ | None -> snapshot.orchestrator
               in
               let orchestrator =
                 match work with
                 | Session { phase = Respond; kind = Some kind; _ } ->
                     let outcome =
                       match status with
                       | Worker_protocol.Completed_ok -> Orchestrator.Respond_ok
                       | Completed_no_commits -> Respond_no_commits
                       | Retryable | Cancelled | Failed | Attention
                       | Merged_observed ->
                           Respond_failed
                     in
                     Orchestrator.apply_respond_outcome orchestrator patch_id
                       kind outcome
                 | Session { phase = Start; _ }
                 | Session { phase = Respond; kind = None; _ }
                 | Reconcile ->
                     Orchestrator.complete orchestrator patch_id
               in
               let orchestrator = apply_pending_input orchestrator in
               ({ snapshot with Runtime.orchestrator }, ()))
         with
        | Ok () -> ()
        | Error reason -> fail_host reason);
        apply Worker_protocol.Cleanup_complete;
        apply (Worker_protocol.Finish { status; diagnostics; renewal_id }))
  in
  let validate_bind message context_v3 =
    match message with
    | Worker_protocol.Bind
        {
          identity;
          workspace_id;
          context;
          capsule = bytes;
          state_version;
          attention_id;
          _;
        } -> (
        let patch_id = Types.Patch_id.of_string identity.patch_id in
        let initial_agent =
          let runtime =
            Runtime.create ~gameplan:env.gameplan ~main_branch:env.main_branch
              ()
          in
          Runtime.read runtime (fun snapshot ->
              Orchestrator.find_agent snapshot.orchestrator patch_id)
        in
        let candidate =
          if
            String.equal bytes "{}"
            && Int64.equal state_version 0L
            && Option.is_none attention_id
          then
            match initial_agent with
            | Some initial ->
                Ok
                  ( Worker_checkpoint.initial ~run_id:identity.run_id ~patch_id
                      ~workspace_id ~context ~agent:initial,
                    initial )
            | None -> Error "unknown assigned patch"
          else
            match Yojson.Safe.from_string bytes with
            | json -> (
                match Worker_checkpoint.decode json with
                | Error _ as error -> error
                | Ok capsule ->
                    Result.map
                      (fun agent -> (capsule, agent))
                      (Worker_checkpoint.validate_binding capsule
                         ~run_id:identity.run_id ~patch_id ~context
                         ~state_version ~attention_id ~gameplan:env.gameplan
                         ~main_branch:env.main_branch))
            | exception _ -> Error "malformed capsule JSON"
        in
        let candidate =
          match (candidate, context_v3) with
          | Ok (capsule, agent), Some context -> (
              match Worker_checkpoint.v3_context capsule with
              | None when Int64.equal state_version 0L ->
                  Ok (Worker_checkpoint.with_v3_context capsule context, agent)
              | Some bytes
                when String.equal bytes
                       (Worker_protocol_v3.context_bytes context) ->
                  Ok (capsule, agent)
              | _ -> Error "v3 capsule context mismatch")
          | Ok (capsule, agent), None
            when Option.is_none (Worker_checkpoint.v3_context capsule) ->
              Ok (capsule, agent)
          | Ok _, None -> Error "v3 capsule bound by v2"
          | Error reason, _ -> Error reason
        in
        match candidate with
        | Error reason -> Error reason
        | Ok (capsule, restored) -> (
            let outputs =
              host_message
                (match context_v3 with
                | Some context -> Worker_protocol_v3.Bind (message, context)
                | None -> Worker_protocol_v3.Legacy_control message)
            in
            if is_stopped () then (
              emit outputs;
              Error "invalid admission receipt")
            else
              match admission_receipt () with
              | Some receipt when capsule_matches_admission capsule receipt -> (
                  match
                    runtime_of_agent env patch_id restored context_v3
                      (Worker_checkpoint.v3_input capsule)
                  with
                  | Error reason -> Error reason
                  | Ok runtime ->
                      binding := Some (runtime, ref capsule, workspace_id);
                      (match Worker_checkpoint.v3_input capsule with
                      | None -> ()
                      | Some input ->
                          if
                            transition_v3
                              (Worker_protocol_v3.Restore_input input)
                            <> []
                          then
                            fail_host
                              "restored input does not match v3 admission";
                          pending_input := Some input);
                      emit outputs;
                      Ok ())
              | Some _ | None -> Error "capsule and admission disagree"))
    | Worker_protocol.Execute _ | Cancel _ | Checkpoint_ack _ | Result_ack _
    | Shutdown _ ->
        Error "not a bind"
  in
  Fun.protect
    ~finally:(fun () -> try Sys.remove event_log_path with Sys_error _ -> ())
    (fun () ->
      Eio.Switch.run (fun sw ->
          let spawn request ~interrupted =
            Eio.Fiber.fork ~sw (fun () ->
                try
                  Eio.Cancel.sub (fun context ->
                      active_cancel := Some context;
                      Fun.protect
                        ~finally:(fun () -> active_cancel := None)
                        (fun () -> execute request ~interrupted))
                with
                | Host_lost _ when !shutdown_requested -> ()
                | Eio.Cancel.Cancelled _ when !shutdown_requested -> ())
          in
          let dispatch = function
            | Worker_protocol.Begin (Worker_protocol.Execute request) ->
                spawn request ~interrupted:false
            | Recover (Worker_protocol.Execute request) ->
                spawn request ~interrupted:true
            | Begin
                ( Bind _ | Cancel _ | Checkpoint_ack _ | Result_ack _
                | Shutdown _ )
            | Recover
                ( Bind _ | Cancel _ | Checkpoint_ack _ | Result_ack _
                | Shutdown _ ) ->
                ()
            | (Worker_protocol.Send _ | Cancel_active | Stop) as output ->
                emit [ output ]
          in
          let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) env.input in
          let rec loop () =
            if not !stopped then
              let line =
                try Some (Eio.Buf_read.line reader ^ "\n") with
                | End_of_file -> None
                | Eio.Buf_read.Buffer_limit_exceeded ->
                    send
                      (Worker_protocol.Protocol_error
                         { identity = None; reason = "frame too large" });
                    fail_host "frame too large"
              in
              match line with
              | None -> fail_host "host disconnected"
              | Some line -> (
                  let decoded =
                    if is_v3 then
                      Result.map_error
                        (fun reason -> Worker_protocol.Invalid reason)
                        (Worker_protocol_v3.decode_host line)
                    else
                      Result.map
                        (fun msg -> Worker_protocol_v3.Legacy_control msg)
                        (Worker_protocol.decode line)
                  in
                  match decoded with
                  | Error (Worker_protocol.Invalid reason) ->
                      send
                        (Worker_protocol.Protocol_error
                           { identity = None; reason });
                      loop ()
                  | Error (Incompatible reason) ->
                      send
                        (Worker_protocol.Protocol_error
                           { identity = None; reason });
                      fail_host reason
                  | Ok
                      (( Worker_protocol_v3.Bind (Worker_protocol.Bind _, _)
                       | Legacy_control (Worker_protocol.Bind _) ) as bound) ->
                      let message, context =
                        match bound with
                        | Worker_protocol_v3.Bind (message, context) ->
                            (message, Some context)
                        | Legacy_control message -> (message, None)
                        | Execute _ | Input _ -> assert false
                      in
                      if Option.is_none !binding then (
                        match validate_bind message context with
                        | Ok () ->
                            if recovery_pending () then
                              List.iter dispatch
                                (if is_v3 then
                                   transition_v3
                                     Worker_protocol_v3.Recover_unfinished
                                 else
                                   transition Worker_protocol.Recover_unfinished)
                        | Error reason ->
                            send
                              (Worker_protocol.Protocol_error
                                 { identity = None; reason });
                            fail_host reason)
                      else apply (Worker_protocol.Host message);
                      loop ()
                  | Ok (Worker_protocol_v3.Execute _ as envelope) ->
                      List.iter dispatch (host_message envelope);
                      loop ()
                  | Ok (Worker_protocol_v3.Input (_, input) as envelope) ->
                      let already =
                        Option.exists (fun old -> old = input) !pending_input
                      in
                      List.iter dispatch (host_message envelope);
                      (if not already then
                         let committed =
                           transition_v3
                             (Worker_protocol_v3.Input_committed
                                (input.input_id, input.version))
                         in
                         match (committed, !pending_input) with
                         | [], Some applied when applied = input -> ()
                         | _ -> fail_host "input commit rejected");
                      loop ()
                  | Ok
                      (Legacy_control
                         (( Worker_protocol.Execute _ | Cancel _
                          | Checkpoint_ack _ | Result_ack _ | Shutdown _ ) as
                          message) as envelope) ->
                      let before = state_version () in
                      let outputs = host_message envelope in
                      (match message with
                      | Worker_protocol.Shutdown _
                        when is_stopped ()
                             && List.exists
                                  (function
                                    | Worker_protocol.Cancel_active -> true
                                    | Send _ | Begin _ | Recover _ | Stop ->
                                        false)
                                  outputs ->
                          shutdown_requested := true;
                          host_alive := false;
                          Eio.Stream.add ack_mailbox false
                      | Bind _ | Execute _ | Cancel _ | Checkpoint_ack _
                      | Result_ack _ | Shutdown _ ->
                          ());
                      List.iter dispatch outputs;
                      (match message with
                      | Worker_protocol.Checkpoint_ack { accepted; _ } ->
                          let after = state_version () in
                          if before <> after || is_stopped () then
                            Eio.Stream.add ack_mailbox accepted
                      | Worker_protocol.Bind _ | Execute _ | Cancel _
                      | Result_ack _ | Shutdown _ ->
                          ());
                      loop ()
                  | Ok (Worker_protocol_v3.Bind _) ->
                      fail_host "invalid binding")
          in
          loop ()))
