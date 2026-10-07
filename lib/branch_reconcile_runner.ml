(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)

type outcome =
  | Idle
  | Repair_needed of Branch_reconcile.token
  | Waiting
  | Intervention of string
  | Checkpoint_failed of string

let checkpoint ~runtime ~persist ~patch_id event =
  Runtime.update_persisting runtime ~persist (fun snap ->
      let orchestrator, commands =
        Orchestrator.reconcile_branch snap.Runtime.orchestrator patch_id event
      in
      ({ snap with Runtime.orchestrator }, commands))

let run_unlocked ~runtime ~persist ~patch_id ~now ~execute event =
  let hold_reason () =
    Runtime.read runtime (fun snap ->
        Patch_agent.reconciliation_hold_reason
          (Orchestrator.agent snap.Runtime.orchestrator patch_id))
  in
  let rec dispatch commands =
    match hold_reason () with
    | Some reason -> Intervention reason
    | None -> dispatch_authorized commands
  and dispatch_authorized = function
    | [] -> (
        let state =
          Runtime.read runtime (fun snap ->
              (Orchestrator.agent snap.Runtime.orchestrator patch_id)
                .Patch_agent.branch_reconcile)
        in
        match Branch_reconcile.phase state with
        | Some (Branch_reconcile.Intervention reason) -> Intervention reason
        | Some (Branch_reconcile.Waiting _) -> Waiting
        | Some (Branch_reconcile.Repairing _) -> (
            match Branch_reconcile.operation state with
            | Some _ -> Waiting
            | None -> Idle)
        | Some
            ( Branch_reconcile.Preparing | Branch_reconcile.Integrating
            | Branch_reconcile.Publishing | Branch_reconcile.Confirming
            | Branch_reconcile.Recovering | Branch_reconcile.Settled )
        | None ->
            Idle)
    | Branch_reconcile.Completed _ :: rest -> dispatch rest
    | Branch_reconcile.Repair token :: _ -> (
        match
          checkpoint ~runtime ~persist ~patch_id
            (Branch_reconcile.Repair_started token)
        with
        | Error message -> Checkpoint_failed message
        | Ok commands -> dispatch commands)
    | Branch_reconcile.Start_repair token :: _ -> Repair_needed token
    | Branch_reconcile.Execute command :: rest -> (
        let operation =
          Runtime.read runtime (fun snap ->
              Branch_reconcile.operation
                (Orchestrator.agent snap.Runtime.orchestrator patch_id)
                  .Patch_agent.branch_reconcile)
        in
        match operation with
        | None -> Intervention "checkpointed_operation_missing"
        | Some operation -> (
            (* Cancellation leaves the pre-command checkpoint intact. Restart
                must inspect it; neither a timeout nor a lost acknowledgement
                authorizes repetition of a Git mutation. *)
            let result = execute ~operation command in
            match
              checkpoint ~runtime ~persist ~patch_id
                (Branch_reconcile.Result
                   { token = command.token; at = now (); result })
            with
            | Error message -> Checkpoint_failed message
            | Ok commands -> dispatch (commands @ rest)))
  in
  match hold_reason () with
  | Some reason -> Intervention reason
  | None -> (
      match checkpoint ~runtime ~persist ~patch_id event with
      | Error message -> Checkpoint_failed message
      | Ok commands -> dispatch commands)

let run_owned ~owner ~persist ~now ~execute event =
  Runtime.with_owned_patch owner (fun runtime patch_id ->
      run_unlocked ~runtime ~persist ~patch_id ~now ~execute event)

let run ~runtime ~persist ~patch_id ~now ~execute event =
  Runtime.with_patch_ownership runtime ~patch_id (fun owner ->
      run_owned ~owner ~persist ~now ~execute event)

let run_repair ~runtime ~persist ~patch_id ~with_capacity ~now ~execute ~perform
    token =
  with_capacity (fun () ->
      Runtime.with_patch_ownership runtime ~patch_id (fun owner ->
          let agent =
            Runtime.read runtime (fun snap ->
                Orchestrator.find_agent snap.Runtime.orchestrator patch_id)
          in
          match agent with
          | None -> Idle
          | Some agent -> (
              match Patch_agent.reconciliation_hold_reason agent with
              | Some reason -> Intervention reason
              | None -> (
                  match
                    Branch_reconcile.repair_turn
                      agent.Patch_agent.branch_reconcile
                      ~branch:(Types.Branch.to_string agent.branch)
                      token
                  with
                  | None -> Idle
                  | Some _ -> (
                      (* Capacity waits release Git ownership. Inspect again
                         before handing the checkout to an agent, retaining any
                         newly discovered work under the normal checkpoint
                         protocol. The original token has already been checked;
                         a stale queued invocation cannot claim a newer turn. *)
                      match
                        run_owned ~owner ~persist ~now ~execute:(execute ~agent)
                          Branch_reconcile.Recover
                      with
                      | Repair_needed token -> (
                          let agent =
                            Runtime.read runtime (fun snap ->
                                Orchestrator.agent snap.Runtime.orchestrator
                                  patch_id)
                          in
                          match
                            Branch_reconcile.repair_turn
                              agent.Patch_agent.branch_reconcile
                              ~branch:(Types.Branch.to_string agent.branch)
                              token
                          with
                          | None -> Idle
                          | Some turn ->
                              let event = perform ~agent ~turn in
                              run_owned ~owner ~persist ~now
                                ~execute:(execute ~agent) event)
                      | (Idle | Waiting | Intervention _ | Checkpoint_failed _)
                        as outcome ->
                          outcome)))))
