(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)

let run ~context ~guidance ~backend ~on_event ~cwd ~project_name ~patch_id
    ~complexity ~turn ~read_head ~now =
  let read_head () =
    try Option.bind (read_head ()) Branch_reconcile.Commit.make
    with exn -> if Process_tree.has_cancellation exn then raise exn else None
  in
  let before_head = read_head () in
  let turn_accepted = ref false in
  let result, failure_detail =
    if not (Branch_reconcile.repair_head_matches turn before_head) then
      (None, "repair_head_probe_unavailable")
    else
      try
        let session_uuid = Session_id.mint () in
        Telemetry_dispatch.emit
          (Telemetry.Event.Action
             {
               patch_id;
               session_uuid = Some session_uuid;
               payload =
                 `Assoc
                   [
                     ("event_log_kind", `String "branch_repair_session_started");
                     ("operation_id", `Int turn.Branch_reconcile.token.operation);
                     ("command_id", `Int turn.Branch_reconcile.token.command);
                   ];
             });
        ( Some
            (backend.Llm_backend.run_streaming ~project_name ~cwd ~patch_id
               ~prompt:
                 (Branch_reconcile.recovery_prompt ~context ~guidance turn)
               ~resume_session:None ~session_uuid ~complexity
               ~on_event:(fun event ->
                 if Branch_reconcile.repair_event_accepted event then
                   turn_accepted := true;
                 on_event event)),
          "" )
      with exn ->
        if Process_tree.has_cancellation exn then raise exn
        else (None, Stdlib.Printexc.to_string exn)
  in
  let after_head = read_head () in
  let timed_out, final_result, detail =
    match result with
    | None -> (false, false, failure_detail)
    | Some result ->
        (result.Llm_backend.timed_out, result.saw_final_result, result.stderr)
  in
  Branch_reconcile.repair_result ~turn ~turn_accepted:!turn_accepted
    ~at:(now ()) ~before_head ~after_head ~timed_out ~final_result ~detail
