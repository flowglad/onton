(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)

let run ~backend ~cwd ~project_name ~patch_id ~complexity ~turn ~read_head ~now
    =
  let read_head () =
    try Option.bind (read_head ()) Branch_reconcile.Commit.make
    with exn -> if Process_tree.has_cancellation exn then raise exn else None
  in
  let before_head = read_head () in
  let result =
    if not (Branch_reconcile.repair_head_matches turn before_head) then None
    else
      try
        Some
          (backend.Llm_backend.run_streaming ~project_name ~cwd ~patch_id
             ~prompt:turn.Branch_reconcile.prompt ~resume_session:None
             ~session_uuid:(Session_id.mint ()) ~complexity ~on_event:(fun _ ->
               ()))
      with exn ->
        if Process_tree.has_cancellation exn then raise exn else None
  in
  let after_head = read_head () in
  let timed_out, final_result, detail =
    match result with
    | None -> (false, false, "repair_backend_or_head_probe_unavailable")
    | Some result ->
        (result.Llm_backend.timed_out, result.saw_final_result, result.stderr)
  in
  Branch_reconcile.repair_result ~turn ~at:(now ()) ~before_head ~after_head
    ~timed_out ~final_result ~detail
