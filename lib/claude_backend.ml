let create ~process_mgr ~clock ~timeout ~setsid_exec : Llm_backend.t =
  {
    name = "Claude";
    run_streaming =
      (fun ~cwd ~patch_id ~prompt ~resume_session ~on_event ->
        Claude_runner.run_streaming ~process_mgr ~clock ~timeout ~setsid_exec
          ~cwd ~patch_id ~prompt ~resume_session ~on_event);
  }
