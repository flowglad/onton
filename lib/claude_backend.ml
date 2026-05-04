let create ~name ~model ~process_mgr ~clock ~timeout ~setsid_exec :
    Llm_backend.t =
  {
    name;
    run_streaming =
      (fun ~cwd ~patch_id ~prompt ~resume_session ~complexity ~on_event ->
        Claude_runner.run_streaming ~model ~process_mgr ~clock ~timeout
          ~setsid_exec ~cwd ~patch_id ~prompt ~resume_session ~complexity
          ~on_event);
  }
