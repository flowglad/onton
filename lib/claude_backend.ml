let create ~process_mgr ~clock ~timeout : Llm_backend.t =
  {
    name = "Claude";
    run_streaming =
      (fun ~cwd ~patch_id ~prompt ~continue ~on_event ->
        Claude_runner.run_streaming ~process_mgr ~clock ~timeout ~cwd ~patch_id
          ~prompt ~continue ~on_event);
  }
