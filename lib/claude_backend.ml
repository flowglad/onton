let create ~process_mgr : Llm_backend.t =
  {
    name = "Claude";
    run_streaming =
      (fun ~cwd ~patch_id ~prompt ~continue ~on_event ->
        Claude_runner.run_streaming ~process_mgr ~cwd ~patch_id ~prompt
          ~continue ~on_event);
  }
