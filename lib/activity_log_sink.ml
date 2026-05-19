let sink ~update () =
  let consume = function
    | Telemetry.Event.Free_form { patch_id; message; _ } ->
        update (fun log ->
            Activity_log.add_event log
              (Activity_log.Event.create ~timestamp:0. ?patch_id message))
    | Stream { patch_id; raw; channel; _ } ->
        let kind = Activity_log.stream_kind_of_raw ~channel raw in
        update (fun log ->
            Activity_log.add_stream_entry log
              (Activity_log.Stream_entry.create ~timestamp:0. ~patch_id ~kind))
    | Poll _ | Action _ | Complete _ | Spawn_started _ | Spawn_finalized _ -> ()
  in
  {
    Telemetry.Sink.name = "activity_log";
    interested_in =
      (function
      | Telemetry.Event.Free_form _ | Telemetry.Event.Stream _ -> true
      | Poll _ | Action _ | Complete _ | Spawn_started _ | Spawn_finalized _ ->
          false);
    consume;
  }
