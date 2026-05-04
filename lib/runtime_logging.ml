let log_event runtime ?patch_id msg =
  Runtime.update_activity_log runtime (fun log ->
      Activity_log.add_event log
        (Activity_log.Event.create ~timestamp:(Unix.gettimeofday ()) ?patch_id
           msg))

let log_stream_entry runtime ~patch_id kind =
  Runtime.update_activity_log runtime (fun log ->
      Activity_log.add_stream_entry log
        (Activity_log.Stream_entry.create ~timestamp:(Unix.gettimeofday ())
           ~patch_id ~kind))
