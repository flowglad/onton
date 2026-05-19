let log_event _runtime ?patch_id msg =
  Telemetry_dispatch.emit
    (Telemetry.Event.Free_form
       { patch_id; level = Telemetry.Event.Info; message = msg })

let log_stream_entry _runtime ~patch_id kind =
  let raw =
    (match kind with
      | Activity_log.Stream_entry.Tool_use (name, input) ->
          `Assoc
            [
              ("activity_log_kind", `String "tool_use");
              ("name", `String name);
              ("input", `String input);
            ]
      | Activity_log.Stream_entry.Text_chunk text ->
          `Assoc
            [
              ("activity_log_kind", `String "text_chunk"); ("text", `String text);
            ]
      | Activity_log.Stream_entry.Finished reason ->
          `Assoc
            [
              ("activity_log_kind", `String "finished");
              ("reason", `String reason);
            ]
      | Activity_log.Stream_entry.Stream_error message ->
          `Assoc
            [
              ("activity_log_kind", `String "stream_error");
              ("message", `String message);
            ])
    |> Yojson.Safe.to_string
  in
  let channel =
    match kind with
    | Activity_log.Stream_entry.Stream_error _ -> `Stderr
    | Activity_log.Stream_entry.Tool_use _
    | Activity_log.Stream_entry.Text_chunk _
    | Activity_log.Stream_entry.Finished _ ->
        `Stdout
  in
  Telemetry_dispatch.emit
    (Telemetry.Event.Stream { patch_id; session_uuid = ""; channel; raw })
