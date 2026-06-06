(* @archlint.module shell
   @archlint.domain pi-event-parser *)

open Base

(* Pure parsing, ANSI scrubbing, and CLI-arg construction live in
   [Claude_event_parser] (lib_core/). This file is the effectful handler:
   subprocess spawning via Eio, environment probing for minted session IDs,
   and the [Persistence.record_session_id] hop that ties a fresh session to
   the snapshot file. *)

let strip_ansi = Claude_event_parser.strip_ansi
let auto_model = Claude_event_parser.auto_model
let parse_stream_event = Claude_event_parser.parse_stream_event
let parse_stream_events = Claude_event_parser.parse_stream_events
let warn_to_stderr msg = Stdio.eprintf "%s\n" msg

let build_args ~getenv_opt ~model ~complexity ~prompt ~resume_session =
  Claude_event_parser.build_args ~getenv_opt ~warn:warn_to_stderr ~model
    ~complexity ~prompt ~resume_session

let build_stream_args ~getenv_opt ~model ~complexity ~prompt ~minted_session_id
    ~resume_session =
  Claude_event_parser.build_stream_args ~getenv_opt ~warn:warn_to_stderr ~model
    ~complexity ~prompt ~minted_session_id ~resume_session

let prepare_minted_session_id_with_env ~getenv_opt ~patch_id ~resume_session =
  ignore (patch_id : Types.Patch_id.t);
  (* Mint a session id when [ONTON_MINTED_SESSION_IDS=1] and there is no
     resume id to fall back to.  The sidecar that lets a crashed supervisor
     resume an in-flight session is written *deferred* by the stream-event
     loop in [session_driver.ml] — only after the first [Text_delta] or
     [Tool_use] proves claude has actually written a real conversation
     turn.  Persisting eagerly here used to poison every later retry that
     landed on a stub [.jsonl] (claude exits before content → sidecar
     restored on startup → [--resume <stub>] → "No conversation found"). *)
  match (resume_session, getenv_opt "ONTON_MINTED_SESSION_IDS") with
  | Some _, _ | _, None | _, Some "" -> Ok None
  | None, Some "1" -> Ok (Some (Session_id.mint ()))
  | None, Some _ -> Ok None

let prepare_minted_session_id =
  prepare_minted_session_id_with_env ~getenv_opt:Stdlib.Sys.getenv_opt

let run ~model ~process_mgr ~cwd ~patch_id ~prompt ~resume_session ~complexity =
  ignore (patch_id : Types.Patch_id.t);
  let args =
    build_args ~getenv_opt:Stdlib.Sys.getenv_opt ~model ~complexity ~prompt
      ~resume_session
  in
  let stdout_content, stderr_content, exit_code =
    Eio.Switch.run @@ fun sw ->
    let stdin_r, stdin_w = Eio.Process.pipe ~sw process_mgr in
    let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
    let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
    let child =
      Eio.Process.spawn ~sw process_mgr ~cwd ~stdin:stdin_r ~stdout:stdout_w
        ~stderr:stderr_w args
    in
    Eio.Switch.on_release sw (fun () ->
        try Eio.Process.signal child Stdlib.Sys.sigterm with _ -> ());
    Eio.Flow.close stdin_r;
    Eio.Flow.close stdin_w;
    Eio.Flow.close stdout_w;
    Eio.Flow.close stderr_w;
    let stdout_buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdout_r in
    let stderr_buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stderr_r in
    let drain flow =
      let buf = Bytes.create 4096 in
      try
        while true do
          ignore (Eio.Flow.single_read flow (Cstruct.of_bytes buf))
        done
      with End_of_file -> ()
    in
    let out, err =
      Eio.Fiber.pair
        (fun () ->
          try Eio.Buf_read.take_all stdout_buf
          with Eio.Buf_read.Buffer_limit_exceeded ->
            drain stdout_r;
            "<stdout exceeded 1MB limit, truncated>")
        (fun () ->
          try Eio.Buf_read.take_all stderr_buf
          with Eio.Buf_read.Buffer_limit_exceeded ->
            drain stderr_r;
            "<stderr exceeded 1MB limit, truncated>")
    in
    let status = Eio.Process.await child in
    let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
    (out, err, code)
  in
  let cleaned_stdout = strip_ansi stdout_content in
  let got_events = not (String.is_empty (String.strip cleaned_stdout)) in
  {
    Llm_backend.exit_code;
    stdout = cleaned_stdout;
    stderr = stderr_content;
    got_events;
    saw_final_result = false;
    timed_out = false;
  }

let run_streaming ~model ~process_mgr ~clock ~timeout ~setsid_exec ~project_name
    ~cwd ~patch_id ~prompt ~resume_session ~session_uuid ~complexity ~on_event =
  let model = Llm_backend.resolve_auto_model ~model ~complexity ~auto_model in
  let env =
    Spawn_env.merge_env ~base_env:(Unix.environment ())
      ~overrides:(Spawn_env.per_patch_env ~project_name ~patch_id)
  in
  match prepare_minted_session_id ~patch_id ~resume_session with
  | Error msg ->
      {
        Llm_backend.exit_code = 1;
        stdout = "";
        stderr = msg;
        got_events = false;
        saw_final_result = false;
        timed_out = false;
      }
  | Ok minted_session_id ->
      let args =
        build_stream_args ~getenv_opt:Stdlib.Sys.getenv_opt ~model ~complexity
          ~prompt ~minted_session_id ~resume_session
      in
      let process_line line =
        let trimmed = strip_ansi (String.strip line) in
        if String.is_empty trimmed then [] else parse_stream_events trimmed
      in
      Llm_backend.emit_spawn_started ~patch_id ~session_uuid ~prompt ~args ~env;
      Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~env
        ~setsid_exec ~args ~session_uuid:(Some session_uuid) ~patch_id
        ~process_line ~on_event

let%test
    "prepare_minted_session_id mints when flag is on (no snapshot path \
     required)" =
  let patch_id = Types.Patch_id.of_string "5" in
  let getenv_opt = function
    | "ONTON_MINTED_SESSION_IDS" -> Some "1"
    | _ -> None
  in
  match
    prepare_minted_session_id_with_env ~getenv_opt ~patch_id
      ~resume_session:None
  with
  | Ok (Some _) -> true
  | Ok None -> false
  | Error _ -> false

let%test "prepare_minted_session_id returns None when flag is off" =
  let patch_id = Types.Patch_id.of_string "5" in
  let getenv_opt _ = None in
  match
    prepare_minted_session_id_with_env ~getenv_opt ~patch_id
      ~resume_session:None
  with
  | Ok None -> true
  | Ok (Some _) -> false
  | Error _ -> false

let%test "prepare_minted_session_id returns None when resume_session is set" =
  let patch_id = Types.Patch_id.of_string "5" in
  let getenv_opt = function
    | "ONTON_MINTED_SESSION_IDS" -> Some "1"
    | _ -> None
  in
  match
    prepare_minted_session_id_with_env ~getenv_opt ~patch_id
      ~resume_session:(Some "existing-id")
  with
  | Ok None -> true
  | Ok (Some _) -> false
  | Error _ -> false
