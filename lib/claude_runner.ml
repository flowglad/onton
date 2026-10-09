(* @archlint.module shell
   @archlint.domain json *)

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

let build_args ~getenv_opt ~model ~effort ~complexity ~prompt ~resume_session =
  Claude_event_parser.build_args ~getenv_opt ~warn:warn_to_stderr ~model ~effort
    ~complexity ~prompt ~resume_session

let build_stream_args ~getenv_opt ~model ~effort ~complexity ~prompt
    ~minted_session_id ~resume_session =
  Claude_event_parser.build_stream_args ~getenv_opt ~warn:warn_to_stderr ~model
    ~effort ~complexity ~prompt ~minted_session_id ~resume_session

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

let run_streaming ~model ~effort ~process_mgr ~clock ~timeout ~setsid_exec
    ~project_name ~cwd ~patch_id ~prompt ~resume_session ~session_uuid
    ~complexity ~on_event =
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
        build_stream_args ~getenv_opt:Stdlib.Sys.getenv_opt ~model ~effort
          ~complexity ~prompt ~minted_session_id ~resume_session
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
