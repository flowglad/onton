(* @archlint.module test
   @archlint.domain patch-agent *)

open Base
open Onton
open Onton_core

let command_available name =
  match
    Stdlib.Sys.command
      (Printf.sprintf "command -v %s >/dev/null 2>&1"
         (Stdlib.Filename.quote name))
  with
  | 0 -> true
  | _ -> false

let read_file path =
  let ic = Stdlib.open_in_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in_noerr ic)
    (fun () ->
      let len = Stdlib.in_channel_length ic in
      Stdlib.really_input_string ic len)

let mktempdir prefix =
  let base = Stdlib.Filename.get_temp_dir_name () in
  let rec loop n =
    let dir =
      Stdlib.Filename.concat base
        (Printf.sprintf "%s-%d-%d" prefix (Unix.getpid ()) n)
    in
    try
      Unix.mkdir dir 0o755;
      dir
    with Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n + 1)
  in
  loop 0

let rm_rf dir =
  ignore
    (Stdlib.Sys.command
       (Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote dir)))

let has_text_delta events =
  List.exists events ~f:(function
    | Types.Stream_event.Text_delta _ -> true
    | Types.Stream_event.Turn_started | Types.Stream_event.Tool_use _
    | Types.Stream_event.Final_result _ | Types.Stream_event.Error _
    | Types.Stream_event.Session_init _ ->
        false)

let is_session_init = function
  | Types.Stream_event.Session_init _ -> true
  | Types.Stream_event.Turn_started | Types.Stream_event.Text_delta _
  | Types.Stream_event.Tool_use _ | Types.Stream_event.Final_result _
  | Types.Stream_event.Error _ ->
      false

let is_turn_started = function
  | Types.Stream_event.Turn_started -> true
  | Types.Stream_event.Text_delta _ | Types.Stream_event.Tool_use _
  | Types.Stream_event.Final_result _ | Types.Stream_event.Error _
  | Types.Stream_event.Session_init _ ->
      false

let is_final_result = function
  | Types.Stream_event.Final_result _ -> true
  | Types.Stream_event.Turn_started | Types.Stream_event.Text_delta _
  | Types.Stream_event.Tool_use _ | Types.Stream_event.Error _
  | Types.Stream_event.Session_init _ ->
      false

let smoke_test =
  QCheck2.Test.make
    ~name:"Patch_agent_backend_integration > one-turn smoke against real binary"
    ~count:1 QCheck2.Gen.unit (fun () ->
      if not (command_available "patch-agent") then (
        Stdlib.prerr_endline
          "SKIP: patch-agent binary not found on PATH; integration smoke not \
           run";
        true)
      else
        try
          Eio_main.run @@ fun env ->
          let fixture_dir = "fixtures/patch_agent_integration" in
          let gameplan_prompt =
            read_file (Stdlib.Filename.concat fixture_dir "gameplan.md")
          in
          let patch_prompt =
            read_file (Stdlib.Filename.concat fixture_dir "patch.md")
          in
          let tmp = mktempdir "onton-patch-agent-integration" in
          Stdlib.Fun.protect
            ~finally:(fun () -> rm_rf tmp)
            (fun () ->
              let process_mgr = Eio.Stdenv.process_mgr env in
              let clock = Eio.Stdenv.clock env in
              let fs = Eio.Stdenv.fs env in
              let backend =
                Patch_agent_backend.create ~process_mgr ~clock ~timeout:60.0
                  ~binary_path:"patch-agent" ~setsid_exec:None
              in
              let events = ref [] in
              let passed = ref false in
              Eio.Switch.run (fun sw ->
                  let (Llm_backend_long_lived.T
                         { start; prompt = prompt_backend; shutdown; _ }) =
                    backend
                  in
                  let worktree = Eio.Path.(fs / tmp) in
                  let handle =
                    start ~sw
                      {
                        Llm_backend_long_lived.project_name = "onton";
                        worktree;
                        patch_id = Types.Patch_id.of_string "patch-6-smoke";
                        provider = "anthropic";
                        model = "claude-sonnet-4-5";
                        effort = "medium";
                        gameplan_prompt;
                        patch_prompt;
                      }
                  in
                  let shutdown_err = ref None in
                  let result =
                    Stdlib.Fun.protect
                      ~finally:(fun () ->
                        try shutdown handle with
                        | Eio.Cancel.Cancelled _ as exn -> raise exn
                        | exn -> shutdown_err := Some exn)
                      (fun () ->
                        prompt_backend handle
                          ~prompt:
                            "Run a smoke turn for the integration fixture. \
                             Reply briefly and do not edit files." ~timeout:60.0
                          ~on_event:(fun event -> events := event :: !events))
                  in
                  let events = List.rev !events in
                  passed :=
                    result.Llm_backend.got_events
                    && result.Llm_backend.saw_final_result
                    && (not result.Llm_backend.timed_out)
                    && Option.value_map (List.hd events) ~default:false
                         ~f:is_session_init
                    && (match events with
                      | _session_init :: turn_started :: _ ->
                          is_turn_started turn_started
                      | _ -> false)
                    && has_text_delta events
                    && List.exists events ~f:is_final_result
                    && Option.is_none !shutdown_err);
              !passed)
        with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | _ -> false)

let () =
  let exit_code = QCheck_base_runner.run_tests ~verbose:true [ smoke_test ] in
  if exit_code <> 0 then Stdlib.exit exit_code
