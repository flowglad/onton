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

let write_executable path contents =
  let oc = Stdlib.open_out_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out_noerr oc)
    (fun () -> Stdlib.output_string oc contents);
  Unix.chmod path 0o755

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

let run_fake_agent ?(on_event = fun _ _ -> ()) ?(prompt = "go") ~script ~timeout
    () =
  Eio_main.run @@ fun env ->
  let tmp = mktempdir "onton-patch-agent-idle-timeout" in
  Stdlib.Fun.protect
    ~finally:(fun () -> rm_rf tmp)
    (fun () ->
      let binary_path = Stdlib.Filename.concat tmp "fake-patch-agent" in
      write_executable binary_path script;
      let clock = Eio.Stdenv.clock env in
      let backend =
        Patch_agent_backend.create
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock ~timeout ~binary_path ~setsid_exec:None
      in
      Eio.Switch.run (fun sw ->
          let (Llm_backend_long_lived.T
                 { start; prompt = prompt_backend; shutdown; _ }) =
            backend
          in
          let handle =
            start ~sw
              {
                Llm_backend_long_lived.project_name = "onton";
                worktree = Eio.Path.(Eio.Stdenv.fs env / tmp);
                patch_id = Types.Patch_id.of_string "patch-idle-timeout";
                provider = "test";
                model = "test";
                effort = "test";
                gameplan_prompt = "gameplan";
                patch_prompt = "patch";
              }
          in
          Stdlib.Fun.protect
            ~finally:(fun () -> shutdown handle)
            (fun () ->
              let started = Eio.Time.now clock in
              let result =
                prompt_backend handle ~prompt ~timeout
                  ~on_event:(on_event clock)
              in
              (result, Eio.Time.now clock -. started))))

let idle_timeout_test =
  QCheck2.Test.make
    ~name:"Patch_agent_backend_integration > activity extends the turn" ~count:1
    QCheck2.Gen.unit (fun () ->
      try
        let result, elapsed =
          run_fake_agent ~timeout:1.0
            ~script:
              {|#!/bin/sh
IFS= read -r prompt || exit 1
printf '%s\n' '{"type":"turn_started","turn_index":0}'
sleep 0.3
printf '%s\n' '{"type":"text_delta","delta":"one"}'
sleep 0.3
printf '%s\n' '{"type":"text_delta","delta":"two"}'
sleep 0.3
printf '%s\n' '{"type":"text_delta","delta":"three"}'
sleep 0.3
printf '%s\n' '{"type":"text_delta","delta":"four"}'
sleep 0.3
printf '%s\n' '{"type":"done","stop_reason":"stop","final_text":"done"}'
|}
            ()
        in
        Float.(elapsed > 1.0)
        && (not result.Llm_backend.timed_out)
        && result.Llm_backend.saw_final_result
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> false)

let silence_timeout_test =
  QCheck2.Test.make ~name:"Patch_agent_backend_integration > silence times out"
    ~count:1 QCheck2.Gen.unit (fun () ->
      try
        let result, elapsed =
          run_fake_agent ~timeout:0.3
            ~script:
              {|#!/bin/sh
IFS= read -r prompt || exit 1
printf '%s\n' '{"type":"turn_started","turn_index":0}'
exec sleep 5
|}
            ()
        in
        result.Llm_backend.timed_out && result.Llm_backend.got_events
        && (not result.Llm_backend.saw_final_result)
        && Float.(elapsed < 3.0)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> false)

let write_and_first_read_share_deadline_test =
  QCheck2.Test.make
    ~name:
      "Patch_agent_backend_integration > write and first read share deadline"
    ~count:1 QCheck2.Gen.unit (fun () ->
      try
        let result, _elapsed =
          run_fake_agent ~timeout:1.0
            ~prompt:(String.make (128 * 1024) 'x')
            ~script:
              {|#!/bin/sh
sleep 0.5
exec python3 -u -c 'import sys, time; sys.stdin.readline(); time.sleep(0.7); print("{\"type\":\"turn_started\",\"turn_index\":0}", flush=True); time.sleep(5)'
|}
            ()
        in
        result.Llm_backend.timed_out && not result.Llm_backend.got_events
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> false)

let closed_stdout_timeout_test =
  QCheck2.Test.make
    ~name:
      "Patch_agent_backend_integration > closed stdout with live child times \
       out"
    ~count:1 QCheck2.Gen.unit (fun () ->
      try
        let result, elapsed =
          run_fake_agent ~timeout:0.3
            ~script:
              {|#!/bin/sh
IFS= read -r prompt || exit 1
printf '%s\n' '{"type":"turn_started","turn_index":0}'
exec sleep 5 >&-
|}
            ()
        in
        result.Llm_backend.timed_out && result.Llm_backend.got_events
        && (not result.Llm_backend.saw_final_result)
        && Float.(elapsed < 3.0)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> false)

let blocked_callback_timeout_test =
  QCheck2.Test.make
    ~name:"Patch_agent_backend_integration > blocked callback times out"
    ~count:1 QCheck2.Gen.unit (fun () ->
      try
        let result, elapsed =
          run_fake_agent ~timeout:0.3
            ~on_event:(fun clock _ -> Eio.Time.sleep clock 5.0)
            ~script:
              {|#!/bin/sh
IFS= read -r prompt || exit 1
printf '%s\n' '{"type":"turn_started","turn_index":0}'
exec sleep 5
|}
            ()
        in
        result.Llm_backend.timed_out && result.Llm_backend.got_events
        && Float.(elapsed < 3.0)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> false)

let () =
  let exit_code =
    QCheck_base_runner.run_tests ~verbose:true
      [
        smoke_test;
        idle_timeout_test;
        silence_timeout_test;
        write_and_first_read_share_deadline_test;
        closed_stdout_timeout_test;
        blocked_callback_timeout_test;
      ]
  in
  if exit_code <> 0 then Stdlib.exit exit_code
