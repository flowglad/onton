(* @archlint.module test
   @archlint.domain llm-backend *)

open Base
open Onton
open Onton_core
open Llm_backend

let expect name condition =
  if not condition then failwith ("idle timeout: " ^ name)

let write_executable path content =
  let channel = Stdlib.open_out_bin path in
  Exn.protect
    ~f:(fun () -> Stdlib.output_string channel content)
    ~finally:(fun () -> Stdlib.close_out channel);
  Unix.chmod path 0o755

let rec remove_tree path =
  if Stdlib.Sys.is_directory path then (
    Stdlib.Sys.readdir path
    |> Array.iter ~f:(fun name ->
        remove_tree (Stdlib.Filename.concat path name));
    Unix.rmdir path)
  else Unix.unlink path

let with_env name value f =
  let previous = Stdlib.Sys.getenv_opt name in
  Unix.putenv name value;
  Exn.protect ~f ~finally:(fun () ->
      match previous with
      | Some value -> Unix.putenv name value
      | None -> Unix.unsetenv name)

(* Exercise the real spawn boundary without relying on slow OS process setup. *)
let delay_spawn (type tag) ~clock ~delay
    (process_mgr : tag Eio.Process.mgr_ty Eio.Resource.t) =
  let (Eio.Resource.T (value, ops)) = process_mgr in
  let module Manager = (val Eio.Resource.get ops Eio.Process.Pi.Mgr) in
  let module Delayed = struct
    include Manager

    let spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
      Eio.Time.sleep clock delay;
      Manager.spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args
  end in
  Eio.Resource.T (value, Eio.Process.Pi.mgr (module Delayed))

let () =
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let cwd = Eio.Stdenv.cwd env in
  let setsid_exec = Some (Stdlib.Sys.getenv "ONTON_SETSID_EXEC") in
  let root = Stdlib.Filename.temp_file "onton-backend-idle-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  Exn.protect
    ~finally:(fun () -> remove_tree root)
    ~f:(fun () ->
      let patch_id = Types.Patch_id.of_string "idle-timeout" in
      let run ?(process_mgr = process_mgr) ?(timeout = 0.5)
          ?(process_line = fun _ -> []) ?(on_event = fun _ -> ()) script =
        let start = Eio.Time.now clock in
        let result =
          Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout ~cwd
            ~env:(Unix.environment ()) ~setsid_exec
            ~args:[ "/bin/sh"; "-c"; script ]
            ~session_uuid:None ~patch_id ~process_line ~on_event
        in
        (result, Eio.Time.now clock -. start)
      in
      let silent, elapsed = run "exec /bin/sleep 30" in
      expect "initial silence times out" silent.timed_out;
      expect "initial silence is bounded" Float.(elapsed < 3.0);
      let delayed_mgr = delay_spawn ~clock ~delay:0.7 process_mgr in
      let delayed_success, elapsed =
        run ~process_mgr:delayed_mgr "printf 'spawned\n'"
      in
      expect "spawn latency does not consume the idle window"
        ((not delayed_success.timed_out) && delayed_success.exit_code = 0);
      expect "delayed spawn really exceeds the idle window"
        Float.(elapsed >= 0.7);
      let delayed_silent, elapsed =
        run ~process_mgr:delayed_mgr "exec /bin/sleep 30"
      in
      expect "silence after delayed spawn times out" delayed_silent.timed_out;
      expect "delayed child receives the full first idle window"
        Float.(elapsed >= 1.15);
      expect "delayed-spawn timeout remains bounded" Float.(elapsed < 3.0);
      let active_then_silent, elapsed =
        run
          "printf 'first\n\
           '; /bin/sleep 0.3; printf 'second\n\
           '; exec /bin/sleep 30"
      in
      expect "silence after output times out" active_then_silent.timed_out;
      expect "last output renews the full idle window" Float.(elapsed >= 0.72);
      expect "renewed silence is bounded" Float.(elapsed < 3.0);
      let pulse =
        "i=0; while [ $i -lt 8 ]; do printf x; /bin/sleep 0.1; i=$((i + 1)); \
         done"
      in
      let partial_stdout, elapsed = run pulse in
      expect "partial stdout keeps the process alive"
        ((not partial_stdout.timed_out) && partial_stdout.exit_code = 0);
      expect "partial stdout exceeds the original deadline"
        Float.(elapsed >= 0.7);
      expect "partial stdout is captured at EOF"
        (String.equal partial_stdout.stdout "xxxxxxxx\n");
      let partial_stderr, elapsed = run ("exec 1>&-; " ^ pulse ^ " >&2") in
      expect "stderr activity after stdout EOF keeps the process alive"
        ((not partial_stderr.timed_out) && partial_stderr.exit_code = 0);
      expect "stderr activity exceeds the original deadline"
        Float.(elapsed >= 0.7);
      expect "partial stderr is captured at EOF"
        (String.equal partial_stderr.stderr "xxxxxxxx");
      let closed_pipes, elapsed = run "exec 1>&- 2>&-; exec /bin/sleep 30" in
      expect "closed pipes do not leave a live child waiting"
        closed_pipes.timed_out;
      expect "closed-pipe wait is bounded" Float.(elapsed < 3.0);
      let blocked_callback, elapsed =
        run "printf 'event\n'; exec /bin/sleep 30"
          ~process_line:(fun _ -> [ Types.Stream_event.Text_delta "event" ])
          ~on_event:(fun _ -> Eio.Time.sleep clock 30.0)
      in
      expect "blocked callbacks remain bounded" blocked_callback.timed_out;
      expect "callback timeout is bounded" Float.(elapsed < 3.0);
      let teardown_error = Failure "callback teardown failed" in
      let nested_error =
        Eio.Exn.Multiple
          [
            (teardown_error, Stdlib.Printexc.get_callstack 0);
            (Eio.Buf_read.Buffer_limit_exceeded, Stdlib.Printexc.get_callstack 0);
          ]
      in
      List.iter
        [
          ("single failure", teardown_error); ("nested failures", nested_error);
        ]
        ~f:(fun (name, error) ->
          let callback_started = ref false in
          let raced, elapsed =
            run "printf 'event\n'; exec /bin/sleep 30"
              ~process_line:(fun _ -> [ Types.Stream_event.Text_delta "event" ])
              ~on_event:(fun _ ->
                callback_started := true;
                Exn.protect
                  ~f:(fun () -> Eio.Time.sleep clock 30.0)
                  ~finally:(fun () -> raise error))
          in
          expect
            (name ^ " races during callback cancellation")
            !callback_started;
          expect (name ^ " preserves timeout classification") raced.timed_out;
          expect (name ^ " timeout remains bounded") Float.(elapsed < 3.0);
          let propagated =
            try
              ignore
                (run "printf 'event\n'; exec /bin/sleep 30"
                   ~process_line:(fun _ -> raise error));
              false
            with exn -> phys_equal exn error
          in
          expect
            (name ^ " propagates unchanged without idle timeout")
            propagated);
      let cancelled =
        Eio.Time.with_timeout clock 0.1 (fun () ->
            Ok (run ~timeout:5.0 "exec /bin/sleep 30"))
      in
      expect "external cancellation propagates"
        (match cancelled with Error `Timeout -> true | Ok _ -> false);
      let backends =
        [
          ( "claude",
            {|{"type":"content_block_delta","delta":{"type":"text_delta","text":"hello"}}|},
            {|{"type":"result","result":"done","stop_reason":"end_turn"}|} );
          ( "codex",
            {|{"type":"item.completed","item":{"type":"agent_message","text":"hello"}}|},
            {|{"type":"turn.completed"}|} );
          ( "opencode",
            {|{"type":"text","part":{"type":"text","text":"hello"}}|},
            {|{"type":"step_finish","part":{"type":"step-finish","reason":"stop"}}|}
          );
          ( "pi",
            {|{"type":"message_update","assistantMessageEvent":{"type":"text_delta","delta":"hello"}}|},
            {|{"type":"agent_end","messages":[]}|} );
          ( "gemini",
            {|{"type":"message","role":"assistant","delta":true,"content":"hello"}|},
            {|{"type":"result","status":"success"}|} );
        ]
      in
      List.iter backends ~f:(fun (name, activity, terminal) ->
          write_executable
            (Stdlib.Filename.concat root name)
            (Printf.sprintf
               "#!/bin/sh\n\
                i=0\n\
                while [ $i -lt 8 ]; do\n\
               \  printf '%%s\\n' %s\n\
               \  /bin/sleep 0.1\n\
               \  i=$((i + 1))\n\
                done\n\
                printf '%%s\\n' %s\n"
               (Stdlib.Filename.quote activity)
               (Stdlib.Filename.quote terminal)));
      let path =
        root ^ ":" ^ Option.value (Stdlib.Sys.getenv_opt "PATH") ~default:""
      in
      with_env "PATH" path @@ fun () ->
      with_env "ONTON_DATA_DIR" (Stdlib.Filename.concat root "data")
      @@ fun () ->
      let registry =
        Backend_registry.create ~process_mgr ~clock ~timeout:0.5 ~setsid_exec
          ~extras:[]
      in
      List.iter backends ~f:(fun (name, _, _) ->
          let backend =
            Backend_registry.get registry ~backend:name ~model:None ~effort:None
          in
          let events = ref [] in
          let start = Eio.Time.now clock in
          let result =
            backend.run_streaming ~project_name:"idle-timeout" ~cwd ~patch_id
              ~prompt:"work" ~resume_session:None ~session_uuid:("idle-" ^ name)
              ~complexity:None ~on_event:(fun event ->
                events := event :: !events)
          in
          expect
            (name ^ " output extends the session")
            ((not result.timed_out) && result.saw_final_result);
          expect
            (name ^ " runs past the original deadline")
            Float.(Eio.Time.now clock -. start >= 0.7);
          expect
            (name ^ " forwards streamed activity")
            (List.count !events ~f:(function
               | Types.Stream_event.Text_delta "hello" -> true
               | Types.Stream_event.Text_delta _
               | Types.Stream_event.Turn_started | Types.Stream_event.Tool_use _
               | Types.Stream_event.Final_result _ | Types.Stream_event.Error _
               | Types.Stream_event.Session_init _ ->
                   false)
            = 8)));
  Stdio.print_endline "backend idle timeout: passed"
