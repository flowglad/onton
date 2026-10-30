(* @archlint.module test
   @archlint.domain pi-event-parser *)

open Onton
open Onton_core
open Llm_backend

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some value -> Unix.putenv name value
      | None -> Unix.unsetenv name)
    f

let write path content =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel content)

let () =
  let root = Filename.temp_dir "onton-pi-context-" "" in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote root)))
    (fun () ->
      let executable = Filename.concat root "pi" in
      let captured = Filename.concat root "delivery-path" in
      let argv = Filename.concat root "argv" in
      write executable
        ("#!/bin/sh\n" ^ "printf '%s' \"$ONTON_PI_CONTEXT\" > "
       ^ Filename.quote captured ^ "\nprintf '%s\\n' \"$@\" > "
       ^ Filename.quote argv
       ^ "\nprintf '%s\\n' '{\"type\":\"agent_end\",\"messages\":[]}'\n");
      Unix.chmod executable 0o700;
      with_env "PATH"
        (root ^ ":" ^ Sys.getenv "PATH")
        (fun () ->
          with_env "ONTON_DATA_DIR" (Filename.concat root "data") (fun () ->
              with_env "ONTON_PI_EXTENSION" "/extension/index.ts" (fun () ->
                  Eio_main.run @@ fun env ->
                  let backend =
                    Pi_backend.create ~model:None ~effort:None
                      ~process_mgr:(Eio.Stdenv.process_mgr env)
                      ~clock:(Eio.Stdenv.clock env) ~timeout:10.
                      ~setsid_exec:None
                  in
                  List.iter
                    (fun (session_uuid, resume_session, prompt) ->
                      if String.equal session_uuid "bundled" then
                        Unix.unsetenv "ONTON_PI_EXTENSION";
                      let result =
                        backend.run_streaming ~project_name:"Pi context"
                          ~cwd:(Eio.Stdenv.cwd env)
                          ~patch_id:(Types.Patch_id.of_string "patch-1")
                          ~prompt ~resume_session ~session_uuid ~complexity:None
                          ~on_event:(fun _ -> ())
                      in
                      assert result.saw_final_result;
                      assert (not result.timed_out);
                      let path =
                        In_channel.with_open_bin captured In_channel.input_all
                      in
                      let json = Yojson.Safe.from_file path in
                      let field key =
                        Yojson.Safe.Util.(json |> member key |> to_string)
                      in
                      assert (field "prompt" = prompt);
                      assert (field "session_uuid" = session_uuid);
                      assert (field "patch_id" = "patch-1");
                      assert (field "project" = "Pi context");
                      assert (
                        Yojson.Safe.Util.(json |> member "version" |> to_int)
                        = 1);
                      assert (
                        field "artifact_dir"
                        = Project_store.artifact_dir ~project_name:"Pi context"
                            ~patch_id:(Types.Patch_id.of_string "patch-1"));
                      let args =
                        In_channel.with_open_bin argv In_channel.input_all
                      in
                      if String.equal session_uuid "bundled" then (
                        let directory =
                          Filename.concat (Filename.dirname path) "pi-onton"
                        in
                        let entrypoint = Filename.concat directory "index.ts" in
                        assert (Sys.file_exists entrypoint);
                        assert (
                          Sys.file_exists
                            (Filename.concat directory "protocol.ts"));
                        assert (
                          Sys.file_exists (Filename.concat directory "files.ts"));
                        assert (
                          String.ends_with
                            ~suffix:("--extension\n" ^ entrypoint ^ "\n")
                            args))
                      else
                        assert (
                          String.ends_with
                            ~suffix:"--extension\n/extension/index.ts\n" args))
                    [
                      ("fresh", None, "Implement this patch");
                      ( "resumed",
                        Some "conversation",
                        "Human: answer this question" );
                      ( "bundled",
                        Some "conversation",
                        "Fix CI with bundled mode" );
                    ]))));
  print_endline "Pi fresh/resumed delivery handoff: OK"
