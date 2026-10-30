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
      let entrypoint = Filename.concat root "custom.ts" in
      write entrypoint "export default function () {}";
      with_env "PATH"
        (root ^ ":" ^ Sys.getenv "PATH")
        (fun () ->
          with_env "ONTON_DATA_DIR" (Filename.concat root "data") (fun () ->
              with_env "ONTON_PI_EXTENSION" entrypoint (fun () ->
                  Eio_main.run @@ fun env ->
                  let backend =
                    Pi_backend.create ~model:None ~effort:None
                      ~process_mgr:(Eio.Stdenv.process_mgr env)
                      ~clock:(Eio.Stdenv.clock env) ~timeout:10.
                      ~setsid_exec:None
                  in
                  with_env "ONTON_PI_EXTENSION" "relative/index.ts" (fun () ->
                      let rejected =
                        try
                          ignore
                            (backend.run_streaming ~project_name:"Pi context"
                               ~cwd:(Eio.Stdenv.cwd env)
                               ~patch_id:
                                 (Types.Patch_id.of_string "invalid-patch")
                               ~prompt:"Invalid override" ~resume_session:None
                               ~session_uuid:"invalid" ~complexity:None
                               ~on_event:(fun _ -> ()));
                          false
                        with Failure message ->
                          String.equal message
                            "ONTON_PI_EXTENSION must be an absolute entrypoint"
                      in
                      assert rejected;
                      assert (not (Sys.file_exists argv)));
                  List.iter
                    (fun (session_uuid, resume_session, prompt) ->
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
                      assert (Sys.is_directory (field "artifact_dir"));
                      (* A fresh session can immediately save either response. *)
                      write
                        (Filename.concat (field "artifact_dir") "pr-body.md")
                        "Notes";
                      write
                        (Filename.concat (field "artifact_dir") "WONTDO.md")
                        "Reason";
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
                      assert (
                        String.starts_with
                          ~prefix:("-p\n" ^ prompt ^ "\n--mode\njson\n")
                          args);
                      let arguments = String.split_on_char '\n' args in
                      (match resume_session with
                      | None -> assert (not (List.mem "--session" arguments))
                      | Some session ->
                          let rec has_session = function
                            | "--session" :: value :: _ ->
                                String.equal value session
                            | _ :: rest -> has_session rest
                            | [] -> false
                          in
                          assert (has_session arguments));
                      assert (
                        String.ends_with
                          ~suffix:
                            ("--extension\n" ^ Unix.realpath entrypoint ^ "\n")
                          args))
                    [
                      ("fresh", None, "Implement this patch");
                      ( "resumed",
                        Some "conversation",
                        "Human: answer this question" );
                    ]))));
  print_endline "Pi fresh/resumed delivery handoff: OK"

let () =
  let root = Filename.temp_dir "onton-pi-install-" "" in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote root)))
    (fun () ->
      let install prefix ~flat =
        let directory = if flat then prefix else Filename.concat prefix "bin" in
        Project_store.ensure_dir directory;
        let executable = Filename.concat directory "onton" in
        write executable "#!/bin/sh\nexit 0\n";
        Unix.chmod executable 0o700;
        let assets = Filename.concat prefix "share/onton/pi-onton" in
        Project_store.ensure_dir assets;
        List.iter
          (fun name -> write (Filename.concat assets name) "extension source")
          [ "index.ts"; "protocol.ts"; "files.ts" ];
        (executable, Filename.concat assets "index.ts")
      in
      let check executable expected =
        assert (Pi_backend.extension_path ~executable = Unix.realpath expected)
      in
      with_env "ONTON_PI_EXTENSION" "" (fun () ->
          let executable, entrypoint =
            install (Filename.concat root "keg") ~flat:false
          in
          check executable entrypoint;
          let linked_bin = Filename.concat root "linked-bin" in
          Project_store.ensure_dir linked_bin;
          let symlink = Filename.concat linked_bin "onton" in
          Unix.symlink executable symlink;
          check symlink entrypoint;
          with_env "PATH" linked_bin (fun () -> check "onton" entrypoint);
          let flat, flat_entrypoint =
            install (Filename.concat root "archive") ~flat:true
          in
          check flat flat_entrypoint;
          let support =
            Filename.concat (Filename.dirname entrypoint) "protocol.ts"
          in
          Unix.unlink support;
          (match Pi_backend.extension_path ~executable with
          | _ -> failwith "Incomplete installation unexpectedly accepted"
          | exception Failure message ->
              assert (
                String.starts_with ~prefix:"Pi extension is missing" message));
          let custom = Filename.concat root "custom.ts" in
          write custom "export default function () {}";
          with_env "ONTON_PI_EXTENSION" custom (fun () ->
              check "/missing/executable" custom);
          with_env "ONTON_PI_EXTENSION" (Filename.concat root "missing.ts")
            (fun () ->
              match Pi_backend.extension_path ~executable with
              | _ -> failwith "Missing override unexpectedly accepted"
              | exception Failure message ->
                  assert (
                    String.starts_with ~prefix:"Pi extension not found" message))));
  print_endline "Pi installed extension discovery: OK"
