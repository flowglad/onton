open Onton

let write_file path contents =
  let oc = Stdlib.Out_channel.open_text path in
  Fun.protect
    ~finally:(fun () -> Stdlib.Out_channel.close oc)
    (fun () -> Stdlib.Out_channel.output_string oc contents)

let () =
  let root =
    Stdlib.Filename.concat
      (Stdlib.Filename.get_temp_dir_name ())
      (Printf.sprintf "onton-project-store-%d" (Unix.getpid ()))
  in
  let file_path = Stdlib.Filename.concat root "stale.md" in
  let subdir = Stdlib.Filename.concat root "nested" in
  Project_store.ensure_dir subdir;
  write_file file_path "stale";
  Project_store.reset_artifact_dir root;
  assert (not (Stdlib.Sys.file_exists file_path));
  assert (Stdlib.Sys.is_directory subdir);
  Unix.rmdir subdir;
  Unix.rmdir root;
  print_endline "test_project_store: OK"
