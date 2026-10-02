(* @archlint.module test
   @archlint.domain project-store *)

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

let () =
  let root = Filename.temp_file "onton-gameplan-publication-" "" in
  Sys.remove root;
  Project_store.ensure_dir root;
  let old_data_dir = Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "ONTON_DATA_DIR" root;
  let project_name = "publication" in
  let dir = Project_store.project_dir project_name in
  let artifact = Project_store.gameplan_artifact_path project_name in
  let artifacts = Filename.dirname artifact in
  let read_file path = In_channel.with_open_bin path In_channel.input_all in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "ONTON_DATA_DIR" (Option.value old_data_dir ~default:"");
      if Sys.file_exists artifacts then Unix.chmod artifacts 0o755;
      let rec remove path =
        if Sys.is_directory path then (
          Array.iter
            (fun name -> remove (Filename.concat path name))
            (Sys.readdir path);
          Unix.rmdir path)
        else Sys.remove path
      in
      remove root)
    (fun () ->
      let source = Filename.concat root "source.yaml" in
      write_file source "value: before\n";
      Project_store.ensure_dir (Project_store.gameplan_path project_name);
      Project_store.save_gameplan_source ~project_name ~source_path:source;
      assert (
        read_file (Project_store.gameplan_yaml_path project_name)
        = "value: before\n");
      Unix.rmdir (Project_store.gameplan_path project_name);
      Project_store.publish_gameplan_artifact ~project_name;
      assert (
        Yojson.Safe.from_string (read_file artifact)
        = `Assoc [ ("value", `String "before") ]);
      (* Replacing a read-only artifact succeeds via rename, whereas opening it
         directly for writing would fail. *)
      Unix.chmod artifact 0o444;
      write_file
        (Project_store.gameplan_yaml_path project_name)
        "value: after\n";
      Project_store.publish_gameplan_artifact ~project_name;
      let valid = read_file artifact in
      assert (
        Yojson.Safe.from_string valid = `Assoc [ ("value", `String "after") ]);
      write_file (Project_store.gameplan_yaml_path project_name) "value: [";
      Project_store.publish_gameplan_artifact ~project_name;
      assert (read_file artifact = valid);
      write_file (Project_store.gameplan_yaml_path project_name) "value: next\n";
      Unix.chmod artifacts 0o555;
      Project_store.publish_gameplan_artifact ~project_name;
      Unix.chmod artifacts 0o755;
      assert (read_file artifact = valid);
      assert (Array.to_list (Sys.readdir artifacts) = [ "gameplan.json" ]);
      assert (Sys.is_directory dir));
  print_endline "gameplan publication and best-effort cleanup: OK"
