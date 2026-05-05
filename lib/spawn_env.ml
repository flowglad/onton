open Base

let patch_root ~project_dir ~patch_id =
  Stdlib.Filename.concat project_dir
    (Stdlib.Filename.concat "spawn-envs" (Types.Patch_id.to_string patch_id))

let backend_dir ~root ~backend = Stdlib.Filename.concat root backend

let per_patch_env_in_project_dir ~project_dir ~patch_id =
  let root = patch_root ~project_dir ~patch_id in
  let claude_dir = backend_dir ~root ~backend:"claude" in
  let codex_dir = backend_dir ~root ~backend:"codex" in
  let opencode_dir = backend_dir ~root ~backend:"opencode" in
  List.iter [ claude_dir; codex_dir; opencode_dir ] ~f:Project_store.ensure_dir;
  [
    ("CLAUDE_CONFIG_DIR", claude_dir);
    ("CODEX_HOME", codex_dir);
    ("OPENCODE_CONFIG_DIR", opencode_dir);
  ]

let per_patch_env ~project_name ~patch_id =
  per_patch_env_in_project_dir
    ~project_dir:(Project_store.project_dir project_name)
    ~patch_id

let split_env_entry entry =
  match String.lsplit2 entry ~on:'=' with
  | Some (key, value) -> (key, value)
  | None -> (entry, "")

let merge_env ~base_env ~overrides =
  let merged = Hashtbl.create (module String) in
  Array.iter base_env ~f:(fun entry ->
      let key, value = split_env_entry entry in
      Hashtbl.set merged ~key ~data:value);
  List.iter overrides ~f:(fun (key, value) ->
      Hashtbl.set merged ~key ~data:value);
  Hashtbl.to_alist merged
  |> List.map ~f:(fun (key, value) -> key ^ "=" ^ value)
  |> Array.of_list

let rec remove_tree path =
  if Stdlib.Sys.file_exists path then
    if Stdlib.Sys.is_directory path then (
      Stdlib.Sys.readdir path
      |> Array.iter ~f:(fun child ->
          remove_tree (Stdlib.Filename.concat path child));
      Unix.rmdir path)
    else Unix.unlink path

let with_temp_project_dir f =
  let base = Stdlib.Filename.get_temp_dir_name () in
  let dir =
    Stdlib.Filename.concat base
      (Printf.sprintf "onton-spawn-env-%06x" (Random.bits ()))
  in
  Project_store.ensure_dir dir;
  Stdlib.Fun.protect ~finally:(fun () -> remove_tree dir) (fun () -> f dir)

let%test "distinct patch_ids yield distinct config dirs" =
  with_temp_project_dir @@ fun project_dir ->
  let patch_a = Types.Patch_id.of_string "patch-1" in
  let patch_b = Types.Patch_id.of_string "patch-2" in
  let env_a = per_patch_env_in_project_dir ~project_dir ~patch_id:patch_a in
  let env_b = per_patch_env_in_project_dir ~project_dir ~patch_id:patch_b in
  let find key env = List.Assoc.find env ~equal:String.equal key in
  not
    (String.equal
       (Option.value_exn (find "CLAUDE_CONFIG_DIR" env_a))
       (Option.value_exn (find "CLAUDE_CONFIG_DIR" env_b)))

let%test "merged env contains per-patch overrides" =
  with_temp_project_dir @@ fun project_dir ->
  let patch_a = Types.Patch_id.of_string "patch-1" in
  let patch_b = Types.Patch_id.of_string "patch-2" in
  let base_env =
    [|
      "PATH=/usr/bin";
      "CLAUDE_CONFIG_DIR=/shared/claude";
      "CODEX_HOME=/shared/codex";
    |]
  in
  let env_a =
    merge_env ~base_env
      ~overrides:(per_patch_env_in_project_dir ~project_dir ~patch_id:patch_a)
  in
  let env_b =
    merge_env ~base_env
      ~overrides:(per_patch_env_in_project_dir ~project_dir ~patch_id:patch_b)
  in
  let find key env =
    Array.find_map env ~f:(fun entry ->
        match String.lsplit2 entry ~on:'=' with
        | Some (k, v) when String.equal k key -> Some v
        | _ -> None)
  in
  Option.is_some (find "PATH" env_a)
  && Option.is_some (find "OPENCODE_CONFIG_DIR" env_a)
  &&
  let claude_a = Option.value_exn (find "CLAUDE_CONFIG_DIR" env_a) in
  let claude_b = Option.value_exn (find "CLAUDE_CONFIG_DIR" env_b) in
  let codex_a = Option.value_exn (find "CODEX_HOME" env_a) in
  String.is_substring claude_a ~substring:"spawn-envs/patch-1/claude"
  && String.is_substring codex_a ~substring:"spawn-envs/patch-1/codex"
  && not (String.equal claude_a claude_b)
