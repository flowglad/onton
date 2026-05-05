open Base

external unsetenv_stub : string -> unit = "caml_onton_unsetenv"

let patch_root ~project_dir ~patch_id =
  Stdlib.Filename.concat project_dir
    (Stdlib.Filename.concat "spawn-envs" (Types.Patch_id.to_string patch_id))

let backend_dir ~root ~backend = Stdlib.Filename.concat root backend

(* Idempotently symlink [src] at [dst]. Skip if [dst] already exists in any
   form (regular file, dir, or existing symlink): the backend CLI may have
   replaced the symlink with a freshly-written file (e.g. atomic-rename token
   refresh) and we must not clobber that real state. Skip if [src] is missing:
   the user may not be logged in to that backend. *)
let seed_link ~src ~dst =
  let dst_exists =
    match Unix.lstat dst with
    | _ -> true
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
  in
  let src_exists =
    match Unix.lstat src with
    | _ -> true
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
  in
  if (not dst_exists) && src_exists then
    try Unix.symlink src dst with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let resolve_user_config_dir ~env_var ~home_subpath =
  let absolutize path =
    if Stdlib.Filename.is_relative path then
      Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) path
    else path
  in
  match Stdlib.Sys.getenv_opt env_var with
  | Some dir when not (String.is_empty dir) -> Some (absolutize dir)
  | _ -> (
      match Stdlib.Sys.getenv_opt "HOME" with
      | Some home when not (String.is_empty home) ->
          Some (Stdlib.Filename.concat (absolutize home) home_subpath)
      | _ -> None)

(* Seed the per-patch config dirs with symlinks to the user's real auth
   files. Symlinks (not copies) so token rotations the backend writes
   in-place propagate back to the user's real config dir, and so the user's
   plain CLI continues to share a session with the per-patch CLI. *)
let seed_auth_links_with ~claude_src_dir ~codex_src_dir ~opencode_src_dir
    ~claude_dir ~codex_dir ~opencode_dir =
  let seed src_dir dst_dir filename =
    Option.iter src_dir ~f:(fun src_dir ->
        seed_link
          ~src:(Stdlib.Filename.concat src_dir filename)
          ~dst:(Stdlib.Filename.concat dst_dir filename))
  in
  seed claude_src_dir claude_dir ".credentials.json";
  seed codex_src_dir codex_dir "auth.json";
  seed opencode_src_dir opencode_dir "opencode.json"

let seed_auth_links ~claude_dir ~codex_dir ~opencode_dir =
  let claude_src_dir =
    resolve_user_config_dir ~env_var:"CLAUDE_CONFIG_DIR" ~home_subpath:".claude"
  in
  let codex_src_dir =
    resolve_user_config_dir ~env_var:"CODEX_HOME" ~home_subpath:".codex"
  in
  let opencode_src_dir =
    resolve_user_config_dir ~env_var:"OPENCODE_CONFIG_DIR"
      ~home_subpath:".config/opencode"
  in
  seed_auth_links_with ~claude_src_dir ~codex_src_dir ~opencode_src_dir
    ~claude_dir ~codex_dir ~opencode_dir

let per_patch_env_in_project_dir ~project_dir ~patch_id =
  let root = patch_root ~project_dir ~patch_id in
  let claude_dir = backend_dir ~root ~backend:"claude" in
  let codex_dir = backend_dir ~root ~backend:"codex" in
  let opencode_dir = backend_dir ~root ~backend:"opencode" in
  List.iter [ claude_dir; codex_dir; opencode_dir ] ~f:Project_store.ensure_dir;
  seed_auth_links ~claude_dir ~codex_dir ~opencode_dir;
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
  |> List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
  |> List.map ~f:(fun (key, value) -> key ^ "=" ^ value)
  |> Array.of_list

let rec remove_tree path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat -> (
      match stat.Unix.st_kind with
      | Unix.S_DIR ->
          Stdlib.Sys.readdir path
          |> Array.iter ~f:(fun child ->
              remove_tree (Stdlib.Filename.concat path child));
          Unix.rmdir path
      | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
      | Unix.S_SOCK ->
          Unix.unlink path)

let with_temp_project_dir f =
  let dir = Stdlib.Filename.temp_dir "onton-spawn-env-" "" in
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

let write_file path contents =
  let oc = Stdlib.open_out path in
  Stdlib.output_string oc contents;
  Stdlib.close_out oc

let read_file path =
  let ic = Stdlib.open_in path in
  let len = Stdlib.in_channel_length ic in
  let buf = Stdlib.Bytes.create len in
  Stdlib.really_input ic buf 0 len;
  Stdlib.close_in ic;
  Stdlib.Bytes.to_string buf

let make_seed_dirs project_dir patch_id_str =
  let patch_id = Types.Patch_id.of_string patch_id_str in
  let root = patch_root ~project_dir ~patch_id in
  let claude_dir = backend_dir ~root ~backend:"claude" in
  let codex_dir = backend_dir ~root ~backend:"codex" in
  let opencode_dir = backend_dir ~root ~backend:"opencode" in
  List.iter [ claude_dir; codex_dir; opencode_dir ] ~f:Project_store.ensure_dir;
  (claude_dir, codex_dir, opencode_dir)

let%test "seed_auth_links creates symlinks when src auth files exist" =
  with_temp_project_dir @@ fun project_dir ->
  with_temp_project_dir @@ fun src_root ->
  let claude_src = Stdlib.Filename.concat src_root "claude" in
  let codex_src = Stdlib.Filename.concat src_root "codex" in
  let opencode_src = Stdlib.Filename.concat src_root "opencode" in
  List.iter [ claude_src; codex_src; opencode_src ] ~f:Project_store.ensure_dir;
  write_file (Stdlib.Filename.concat claude_src ".credentials.json") "c-tok";
  write_file (Stdlib.Filename.concat codex_src "auth.json") "x-tok";
  write_file (Stdlib.Filename.concat opencode_src "opencode.json") "o-cfg";
  let claude_dir, codex_dir, opencode_dir =
    make_seed_dirs project_dir "patch-seed-1"
  in
  seed_auth_links_with ~claude_src_dir:(Some claude_src)
    ~codex_src_dir:(Some codex_src) ~opencode_src_dir:(Some opencode_src)
    ~claude_dir ~codex_dir ~opencode_dir;
  let target dst_dir filename =
    Unix.readlink (Stdlib.Filename.concat dst_dir filename)
  in
  String.equal
    (target claude_dir ".credentials.json")
    (Stdlib.Filename.concat claude_src ".credentials.json")
  && String.equal
       (target codex_dir "auth.json")
       (Stdlib.Filename.concat codex_src "auth.json")
  && String.equal
       (target opencode_dir "opencode.json")
       (Stdlib.Filename.concat opencode_src "opencode.json")

let%test "seed_auth_links is idempotent" =
  with_temp_project_dir @@ fun project_dir ->
  with_temp_project_dir @@ fun src_root ->
  let codex_src = Stdlib.Filename.concat src_root "codex" in
  Project_store.ensure_dir codex_src;
  write_file (Stdlib.Filename.concat codex_src "auth.json") "x-tok";
  let claude_dir, codex_dir, opencode_dir =
    make_seed_dirs project_dir "patch-seed-2"
  in
  let do_seed () =
    seed_auth_links_with ~claude_src_dir:None ~codex_src_dir:(Some codex_src)
      ~opencode_src_dir:None ~claude_dir ~codex_dir ~opencode_dir
  in
  do_seed ();
  do_seed ();
  String.equal
    (Unix.readlink (Stdlib.Filename.concat codex_dir "auth.json"))
    (Stdlib.Filename.concat codex_src "auth.json")

let%test "seed_auth_links skips backends with no src auth file" =
  with_temp_project_dir @@ fun project_dir ->
  with_temp_project_dir @@ fun src_root ->
  let codex_src = Stdlib.Filename.concat src_root "codex" in
  Project_store.ensure_dir codex_src;
  let claude_dir, codex_dir, opencode_dir =
    make_seed_dirs project_dir "patch-seed-3"
  in
  seed_auth_links_with ~claude_src_dir:None ~codex_src_dir:(Some codex_src)
    ~opencode_src_dir:None ~claude_dir ~codex_dir ~opencode_dir;
  not (Stdlib.Sys.file_exists (Stdlib.Filename.concat codex_dir "auth.json"))

let%test "seed_auth_links preserves an existing dst file (no clobber)" =
  with_temp_project_dir @@ fun project_dir ->
  with_temp_project_dir @@ fun src_root ->
  let codex_src = Stdlib.Filename.concat src_root "codex" in
  Project_store.ensure_dir codex_src;
  write_file (Stdlib.Filename.concat codex_src "auth.json") "src-tok";
  let claude_dir, codex_dir, opencode_dir =
    make_seed_dirs project_dir "patch-seed-4"
  in
  let dst = Stdlib.Filename.concat codex_dir "auth.json" in
  write_file dst "preexisting-tok";
  seed_auth_links_with ~claude_src_dir:None ~codex_src_dir:(Some codex_src)
    ~opencode_src_dir:None ~claude_dir ~codex_dir ~opencode_dir;
  let stat = Unix.lstat dst in
  let is_regular =
    match stat.Unix.st_kind with
    | Unix.S_REG -> true
    | Unix.S_DIR | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
    | Unix.S_SOCK ->
        false
  in
  is_regular && String.equal (read_file dst) "preexisting-tok"

let%test "resolve_user_config_dir absolutizes relative env and HOME paths" =
  let cwd = Stdlib.Sys.getcwd () in
  let old_home = Stdlib.Sys.getenv_opt "HOME" in
  let restore_home () =
    match old_home with
    | Some home -> Unix.putenv "HOME" home
    | None -> unsetenv_stub "HOME"
  in
  Unix.putenv "HOME" "relative-home";
  Stdlib.Fun.protect ~finally:restore_home @@ fun () ->
  let env_dir =
    Option.value_exn
      (resolve_user_config_dir ~env_var:"HOME" ~home_subpath:".unused")
  in
  let home_dir =
    Option.value_exn
      (resolve_user_config_dir ~env_var:"ONTON_MISSING_CONFIG_DIR"
         ~home_subpath:".config/onton")
  in
  String.equal env_dir (Stdlib.Filename.concat cwd "relative-home")
  && String.equal home_dir
       (Stdlib.Filename.concat
          (Stdlib.Filename.concat cwd "relative-home")
          ".config/onton")
