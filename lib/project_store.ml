open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let data_dir () =
  match Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" with
  | Some d -> d
  | None -> (
      match Stdlib.Sys.getenv_opt "XDG_DATA_HOME" with
      | Some xdg -> Stdlib.Filename.concat xdg "onton"
      | None ->
          Stdlib.Filename.concat
            (Stdlib.Filename.concat (Stdlib.Sys.getenv "HOME") ".local/share")
            "onton")

let slugify name =
  String.concat_map name ~f:(fun c ->
      if Char.is_alphanum c || Char.equal c '-' then String.of_char c
      else if Char.equal c ' ' || Char.equal c '_' then "-"
      else "")
  |> String.lowercase

let project_dir project_name =
  Stdlib.Filename.concat (data_dir ()) (slugify project_name)

let snapshot_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "snapshot.json"

let event_log_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "events.jsonl"

let config_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "config.json"

let gameplan_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "gameplan.md"

let gameplan_json_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "gameplan.json"

let stored_gameplan_path project_name =
  let md = gameplan_path project_name in
  if Stdlib.Sys.file_exists md then md else gameplan_json_path project_name

let ensure_dir path =
  let rec mkdir_p dir =
    if not (Stdlib.Sys.file_exists dir) then (
      mkdir_p (Stdlib.Filename.dirname dir);
      Stdlib.Sys.mkdir dir 0o755)
  in
  mkdir_p path

type stored_config = {
  project_name : string;
  github_token : string;
  github_owner : string;
  github_repo : string;
  main_branch : string;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
}
[@@deriving yojson]

let save_config ~project_name ~github_token ~github_owner ~github_repo
    ~main_branch ~poll_interval ~repo_root ~max_concurrency =
  let dir = project_dir project_name in
  ensure_dir dir;
  let config =
    {
      project_name;
      github_token;
      github_owner;
      github_repo;
      main_branch;
      poll_interval;
      repo_root;
      max_concurrency;
    }
  in
  let json = yojson_of_stored_config config in
  let oc = Stdlib.open_out_bin (config_path project_name) in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () ->
      Stdlib.output_string oc (Yojson.Safe.pretty_to_string json);
      Stdlib.flush oc)

let load_config ~project_name =
  let path = config_path project_name in
  try
    let ic = Stdlib.open_in path in
    let content =
      Stdlib.Fun.protect
        ~finally:(fun () -> Stdlib.close_in_noerr ic)
        (fun () -> Stdlib.In_channel.input_all ic)
    in
    let json = Yojson.Safe.from_string content in
    Ok (stored_config_of_yojson json)
  with exn -> Error (Stdlib.Printexc.to_string exn)

let save_gameplan_source ~project_name ~source_path =
  let dir = project_dir project_name in
  ensure_dir dir;
  let dest, stale =
    if Stdlib.Filename.check_suffix source_path ".json" then
      (gameplan_json_path project_name, gameplan_path project_name)
    else (gameplan_path project_name, gameplan_json_path project_name)
  in
  let ic = Stdlib.open_in source_path in
  let content =
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_in_noerr ic)
      (fun () -> Stdlib.In_channel.input_all ic)
  in
  let oc = Stdlib.open_out_bin dest in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () ->
      Stdlib.output_string oc content;
      Stdlib.flush oc);
  if Stdlib.Sys.file_exists stale then
    try Stdlib.Sys.remove stale with Sys_error _ -> ()

let project_exists project_name =
  Stdlib.Sys.file_exists (config_path project_name)

let list_projects () =
  let dir = data_dir () in
  if Stdlib.Sys.file_exists dir then
    Stdlib.Sys.readdir dir |> Array.to_list
    |> List.filter ~f:(fun name ->
        Stdlib.Sys.is_directory (Stdlib.Filename.concat dir name)
        && Stdlib.Sys.file_exists
             (Stdlib.Filename.concat
                (Stdlib.Filename.concat dir name)
                "config.json"))
  else []
