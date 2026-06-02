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

let managed_repo_dir project_name =
  Stdlib.Filename.concat (project_dir project_name) "repo"

let event_log_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "events.jsonl"

let sessions_dir project_name =
  Stdlib.Filename.concat (project_dir project_name) "sessions"

let config_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "config.json"

let gameplan_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "gameplan.md"

let gameplan_json_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "gameplan.json"

let stored_gameplan_path project_name =
  let md = gameplan_path project_name in
  if Stdlib.Sys.file_exists md then md else gameplan_json_path project_name

let artifacts_root project_name =
  Stdlib.Filename.concat (project_dir project_name) "artifacts"

let artifact_dir ~project_name ~patch_id =
  Stdlib.Filename.concat
    (artifacts_root project_name)
    (Types.Patch_id.to_string patch_id)

let gameplan_artifact_path project_name =
  Stdlib.Filename.concat (artifacts_root project_name) "gameplan.json"

let pr_body_artifact_path ~project_name ~patch_id =
  Stdlib.Filename.concat (artifact_dir ~project_name ~patch_id) "pr-body.md"

(** Absolute path the agent writes [findings_wontfix.json] to during a Findings
    session. Lives alongside [pr-body.md] under [artifacts/<patch_id>/]. The
    supervisor reads it after the session to decide which findings to POST as
    ["wontfix"]; everything not listed is POSTed as ["addressed"]. *)
let findings_wontfix_artifact_path ~project_name ~patch_id =
  Stdlib.Filename.concat
    (artifact_dir ~project_name ~patch_id)
    "findings_wontfix.json"

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
  backend : string;
  model : string;
  main_branch : string;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
  url_scheme : string option; [@yojson.default None]
      (* Persisted transport scheme for the managed clone's [origin]. [None]
         on legacy configs predating P0-D; on the next [ensure_managed_repo]
         we auto-detect from sibling clones and rewrite to [Some "https"] or
         [Some "ssh"]. *)
}
[@@deriving yojson]

let save_config ~project_name ~github_token ~github_owner ~github_repo ~backend
    ~model ~main_branch ~poll_interval ~repo_root ~max_concurrency
    ?(url_scheme : string option = None) () =
  let dir = project_dir project_name in
  ensure_dir dir;
  let config =
    {
      project_name;
      github_token;
      github_owner;
      github_repo;
      backend;
      model;
      main_branch;
      poll_interval;
      repo_root;
      max_concurrency;
      url_scheme;
    }
  in
  let json = yojson_of_stored_config config in
  let oc = Stdlib.open_out_bin (config_path project_name) in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () ->
      Stdlib.output_string oc (Yojson.Safe.pretty_to_string json);
      Stdlib.flush oc)

(* Migrate legacy combined backend strings (["claude-sonnet"], ["claude-opus"])
   into the decomposed [backend] + [model] form, and ensure the [model] field
   is always present (it was added after [backend] and is missing from older
   configs). Only the legacy combined names inject a model; bare ["claude"]
   stays bare and lets the runtime omit [--model] so the Claude CLI applies
   its own default. *)
let migrate_backend_model fields =
  let assoc = List.Assoc.find fields ~equal:String.equal in
  let stored_backend =
    match assoc "backend" with Some (`String s) -> s | _ -> ""
  in
  let stored_model =
    match assoc "model" with Some (`String s) -> s | _ -> ""
  in
  let backend, model =
    match (stored_backend, stored_model) with
    | "claude-sonnet", "" -> ("claude", "sonnet")
    | "claude-opus", "" -> ("claude", "opus")
    (* Legacy combined name with an explicitly stored model: keep the stored
       model rather than overriding it from the legacy backend suffix. *)
    | ("claude-sonnet" | "claude-opus"), m -> ("claude", m)
    | b, m -> (b, m)
  in
  let without =
    List.filter fields ~f:(fun (k, _) ->
        not (String.equal k "backend" || String.equal k "model"))
  in
  ("backend", `String backend) :: ("model", `String model) :: without

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
    match json with
    | `Assoc fields ->
        Ok (stored_config_of_yojson (`Assoc (migrate_backend_model fields)))
    | _ -> Ok (stored_config_of_yojson json)
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

let publish_gameplan_artifact ~project_name =
  let source = stored_gameplan_path project_name in
  if Stdlib.Sys.file_exists source then (
    let ic = Stdlib.open_in_bin source in
    let content =
      Stdlib.Fun.protect
        ~finally:(fun () -> Stdlib.close_in_noerr ic)
        (fun () -> Stdlib.In_channel.input_all ic)
    in
    let dest = gameplan_artifact_path project_name in
    ensure_dir (Stdlib.Filename.dirname dest);
    let oc = Stdlib.open_out_bin dest in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_out oc)
      (fun () ->
        Stdlib.output_string oc content;
        Stdlib.flush oc))

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

(* === Inline tests === *)

(* Mirrors [Session_artifacts]'s test helper: point ONTON_DATA_DIR at a
   fresh temp dir for the duration of [f], then restore. *)
let with_temp_data_dir f =
  let old = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
  let dir = Stdlib.Filename.temp_dir "onton-project-store-" "" in
  Unix.putenv "ONTON_DATA_DIR" dir;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      (match old with
      | Some value -> Unix.putenv "ONTON_DATA_DIR" value
      | None ->
          (* Tests do not have an unsetenv binding. Restore the resolved
             default data root so later tests never inherit a deleted temp
             directory through ONTON_DATA_DIR. *)
          let default =
            match Stdlib.Sys.getenv_opt "XDG_DATA_HOME" with
            | Some xdg -> Stdlib.Filename.concat xdg "onton"
            | None ->
                Stdlib.Filename.concat
                  (Stdlib.Filename.concat (Stdlib.Sys.getenv "HOME")
                     ".local/share")
                  "onton"
          in
          ensure_dir default;
          Unix.putenv "ONTON_DATA_DIR" default);
      try
        ignore
          (Stdlib.Sys.command
             (Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote dir)))
      with _ -> ())
    (fun () -> f ())

let read_file_for_test path =
  let ic = Stdlib.open_in_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in_noerr ic)
    (fun () -> Stdlib.In_channel.input_all ic)

let%test "publish_gameplan_artifact copies the stored gameplan for agents" =
  with_temp_data_dir (fun () ->
      let project_name = "publish-test" in
      ensure_dir (project_dir project_name);
      let content = "{\"project_name\": \"publish-test\", \"patches\": []}" in
      let oc = Stdlib.open_out_bin (gameplan_json_path project_name) in
      Stdlib.output_string oc content;
      Stdlib.close_out oc;
      publish_gameplan_artifact ~project_name;
      let dest = gameplan_artifact_path project_name in
      Stdlib.Sys.file_exists dest
      && String.equal (read_file_for_test dest) content)

let%test "publish_gameplan_artifact refreshes a stale copy" =
  with_temp_data_dir (fun () ->
      let project_name = "publish-test-refresh" in
      ensure_dir (project_dir project_name);
      let write path content =
        let oc = Stdlib.open_out_bin path in
        Stdlib.output_string oc content;
        Stdlib.close_out oc
      in
      write (gameplan_json_path project_name) "{\"v\": 1}";
      publish_gameplan_artifact ~project_name;
      write (gameplan_json_path project_name) "{\"v\": 2}";
      publish_gameplan_artifact ~project_name;
      String.equal
        (read_file_for_test (gameplan_artifact_path project_name))
        "{\"v\": 2}")

let%test "publish_gameplan_artifact is a no-op without a stored gameplan" =
  with_temp_data_dir (fun () ->
      let project_name = "publish-test-empty" in
      publish_gameplan_artifact ~project_name;
      not (Stdlib.Sys.file_exists (gameplan_artifact_path project_name)))
