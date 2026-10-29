(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)
open Onton
open Onton_core

[@@@warning "-4"]

let usage () =
  prerr_endline
    "Usage: onton-coordinator --gameplan FILE --config FILE --run-id ID \
     --config-id DIGEST --coordinator-id TASK --resource-helper EXEC \
     --control-socket PATH (--fresh | --takeover-epoch N --takeover-revision \
     N) [--forge-helper EXEC]";
  exit 2

let fail why =
  prerr_endline ("onton-coordinator: " ^ why);
  exit 1

let get = function Ok x -> x | Error why -> fail why

let read path limit =
  let stat = Unix.stat path in
  if stat.Unix.st_kind <> Unix.S_REG || stat.st_size > limit then
    fail "input is not a bounded regular file";
  In_channel.with_open_bin path In_channel.input_all

let executable path =
  try
    Unix.access path [ Unix.X_OK ];
    true
  with _ -> false

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let args = Hashtbl.create 16 in
  let rec scan = function
    | [] -> ()
    | "--fresh" :: rest ->
        if Hashtbl.mem args "--fresh" then usage ();
        Hashtbl.add args "--fresh" "true";
        scan rest
    | key :: value :: rest
      when List.mem key
             [
               "--gameplan";
               "--config";
               "--run-id";
               "--config-id";
               "--coordinator-id";
               "--resource-helper";
               "--control-socket";
               "--takeover-epoch";
               "--takeover-revision";
               "--forge-helper";
             ] ->
        if Hashtbl.mem args key then usage ();
        Hashtbl.add args key value;
        scan rest
    | _ -> usage ()
  in
  scan (List.tl (Array.to_list Sys.argv));
  let required key =
    match Hashtbl.find_opt args key with
    | Some value when value <> "" -> value
    | _ -> usage ()
  in
  let run_id = required "--run-id"
  and config_id = required "--config-id"
  and coordinator_id = required "--coordinator-id" in
  let helper = required "--resource-helper"
  and control_path = required "--control-socket" in
  let fresh = Hashtbl.mem args "--fresh" in
  let takeover_guards =
    match
      ( fresh,
        Hashtbl.find_opt args "--takeover-epoch",
        Hashtbl.find_opt args "--takeover-revision" )
    with
    | true, None, None -> None
    | false, Some e, Some r -> (
        match (Int64.of_string_opt e, Int64.of_string_opt r) with
        | Some e, Some r when e >= 0L && r >= 0L -> Some (e, r)
        | _ -> usage ())
    | _ -> usage ()
  in
  try
    let config_bytes = read (required "--config") 16384 in
    let config = get (Coordinator_config.decode config_bytes) in
    let plan_bytes = read (required "--gameplan") (7 * 1024 * 1024) in
    let gameplan =
      (get (Gameplan_parser.parse_string plan_bytes)).Gameplan_parser.gameplan
    in
    if
      gameplan.Types.Gameplan.repo_owner = ""
      || gameplan.repo_name = "" || gameplan.patches = []
    then fail "gameplan requires repository identity and patches";
    if not (executable helper) then
      fail "resource helper must be an executable path";
    let client =
      Resource_client.create ~helper ~transport:Resource_client.subprocess
    in
    Eio_main.run @@ fun system ->
    let main_branch = Types.Branch.of_string config.main_branch in
    let git =
      Branch_reconcile_executor.make_io
        ~process_mgr:(Eio.Stdenv.process_mgr system)
        ~clock:(Eio.Stdenv.clock system) ~path:config.repo_root
    in
    let code, _, _ = git.git [ "rev-parse"; "--git-dir" ] in
    if code <> 0 then fail "coordinator read checkout is unavailable";
    (match
       Branch_reconcile_executor.remote_base git ~branch:config.main_branch
     with
    | Ok (Some _) -> ()
    | _ -> fail "origin main branch is not directly observable");
    let start (type e) (module F : Forge.S with type error = e) =
      (match F.check_repo_access () with
      | Ok () -> ()
      | Error e -> fail (F.show_error e));
      let ready =
        get
          (Coordinator_bootstrap.start ~client ~run_id ~coordinator_id
             ~config_id
             ~plan_id:(Digest.to_hex (Digest.string plan_bytes))
             ~plan_bytes ~config_bytes ~capacity:config.capacity ~fresh
             ~takeover_guards ~next_id:Coordinator_runtime.next_id)
      in
      Coordinator_runtime.run ~system ~client ~store:ready.store ~config
        ~gameplan
        ~forge:(module F)
        ~control_path
    in
    match Hashtbl.find_opt args "--forge-helper" with
    | Some helper ->
        if not (executable helper) then fail "forge helper is not executable";
        start
          (Coordinator_command_forge.make ~helper ~owner:gameplan.repo_owner)
    | None ->
        let token =
          match Sys.getenv_opt "ONTON_GITHUB_TOKEN_FILE" with
          | Some path -> String.trim (read path 65536)
          | None -> Option.value (Sys.getenv_opt "GITHUB_TOKEN") ~default:""
        in
        if token = "" then fail "GitHub credentials unavailable";
        start
          (Github.make ~net:(Eio.Stdenv.net system)
             ~clock:(Eio.Stdenv.clock system) ~token ~owner:gameplan.repo_owner
             ~repo:gameplan.repo_name ~main_branch)
  with exn -> fail (Printexc.to_string exn)
