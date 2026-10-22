(* @archlint.module shell
   @archlint.domain worker-transport *)

open Onton
open Onton_core

let usage () =
  prerr_endline
    "Usage: onton-worker --gameplan FILE --repo-root DIR --project-name NAME \
     [--main-branch BRANCH] [--backend codex|claude] [--ack-timeout SECONDS]";
  exit 2

let parse_args () =
  let gameplan = ref None in
  let repo_root = ref None in
  let project_name = ref None in
  let main_branch = ref "main" in
  let backend = ref "codex" in
  let ack_timeout = ref 10. in
  let rec scan = function
    | [] -> ()
    | "--gameplan" :: value :: rest ->
        gameplan := Some value;
        scan rest
    | "--repo-root" :: value :: rest ->
        repo_root := Some value;
        scan rest
    | "--project-name" :: value :: rest ->
        project_name := Some value;
        scan rest
    | "--main-branch" :: value :: rest ->
        main_branch := value;
        scan rest
    | "--backend" :: value :: rest ->
        backend := value;
        scan rest
    | "--ack-timeout" :: value :: rest -> (
        match float_of_string_opt value with
        | Some seconds when seconds > 0. && seconds <= 60. ->
            ack_timeout := seconds;
            scan rest
        | _ -> usage ())
    | _ -> usage ()
  in
  scan (List.tl (Array.to_list Sys.argv));
  match (!gameplan, !repo_root, !project_name) with
  | Some gameplan, Some repo_root, Some project_name
    when String.length project_name > 0
         && String.length !main_branch > 0
         && List.mem !backend [ "codex"; "claude" ] ->
      (gameplan, repo_root, project_name, !main_branch, !backend, !ack_timeout)
  | _ -> usage ()

let github_token () =
  match Sys.getenv_opt "ONTON_GITHUB_TOKEN_FILE" with
  | Some path -> (
      try String.trim (In_channel.with_open_bin path In_channel.input_all)
      with _ -> "")
  | None -> Option.value (Sys.getenv_opt "GITHUB_TOKEN") ~default:""

let fixture_forge ~branch path =
  try
    let json = Yojson.Safe.from_file path in
    match json with
    | `Assoc [ ("branch", `String named); ("state", `String state) ]
      when String.equal named (Types.Branch.to_string branch) -> (
        match state with
        | "merged" -> Ok (Branch_reconcile.Forge_merged, None)
        | "not_merged" -> Ok (Branch_reconcile.Forge_not_merged, None)
        | "unavailable" -> Error "forge fixture evidence unavailable"
        | _ -> Error "invalid forge fixture state")
    | _ -> Error "invalid forge fixture observation"
  with _ -> Error "forge fixture read failed"

let () =
  let ( gameplan_path,
        repo_root,
        project_name,
        main_branch_name,
        backend_name,
        ack_timeout ) =
    parse_args ()
  in
  let gameplan =
    match Gameplan_parser.parse_file gameplan_path with
    | Ok parsed -> parsed.Gameplan_parser.gameplan
    | Error reason ->
        prerr_endline reason;
        exit 2
  in
  let main_branch = Types.Branch.of_string main_branch_name in
  Eio_main.run @@ fun system ->
  let clock = Eio.Stdenv.clock system in
  let process_mgr = Eio.Stdenv.process_mgr system in
  let registry =
    Backend_registry.create ~process_mgr ~clock ~timeout:1800.
      ~setsid_exec:(Sys.getenv_opt "ONTON_SETSID_EXEC")
      ~extras:[]
  in
  let backend =
    Backend_registry.get registry ~backend:backend_name ~model:None ~effort:None
  in
  let observe_forge ~branch =
    match Sys.getenv_opt "ONTON_WORKER_FORGE_FIXTURE" with
    | Some path -> fixture_forge ~branch path
    | None -> (
        let token = github_token () in
        if String.length token = 0 then
          Error "GitHub token unavailable for recovery observation"
        else
          let module Github =
            (val Github.make ~net:(Eio.Stdenv.net system) ~clock ~token
                   ~owner:gameplan.repo_owner ~repo:gameplan.repo_name
                   ~main_branch)
          in
          match Github.list_prs ~branch ~state:`All () with
          | Error error -> Error (Github.show_error error)
          | Ok prs -> (
              match List.find_opt (fun (_, _, merged) -> merged) prs with
              | Some (number, _, _) ->
                  Ok (Branch_reconcile.Forge_merged, Some number)
              | None ->
                  Ok
                    ( Branch_reconcile.Forge_not_merged,
                      Option.map
                        (fun (number, _, _) -> number)
                        (List.nth_opt prs 0) )))
  in
  let env : Worker.env =
    {
      input = Eio.Stdenv.stdin system;
      output = Eio.Stdenv.stdout system;
      fs = Eio.Stdenv.fs system;
      clock;
      process_mgr;
      gameplan;
      main_branch;
      repo_root;
      project_name;
      backend;
      observe_forge;
      ack_timeout;
    }
  in
  try Worker.run env
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 1
