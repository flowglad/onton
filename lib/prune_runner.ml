open Base
open Types

type prune_status =
  | All_merged
  | Not_merged
  | No_patches
  | No_snapshot
  | Load_error of string

type refresh_summary = {
  attempted : int;
  newly_merged : int;
  errors : int;
  skipped_reason : string option;
      (** When the project was eligible for refresh but it was skipped (e.g.
          missing token), explain why so the user understands the kept reason.
      *)
}
(** Per-project refresh summary, captured during classification and stitched
    into the human-readable [kept] reason. None when refresh was disabled or the
    project had nothing eligible to refresh. *)

let pluralize ?plural n singular =
  let many = match plural with Some p -> p | None -> singular ^ "s" in
  Stdlib.Printf.sprintf "%d %s" n (if n = 1 then singular else many)

let rec remove_path path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      let entries = Stdlib.Sys.readdir path in
      Array.iter entries ~f:(fun name ->
          remove_path (Stdlib.Filename.concat path name));
      try Unix.rmdir path with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
  | {
   Unix.st_kind =
     ( Unix.S_REG | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO
     | Unix.S_SOCK );
   _;
  } -> (
      try Stdlib.Sys.remove path with Sys_error _ -> ())

(** Build the list of (patch_id, pr_number) pairs that can be reconciled with
    the forge: agent stored as not-merged AND has a PR number on file. *)
let refresh_candidates (agents : Patch_agent.t Map.M(Patch_id).t) =
  Map.fold agents ~init:[] ~f:(fun ~key:patch_id ~data:agent acc ->
      if agent.Patch_agent.merged then acc
      else
        match agent.Patch_agent.pr_number with
        | None -> acc
        | Some pr_number -> (patch_id, pr_number) :: acc)

(** Refresh [agents] by querying the forge for every non-terminal patch with a
    recorded PR number. Returns the (possibly updated) agents map and a
    [refresh_summary]. Forge errors are tolerated — the corresponding agent is
    left untouched, classification proceeds with its stored [merged] flag. *)
let refresh_agents_from_forge ~net ~clock ~(cfg : Project_store.stored_config)
    ~(agents : Patch_agent.t Map.M(Patch_id).t) =
  let candidates = refresh_candidates agents in
  if List.is_empty candidates then
    ( agents,
      { attempted = 0; newly_merged = 0; errors = 0; skipped_reason = None } )
  else
    let token = String.strip cfg.Project_store.github_token in
    if String.is_empty token then
      ( agents,
        {
          attempted = 0;
          newly_merged = 0;
          errors = 0;
          skipped_reason = Some "no GitHub token in stored config";
        } )
    else
      let module Forge =
        (val Github.make ~net ~clock ~token ~owner:cfg.github_owner
               ~repo:cfg.github_repo)
      in
      let results =
        Eio.Fiber.List.map ~max_fibers:16
          (fun (patch_id, pr_number) ->
            (patch_id, pr_number, Forge.pr_state pr_number))
          candidates
      in
      let attempted = List.length results in
      let agents, newly_merged, errors =
        List.fold results ~init:(agents, 0, 0)
          ~f:(fun (agents, merged_count, errs) (patch_id, _pr_number, result) ->
            match result with
            | Error _ -> (agents, merged_count, errs + 1)
            | Ok pr_state ->
                if Pr_state.merged pr_state then
                  let agents =
                    Map.change agents patch_id ~f:(function
                      | None -> None
                      | Some agent -> Some (Patch_agent.mark_merged agent))
                  in
                  (agents, merged_count + 1, errs)
                else (agents, merged_count, errs))
      in
      (agents, { attempted; newly_merged; errors; skipped_reason = None })

let format_refresh_summary (s : refresh_summary) =
  let pr_word n = if n = 1 then "PR" else "PRs" in
  match s.skipped_reason with
  | Some reason -> Some (Stdlib.Printf.sprintf "refresh skipped — %s" reason)
  | None ->
      if s.attempted = 0 then None
      else
        let parts =
          [
            Stdlib.Printf.sprintf "refreshed %d %s" s.attempted
              (pr_word s.attempted);
          ]
          @ (if s.newly_merged > 0 then
               [ Stdlib.Printf.sprintf "%d newly merged" s.newly_merged ]
             else [])
          @
          if s.errors > 0 then
            [
              Stdlib.Printf.sprintf "%d forge error%s" s.errors
                (if s.errors = 1 then "" else "s");
            ]
          else []
        in
        Some (String.concat ~sep:", " parts)

let classify_project ~net ~clock ~refresh ~slug =
  let snap_path = Project_store.snapshot_path slug in
  if not (Stdlib.Sys.file_exists snap_path) then (No_snapshot, None)
  else
    match Persistence.load ~path:snap_path with
    | Error msg -> (Load_error msg, None)
    | Ok snap ->
        let agents = Orchestrator.agents_map snap.Runtime.orchestrator in
        let patches = snap.Runtime.gameplan.Gameplan.patches in
        let agents, refresh_summary =
          if not refresh then (agents, None)
          else
            match Project_store.load_config ~project_name:slug with
            | Error _ ->
                ( agents,
                  Some
                    {
                      attempted = 0;
                      newly_merged = 0;
                      errors = 0;
                      skipped_reason = Some "stored config unreadable";
                    } )
            | Ok cfg ->
                let agents, summary =
                  refresh_agents_from_forge ~net ~clock ~cfg ~agents
                in
                (agents, Some summary)
        in
        let status =
          match Prune_decision.classify_snapshot ~patches ~agents with
          | Prune_decision.All_merged -> All_merged
          | Prune_decision.Not_merged -> Not_merged
          | Prune_decision.No_patches -> No_patches
        in
        (status, refresh_summary)

let worktree_root_for ~project_name =
  let home =
    match Stdlib.Sys.getenv_opt "HOME" with Some h -> h | None -> "."
  in
  Stdlib.Filename.concat home (Stdlib.Filename.concat "worktrees" project_name)

(** Stored config records the gameplan-provided [project_name] verbatim, which
    is what worktrees are keyed by under [~/worktrees/]. The data dir is keyed
    by slug, so the worktree hint requires reading the config to recover the
    original name — falling back to the slug if config is unreadable. *)
let recover_project_name ~slug =
  match Project_store.load_config ~project_name:slug with
  | Ok cfg -> cfg.Project_store.project_name
  | Error _ -> slug

(** Compose a "kept" reason, appending refresh details when they apply. *)
let kept_reason ~base ~refresh_summary =
  match Option.bind refresh_summary ~f:format_refresh_summary with
  | None -> base
  | Some note -> Stdlib.Printf.sprintf "%s (%s)" base note

let run_prune ~net ~clock ~refresh () =
  let slugs = Project_store.list_projects () in
  if List.is_empty slugs then (
    Stdlib.Printf.printf "No stored projects to consider.\n";
    0)
  else
    let removed = ref [] in
    let kept = ref [] in
    let errors = ref [] in
    List.iter slugs ~f:(fun slug ->
        (* [list_projects] returns slugs (already valid path components); the
           data dir is keyed by slug and slugifying a slug is a no-op, so
           passing the slug back through [project_dir] is correct. The
           recovered [project_name] is only used for reporting and the
           worktree hint. *)
        let project_dir = Project_store.project_dir slug in
        let acquired =
          Project_lock.acquire ~project_dir ~on_stale:(fun pid ->
              Stdlib.Printf.eprintf
                "onton prune: reclaimed stale lock for %s (PID %d)\n%!" slug pid)
        in
        match acquired with
        | Error (Project_lock.Held_by { pid; _ }) ->
            let project_name = recover_project_name ~slug in
            kept :=
              (project_name, Stdlib.Printf.sprintf "in use (PID %d)" pid)
              :: !kept
        | Error (Project_lock.Io_error msg) ->
            let project_name = recover_project_name ~slug in
            errors :=
              (project_name, Stdlib.Printf.sprintf "lock error: %s" msg)
              :: !errors
        | Ok lock -> (
            let project_name = recover_project_name ~slug in
            match
              try
                Ok
                  (Stdlib.Fun.protect
                     ~finally:(fun () -> Project_lock.release lock)
                     (fun () ->
                       let status, refresh_summary =
                         classify_project ~net ~clock ~refresh ~slug
                       in
                       (match status with
                       | All_merged -> remove_path project_dir
                       | Not_merged | No_patches | No_snapshot | Load_error _ ->
                           ());
                       (status, refresh_summary)))
              with exn -> Error (Exn.to_string exn)
            with
            | Error msg ->
                errors :=
                  (project_name, Stdlib.Printf.sprintf "prune failed: %s" msg)
                  :: !errors
            | Ok (All_merged, _) -> (
                try
                  removed :=
                    (project_name, worktree_root_for ~project_name) :: !removed
                with exn ->
                  errors := (project_name, Exn.to_string exn) :: !errors)
            | Ok (Not_merged, refresh_summary) ->
                kept :=
                  ( project_name,
                    kept_reason ~base:"has unmerged patches" ~refresh_summary )
                  :: !kept
            | Ok (No_patches, _) ->
                kept := (project_name, "gameplan has no patches") :: !kept
            | Ok (No_snapshot, _) ->
                kept := (project_name, "no snapshot — never ran") :: !kept
            | Ok (Load_error msg, _) ->
                errors :=
                  ( project_name,
                    Stdlib.Printf.sprintf "snapshot load failed: %s" msg )
                  :: !errors));
    List.iter (List.rev !removed) ~f:(fun (n, _) ->
        Stdlib.Printf.printf "removed %s\n" n);
    List.iter (List.rev !kept) ~f:(fun (n, reason) ->
        Stdlib.Printf.printf "kept    %s — %s\n" n reason);
    List.iter (List.rev !errors) ~f:(fun (n, msg) ->
        Stdlib.Printf.eprintf "error   %s — %s\n" n msg);
    Stdlib.Printf.printf "\n%s pruned, %d kept, %s.\n"
      (pluralize (List.length !removed) "project")
      (List.length !kept)
      (pluralize (List.length !errors) "error");
    let leftover_worktrees =
      List.filter !removed ~f:(fun (_, p) -> Stdlib.Sys.file_exists p)
    in
    if not (List.is_empty leftover_worktrees) then (
      Stdlib.Printf.printf
        "\n\
         Worktree directories are not pruned automatically (they may contain \
         build outputs).\n\
         Remove them manually once they are no longer needed:\n";
      List.iter (List.rev leftover_worktrees) ~f:(fun (_, p) ->
          Stdlib.Printf.printf "  rm -rf %s\n" p));
    if not (List.is_empty !errors) then 1 else 0
