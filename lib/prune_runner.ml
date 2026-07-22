(* @archlint.module shell
   @archlint.domain orchestrator *)

open Base
open Types

type prune_status =
  | All_terminal
  | Not_terminal
  | No_patches
  | No_snapshot
  | Load_error of string

type refresh_summary = {
  attempted : int;
  newly_merged : int;
  closed : int;
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
    the forge: agent stored as not-merged AND has a PR number on file. Both
    present and missing PRs are included: a missing PR may be a closed PR, which
    is terminal for pruning. *)
let refresh_candidates (agents : Patch_agent.t Map.M(Patch_id).t) =
  Map.fold agents ~init:[] ~f:(fun ~key:patch_id ~data:agent acc ->
      if agent.Patch_agent.merged then acc
      else
        match Patch_agent.pr_number agent with
        | None -> acc
        | Some pr_number -> (patch_id, pr_number) :: acc)

(** Refresh [agents] by querying the forge for every non-terminal patch with a
    recorded PR number. Returns the (possibly updated) agents map and a
    [refresh_summary], plus patch IDs whose PRs are closed. Forge errors are
    tolerated — the corresponding agent is left untouched and non-terminal. *)
let refresh_agents_from_forge
    ~(make_forge :
       owner:string ->
       repo:string ->
       main_branch:Branch.t ->
       (module Forge.S with type error = Github.error) option)
    ~(cfg : Project_store.stored_config)
    ~(agents : Patch_agent.t Map.M(Patch_id).t) =
  let candidates = refresh_candidates agents in
  if List.is_empty candidates then
    ( agents,
      [],
      {
        attempted = 0;
        newly_merged = 0;
        closed = 0;
        errors = 0;
        skipped_reason = None;
      } )
  else
    match
      make_forge ~owner:cfg.github_owner ~repo:cfg.github_repo
        ~main_branch:(Types.Branch.of_string cfg.main_branch)
    with
    | None ->
        ( agents,
          [],
          {
            attempted = 0;
            newly_merged = 0;
            closed = 0;
            errors = 0;
            skipped_reason =
              Some
                "no GitHub token available (pass --token, set GITHUB_TOKEN, or \
                 log in with `gh`)";
          } )
    | Some forge ->
        let module Forge = (val forge) in
        let results =
          Eio.Fiber.List.map ~max_fibers:16
            (fun (patch_id, pr_number) ->
              let result =
                try
                  Result.map_error (Forge.pr_state pr_number) ~f:(fun _ -> ())
                with
                | Eio.Cancel.Cancelled _ as exn -> raise exn
                | _ -> Error ()
              in
              (patch_id, pr_number, result))
            candidates
        in
        let attempted = List.length results in
        let agents, closed_patch_ids, newly_merged, errors =
          List.fold results ~init:(agents, [], 0, 0)
            ~f:(fun
                (agents, closed_ids, merged_count, errs)
                (patch_id, _pr_number, result)
              ->
              match result with
              | Error _ -> (agents, closed_ids, merged_count, errs + 1)
              | Ok pr_state ->
                  if Pr_state.merged pr_state then
                    let agents =
                      Map.change agents patch_id ~f:(function
                        | None -> None
                        | Some agent ->
                            (* Record the merge-commit SHA we already have in
                             hand so dependents' base-containment gate can
                             ancestry-check it without waiting for a re-poll. *)
                            Some
                              (Patch_agent.set_merge_commit_sha
                                 (Patch_agent.mark_merged agent)
                                 pr_state.Pr_state.merge_commit_sha))
                    in
                    (agents, closed_ids, merged_count + 1, errs)
                  else if Pr_state.closed pr_state then
                    (agents, patch_id :: closed_ids, merged_count, errs)
                  else (agents, closed_ids, merged_count, errs))
        in
        ( agents,
          closed_patch_ids,
          {
            attempted;
            newly_merged;
            closed = List.length closed_patch_ids;
            errors;
            skipped_reason = None;
          } )

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
          @ (if s.closed > 0 then [ Stdlib.Printf.sprintf "%d closed" s.closed ]
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

let classify_project ~make_forge ~refresh ~slug =
  let snap_path = Project_store.snapshot_path slug in
  if not (Stdlib.Sys.file_exists snap_path) then (No_snapshot, None)
  else
    match Persistence.load ~path:snap_path with
    | Error msg -> (Load_error msg, None)
    | Ok snap ->
        let agents = Orchestrator.agents_map snap.Runtime.orchestrator in
        let patches = snap.Runtime.gameplan.Gameplan.patches in
        let agents, closed_patch_ids, refresh_summary =
          if not refresh then (agents, [], None)
          else
            match Project_store.load_config ~project_name:slug with
            | Error _ ->
                ( agents,
                  [],
                  Some
                    {
                      attempted = 0;
                      newly_merged = 0;
                      closed = 0;
                      errors = 0;
                      skipped_reason = Some "stored config unreadable";
                    } )
            | Ok cfg ->
                let agents, closed_patch_ids, summary =
                  refresh_agents_from_forge ~make_forge ~cfg ~agents
                in
                (agents, closed_patch_ids, Some summary)
        in
        let status =
          match
            Prune_decision.classify_snapshot ~patches ~agents ~closed_patch_ids
          with
          | Prune_decision.All_terminal -> All_terminal
          | Prune_decision.Not_terminal -> Not_terminal
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

let run_prune ~net ~clock ~github_token ~refresh () =
  let slugs = Project_store.list_projects () in
  if List.is_empty slugs then (
    Stdlib.Printf.printf "No stored projects to consider.\n";
    0)
  else
    (* Resolve the GitHub token once for the whole prune run. Prefer the
       explicit CLI/env value, then fall back to [gh auth token]; tokens are no
       longer persisted per project. Skipped under [--no-refresh] since no forge
       queries happen. [make_forge] closes over the run-wide [net]/[clock]/
       [token] so per-project classification only supplies the
       owner/repo/branch that actually vary, and yields [None] when no token is
       available so the refresh is reported as skipped rather than failing. *)
    let token =
      if not refresh then ""
      else
        let explicit = String.strip github_token in
        if String.is_empty explicit then Managed_repo.infer_github_token ()
        else explicit
    in
    let make_forge ~owner ~repo ~main_branch =
      if String.is_empty token then None
      else Some (Github.make ~net ~clock ~token ~owner ~repo ~main_branch)
    in
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
                         classify_project ~make_forge ~refresh ~slug
                       in
                       (match status with
                       | All_terminal -> remove_path project_dir
                       | Not_terminal | No_patches | No_snapshot | Load_error _
                         ->
                           ());
                       (status, refresh_summary)))
              with
              | Eio.Cancel.Cancelled _ as exn -> raise exn
              | exn -> Error (Exn.to_string exn)
            with
            | Error msg ->
                errors :=
                  (project_name, Stdlib.Printf.sprintf "prune failed: %s" msg)
                  :: !errors
            | Ok (All_terminal, refresh_summary) -> (
                try
                  let refresh_note =
                    Option.bind refresh_summary ~f:format_refresh_summary
                  in
                  removed :=
                    (project_name, worktree_root_for ~project_name, refresh_note)
                    :: !removed
                with exn ->
                  errors := (project_name, Exn.to_string exn) :: !errors)
            | Ok (Not_terminal, refresh_summary) ->
                kept :=
                  ( project_name,
                    kept_reason ~base:"has non-terminal patches"
                      ~refresh_summary )
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
    List.iter (List.rev !removed) ~f:(fun (n, _, refresh_note) ->
        match refresh_note with
        | None -> Stdlib.Printf.printf "removed %s\n" n
        | Some note -> Stdlib.Printf.printf "removed %s — %s\n" n note);
    List.iter (List.rev !kept) ~f:(fun (n, reason) ->
        Stdlib.Printf.printf "kept    %s — %s\n" n reason);
    List.iter (List.rev !errors) ~f:(fun (n, msg) ->
        Stdlib.Printf.eprintf "error   %s — %s\n" n msg);
    Stdlib.Printf.printf "\n%s pruned, %d kept, %s.\n"
      (pluralize (List.length !removed) "project")
      (List.length !kept)
      (pluralize (List.length !errors) "error");
    let leftover_worktrees =
      List.filter !removed ~f:(fun (_, p, _) -> Stdlib.Sys.file_exists p)
    in
    if not (List.is_empty leftover_worktrees) then (
      Stdlib.Printf.printf
        "\n\
         Worktree directories are not pruned automatically (they may contain \
         build outputs).\n\
         Remove them manually once they are no longer needed:\n";
      List.iter (List.rev leftover_worktrees) ~f:(fun (_, p, _) ->
          Stdlib.Printf.printf "  rm -rf %s\n" p));
    if not (List.is_empty !errors) then 1 else 0
