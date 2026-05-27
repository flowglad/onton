open Base

type sha = string [@@deriving show, eq, sexp_of, compare]

type ancestry = Local_ahead | Remote_ahead | Equal | Diverged | Unknown
[@@deriving show, eq, sexp_of, compare]

type action =
  | Reset_and_use_remote_tracking of { remote_sha : sha }
  | Use_local_branch_unchanged of { local_sha : sha }
  | Create_new_branch_from_base of { base_branch : string }
[@@deriving show, eq, sexp_of, compare]

type refusal =
  | Local_diverged_from_remote of { local_sha : sha; remote_sha : sha }
  | Local_has_unpushed_commits of { local_sha : sha; remote_sha : sha }
  | Branch_checked_out_in_main_root
  | Worktree_already_registered of { existing_path : string }
[@@deriving show, eq, sexp_of, compare]

type decision = Plan of action | Refuse of refusal
[@@deriving show, eq, sexp_of, compare]

let plan ~local_ref ~remote_ref ~ancestry ~base_branch
    ~branch_checked_out_in_main_root ~existing_worktree_path =
  if branch_checked_out_in_main_root then Refuse Branch_checked_out_in_main_root
  else
    match existing_worktree_path with
    | Some existing_path ->
        Refuse (Worktree_already_registered { existing_path })
    | None -> (
        match (local_ref, remote_ref) with
        | None, None ->
            (* Cut a brand-new branch from [base_branch]. Whether the base is up
               to date with [origin/<main>] is intentionally not gated here:
               freshness is dependency-scoped and enforced by the orchestrator's
               scheduling gate (see {!Start_eligibility}). A base lagging main
               for unrelated reasons must not block the cut. *)
            Plan (Create_new_branch_from_base { base_branch })
        | None, Some remote_sha ->
            Plan (Reset_and_use_remote_tracking { remote_sha })
        | Some local_sha, None ->
            Plan (Use_local_branch_unchanged { local_sha })
        | Some local_sha, Some remote_sha -> (
            match ancestry with
            | Equal | Remote_ahead ->
                Plan (Reset_and_use_remote_tracking { remote_sha })
            | Local_ahead ->
                Refuse (Local_has_unpushed_commits { local_sha; remote_sha })
            | Diverged | Unknown ->
                Refuse (Local_diverged_from_remote { local_sha; remote_sha })))

let short_label = function
  | Plan (Reset_and_use_remote_tracking _) -> "reset_to_remote"
  | Plan (Use_local_branch_unchanged _) -> "use_local_unchanged"
  | Plan (Create_new_branch_from_base _) -> "create_from_base"
  | Refuse (Local_diverged_from_remote _) -> "refuse_local_diverged"
  | Refuse (Local_has_unpushed_commits _) -> "refuse_local_ahead"
  | Refuse Branch_checked_out_in_main_root -> "refuse_main_checkout"
  | Refuse (Worktree_already_registered _) -> "refuse_wt_registered"
