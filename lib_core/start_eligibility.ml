open Base

type defer_reason =
  | Base_patch_busy_with_rebase of { base_branch : string }
  | Base_not_fresh_for_cut of { base_branch : string }
[@@deriving show, eq, sexp_of, compare]

type decision = Allow | Defer of defer_reason
[@@deriving show, eq, sexp_of, compare]

let decide ~base_is_main ~base_branch ~base_patch_merged
    ~base_patch_busy_rebasing ~base_structurally_fresh =
  if base_is_main then Allow
  else if base_patch_merged then Allow
  else if base_patch_busy_rebasing then
    (* An active/imminent rebase is the most informative signal — wait for it
       rather than racing it. *)
    Defer (Base_patch_busy_with_rebase { base_branch })
  else if not base_structurally_fresh then
    (* The base patch's local branch is not yet sitting on its
       structurally-correct base (a dependency merged but the rebase that
       absorbs it has not landed), so cutting a downstream worktree here would
       elide the just-merged ancestor's commits. *)
    Defer (Base_not_fresh_for_cut { base_branch })
  else Allow

let short_label = function
  | Allow -> "allow"
  | Defer (Base_patch_busy_with_rebase _) -> "defer_base_busy_rebasing"
  | Defer (Base_not_fresh_for_cut _) -> "defer_base_stale"
