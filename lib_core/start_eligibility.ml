(* @archlint.module core
   @archlint.domain start-eligibility *)

open Base

type defer_reason =
  | Base_patch_busy_with_rebase of { base_branch : string }
  | Base_resolving_conflict of { base_branch : string }
  | Base_not_fresh_for_cut of { base_branch : string }
  | Base_missing_merged_sibling of { base_branch : string }
[@@deriving show, eq, sexp_of, compare]

type decision = Allow | Defer of defer_reason
[@@deriving show, eq, sexp_of, compare]

let decide ~base_is_main ~base_branch ~base_patch_merged
    ~base_patch_busy_rebasing ~base_patch_has_conflict ~base_structurally_fresh
    ~base_contains_merged_siblings =
  if base_is_main then Allow
  else if base_patch_merged then Allow
  else if base_patch_busy_rebasing then
    (* An active/imminent rebase is the most informative signal — wait for it
       rather than racing it. *)
    Defer (Base_patch_busy_with_rebase { base_branch })
  else if base_patch_has_conflict then
    (* The base's branch tip is pending a rewrite: a rebase of the base hit
       conflicts (or GitHub reports its PR conflicting with its own base), and
       the conflict-resolution pipeline (Merge_conflict respond → resolution
       rebase → force push) will replace the commits a cut taken now would
       build on. The [Rebase] op itself already completed when it conflicted —
       [base_patch_busy_rebasing] is false — so without this arm the gate
       reopens mid-pipeline and the downstream worktree is cut from the doomed
       pre-rebase tip. A same-name freshen rebase (e.g. main → newer main) is
       invisible to the structural check below, so this arm is the only thing
       holding the gate through that window. *)
    Defer (Base_resolving_conflict { base_branch })
  else if not base_structurally_fresh then
    (* The base patch's local branch is not yet sitting on its
       structurally-correct base (a dependency merged but the rebase that
       absorbs it has not landed), so cutting a downstream worktree here would
       elide the just-merged ancestor's commits. *)
    Defer (Base_not_fresh_for_cut { base_branch })
  else if not base_contains_merged_siblings then
    (* The base is structurally settled on its own dependency lineage, but the
       patch being started/rebased fans in on a *sibling* dependency that has
       merged to main independently of this base. The base's branch does not
       yet contain that sibling's squash commit, so cutting/rebasing here would
       start from a base missing one of the patch's dependencies. This flag is
       computed against the *launching patch's* full merged-dep set (not the
       base patch's own deps), so an arbitrarily deep fan-in gap cannot slip
       through: the base must be freshened first. *)
    Defer (Base_missing_merged_sibling { base_branch })
  else Allow

let short_label = function
  | Allow -> "allow"
  | Defer (Base_patch_busy_with_rebase _) -> "defer_base_busy_rebasing"
  | Defer (Base_resolving_conflict _) -> "defer_base_conflicted"
  | Defer (Base_not_fresh_for_cut _) -> "defer_base_stale"
  | Defer (Base_missing_merged_sibling _) -> "defer_base_missing_sibling"
