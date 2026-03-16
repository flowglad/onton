open Types

(** Startup reconciliation: discover existing GitHub PRs at launch.

    When onton starts fresh (no persisted state), each patch agent begins with
    [has_pr = false]. If PRs already exist from a previous run, the startup
    reconciler queries GitHub to discover them and returns updates to apply
    before the main loop begins. This avoids re-spawning Claude for patches that
    already have open or merged PRs. *)

type pr_discovery = {
  patch_id : Patch_id.t;
  pr_number : Pr_number.t;
  base_branch : Branch.t;
  merged : bool;
}
[@@deriving show, eq]
(** A discovered PR for a patch, including the PR's base ref. *)

type t = { discovered : pr_discovery list; errors : (Patch_id.t * string) list }
[@@deriving show, eq]
(** Result of startup reconciliation. [errors] contains per-patch discovery
    failures with diagnostic messages. *)

val reconcile :
  process_mgr:_ Eio.Process.mgr ->
  token:string ->
  owner:string ->
  repo:string ->
  patches:Patch.t list ->
  t
(** [reconcile ~process_mgr ~token ~owner ~repo ~patches] queries GitHub for
    each patch's branch to find existing PRs. For each found PR, queries its
    merge status and base ref. CLOSED PRs are skipped. Iterates through all
    matching PRs to find the first non-CLOSED one. Per-patch errors are
    collected in [errors] rather than silently dropped. *)
