(* @archlint.module interface
   @archlint.domain push-plan *)

(** Validate publication at the owning Git boundary. A force-push action carries
    the exact local commit and incorporated remote commit it authorizes. Reading
    a newer tracking ref alone cannot authorize overwriting it: its commit must
    be reachable from the captured local commit or have its changes represented
    in that commit's rewritten history. The push uses an explicit SHA lease, so
    later fetches cannot change the authority and concurrent remote writes are
    rejected by Git. History-preserving publication additionally requires the
    captured remote commit to be an ancestor of the captured local commit. *)

type sha = string [@@deriving show, eq, sexp_of, compare]

(** Two-way ancestor relationship between [refs/heads/<branch>] and
    [refs/remotes/origin/<branch>], as observed by the caller. *)
type ancestry =
  | Local_includes_remote
      (** Local is at or ahead of remote — safe to force-push. *)
  | Local_missing_remote
      (** Remote has commits the local branch does not reach — a force-push
          would lose them. *)
  | Local_diverged_from_remote
      (** Local and remote each have commits the other does not reach. *)
  | No_remote_yet  (** The branch has never been pushed. *)
  | Unknown  (** Ancestry could not be determined; treat conservatively. *)
[@@deriving show, eq, sexp_of, compare]

type action = private
  | Force_push_with_lease of { local_sha : sha; remote_sha : sha }
  | Initial_push of { local_sha : sha }
[@@deriving show, eq, sexp_of, compare]

type refusal =
  | No_commits_ahead_of_base
      (** The named branch has no commits beyond [base]. GitHub would reject the
          push with "everything up-to-date" or accept a no-op; skip it. *)
  | Worktree_missing
      (** The worktree directory was deleted out from under the supervisor;
          there is no local state to push from. *)
  | Branch_ref_missing of { branch : string }
      (** [refs/heads/<branch>] does not exist locally — we cannot push it. *)
  | Branch_switched of { expected : string; got : string option }
      (** The worktree's current HEAD does not match [expected_branch]. An agent
          switched to a different branch mid-session, so the gate's commit count
          does not represent what the push command would upload. *)
  | Local_missing_remote_commits of { local_sha : sha; remote_sha : sha }
      (** [ancestry = Local_missing_remote] — pushing now would force-push a
          local that is strictly behind remote on real content, wiping commits.
          [ancestry = Local_diverged_from_remote] is intentionally NOT a refusal
          here when every remote-only commit is patch-equivalent to a commit
          reachable from the captured local SHA. *)
  | History_would_be_rewritten of { local_sha : sha; remote_sha : sha }
      (** History-preserving publication requires the captured remote tip to
          remain an ancestor of the captured local tip. Patch equivalence does
          not authorize replacing published ancestry. *)
  | Remote_not_integrated of { remote_sha : sha }
      (** The observed remote commit is absent from both current ancestry and
          current rewritten history. Retry after incorporating remote work. *)
[@@deriving show, eq, sexp_of, compare]

type decision = Push of action | Refuse of refusal
[@@deriving show, eq, sexp_of, compare]

val plan :
  preserve_history:bool ->
  expected_branch:string ->
  worktree_path_exists:bool ->
  worktree_head_branch:string option ->
  branch_ref_sha:sha option ->
  remote_tracking_sha:sha option ->
  ancestry:ancestry ->
  remote_changes_included:bool ->
  commits_ahead_of_base:int option ->
  decision
(** Total and deterministic. Missing worktrees, switched/missing branches, empty
    patches, and strictly-behind branches are refused before publication.
    Divergent history requires [remote_changes_included]: every remote-only
    commit is patch-equivalent to a commit in the captured current history.
    [preserve_history = true] only authorizes updates with
    [ancestry = Local_includes_remote], regardless of patch equivalence. Unknown
    ancestry fails closed in the Git handler. *)

val short_label : decision -> string
(** A short, lowercase, snake_case identifier for the planner arm that fired,
    suitable for the activity log. Always non-empty and ≤ 32 characters.
    Examples: ["force_push"], ["initial_push"], ["refuse_no_commits"],
    ["refuse_wt_missing"], ["refuse_ref_missing"], ["refuse_branch_switched"],
    ["refuse_local_behind"]. *)

val to_push_reject_classify_rejection :
  refusal -> Push_reject_classify.rejection option
(** Map planner refusals onto the rejection variant used by the orchestrator's
    permanent-rejection escalation:

    - [Branch_switched] / [Local_missing_remote_commits] / [Branch_ref_missing]
      / [History_would_be_rewritten] →
      [Some (Local_state_unsafe { reason = short_label_of_refusal })] — route
      through [needs_intervention].
    - [Remote_not_integrated] → [Some Lease_violation] for incorporation/retry.
    - [No_commits_ahead_of_base] / [Worktree_missing] → [None] — the
      orchestrator already has dedicated non-rejection handlers
      ([Push_no_commits], [Push_worktree_missing]). *)
