open Types

(** The lifecycle state of a patch's GitHub pull request.

    Distinguishes three honest states a patch can occupy along the PR axis,
    eliminating the historical collapse where [Pr_number.t option = None] meant
    both "no PR yet" (bootstrap) and "PR previously existed but is now gone"
    (the remote lost it). That collision was the root cause of a crash in
    {!Patch_controller.reconcile_messages}: an ad-hoc agent whose remote PR
    vanished was [clear_pr]-ed back to [None], leaving the orchestrator's graph
    holding a node with neither gameplan nor PR.

    Do not confuse this module with {!Pr_state}, which is the forge-agnostic
    view of a single PR's GitHub-side facts (status, merge_state, ci_checks,
    findings, ...). [Patch_pr_status] is strictly the patch agent's own
    lifecycle status. *)

type t =
  | Absent  (** No PR has been opened for this patch yet. *)
  | Present of Pr_number.t  (** PR is open and tracked. *)
  | Missing of Pr_number.t
      (** PR previously existed and the remote no longer has it. The agent is
          surfaced for intervention; the recorded number lets the poller adopt
          the PR back if it reappears, and the TUI can tell the operator which
          PR it was. *)
[@@deriving show, eq, sexp_of, compare]

(** {2 Predicates} *)

val has_pr : t -> bool
(** [true] for [Present] and [Missing] — the orchestrator-invariant sense: "we
    know a PR identity for this patch." Drives the
    {!Patch_controller.reconcile_messages} invariant that every graph node must
    be in the gameplan or have a PR identity. *)

val is_pr_present : t -> bool
(** [true] only for [Present] — the functional sense: "the PR can be rebased /
    responded to / merged." Callers about to call GitHub against the recorded
    number should gate on this, not on [has_pr]. *)

val is_missing : t -> bool
(** [true] only for [Missing]. *)

val pr_number : t -> Pr_number.t option
(** The recorded PR number, if any. [Some] for both [Present] and [Missing];
    [None] for [Absent]. *)

(** {2 Smart constructors} *)

val set_present : t -> Pr_number.t -> t
(** Move to [Present n]. Total: accepts any source state. Used by all paths
    where a PR has been observed on the remote — first discovery
    ([Absent → Present]), re-discovery after a vanish ([Missing → Present]), and
    replacement renumbering ([Present → Present]). *)

val clear_for_recreate : t -> t
(** Move to [Absent]. Partial: raises [Invalid_argument] on [Absent] or
    [Missing]. Used only by the gameplan-recreate path: when a gameplan patch's
    PR has been closed and we want the next [Start] to open a fresh one. Calling
    this on [Missing] would erase the user-visible history of which PR vanished;
    use {!mark_missing} for that case instead. *)

val mark_missing : t -> t
(** Move to [Missing n] preserving the recorded number. Partial: requires
    [Present _]; raises [Invalid_argument] on [Absent] (cannot lose what was
    never had) and on [Missing _] (idempotency is the caller's responsibility —
    re-marking would discard the original observation). *)

(** {2 Persistence} *)

val yojson_of_t : t -> Yojson.Safe.t
(** Tagged-variant encoding: emits [\{"kind": "absent"\}] /
    [\{"kind": "present", "pr_number": n\}] /
    [\{"kind": "missing", "pr_number": n\}]. *)

val t_of_yojson_compat : Yojson.Safe.t -> (t, string) Result.t
(** Decode tagged form, with backward-compatible fall-backs:
    - [`Null] → [Absent] (legacy [pr_number = null]).
    - bare integer or any value decodable by [Pr_number.t_of_yojson] →
      [Present n] (legacy [pr_number = n]).
    - unrecognized JSON → [Error _].

    Wraps raises from [Pr_number]'s ppx-derived decoder. *)
