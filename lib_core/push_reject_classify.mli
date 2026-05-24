(** Pure classifier for [git push] server-side rejection messages.

    [classify_push_result] in [Worktree_parser] decides that a push was rejected
    (porcelain [!]); this module decides {e why}. The wording lives entirely in
    the captured stderr, since [git push --porcelain] strips it out of stdout.
    Distinguishing causes matters because:

    - [Workflow_scope_missing] / [Branch_protection] / [Push_pattern_block] /
      [Hook_failure] are {e permanent} under the current credentials — retrying
      will just hit the same wall. The orchestrator escalates these directly to
      [needs_intervention] instead of looping. See [is_permanent].
    - [Lease_violation] is a real race (the remote ref advanced between fetch
      and push). Retrying after a re-fetch is the correct response.
    - [Unknown] is anything we don't recognize; treated conservatively as
      transient so we don't accidentally trip intervention on novel server
      messages. *)

type rejection =
  | Workflow_scope_missing
      (** [refusing to allow an OAuth App to create or update workflow] — the
          token lacks the [workflow] OAuth scope and the push includes a change
          to a file under [.github/workflows/]. *)
  | Branch_protection
      (** [GH006: Protected branch update failed] /
          [protected branch hook declined] — a branch protection rule rejected
          the push. *)
  | Push_pattern_block
      (** [push declined due to repository rule violations] — a repository
          push-pattern ruleset (org-level or repo-level) rejected the ref. *)
  | Lease_violation
      (** [stale info] / [fetch first] — the remote ref moved between the lease
          read and the push; a re-fetch will resolve it. *)
  | Hook_failure of string
      (** Any other [remote: …] message preceding [! [remote rejected]]; the
          excerpt is preserved verbatim for the activity log. *)
  | Unknown of string
      (** Nothing recognizable in stderr; the excerpt is preserved (truncated to
          200 chars) so the user has a starting point for diagnosis. *)
[@@deriving show, eq, sexp_of, compare]

val classify : stderr:string -> stdout:string -> rejection
(** Decide why a push was rejected, given the captured stderr and stdout from
    [git push --porcelain --force-with-lease]. Total over arbitrary input.
    Stdout is currently unused but accepted so future porcelain hints (e.g. the
    parenthesized reason on the [!] line) can be folded in without churning call
    sites. *)

val short_label : rejection -> string
(** A short, lowercase, snake_case label suitable for an activity-log line
    ("workflow_scope_missing", "branch_protection", etc.). Always non-empty and
    ≤ 32 chars. *)

val detail_excerpt : rejection -> string option
(** The server-supplied detail line, if any (truncated to 200 chars). [None] for
    variants that carry no payload ([Lease_violation], the named variants). Used
    by callers that want to emit a follow-up activity-log line with the raw
    server message. *)

val is_permanent : rejection -> bool
(** [true] for rejections that will not resolve on retry under the current
    credentials and branch state ([Workflow_scope_missing], [Branch_protection],
    [Push_pattern_block], [Hook_failure]); [false] for [Lease_violation]
    (genuine race) and [Unknown] (conservative — we don't escalate on something
    we don't understand). The orchestrator uses this to short-circuit the
    push-failure counter and flip directly to intervention. *)
