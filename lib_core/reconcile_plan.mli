(* @archlint.module interface
   @archlint.domain reconcile-plan *)

(** Planning describes outstanding work independently of the command or agent
    currently trying to satisfy it. Diagnostic strings are not planning inputs.
*)
type obligation = Preserve_work | Finish_work | Integrate | Publish
[@@deriving eq, compare, sexp_of]

type strategy =
  | Observe_evidence
  | Deterministic
  | Content_repair
  | Agent_required
[@@deriving eq, compare, sexp_of]

type readiness =
  | Satisfied
  | Unobserved
  | Required of {
      strategy : strategy;
      deterministic_failures : int;
      content_failures : int;
      recovery_failures : int;
    }
[@@deriving eq, compare, sexp_of]

val required :
  ?deterministic_failures:int ->
  ?content_failures:int ->
  ?recovery_failures:int ->
  strategy ->
  readiness
(** Attempt history belongs to the outstanding obligation. A verified completion
    replaces it with [Satisfied]; its budget cannot block later obligations. *)

type authority = Held | Observe_only | Manage
[@@deriving eq, compare, sexp_of]

type execution = Available | In_flight | Backoff | Agent_result of obligation
[@@deriving eq, compare, sexp_of]

type t = {
  authority : authority;
  execution : execution;
  preserve_work : readiness;
  finish_work : readiness;
  integrate : readiness;
  publish : readiness;
}
[@@deriving eq, compare, sexp_of]

type hold = User_hold | Recovery_budget_exhausted
[@@deriving eq, compare, sexp_of]

type agent_scope = Content_only | Full_recovery | Diagnosis
[@@deriving eq, compare, sexp_of]

type decision =
  | Settled
  | Wait
  | Observe
  | Execute of obligation
  | Run_agent of obligation * agent_scope
  | Verify_agent of obligation
  | Hold of hold
[@@deriving eq, compare, sexp_of]

val for_obligation : authority:authority -> obligation -> readiness -> t

val next : t -> decision
(** Fixed prerequisite order: retain work, finish pending local work, integrate,
    publish. A non-required obligation is [Satisfied]. In particular, publishing
    an already captured commit need not require cleaning its checkout.

    An unavailable deterministic strategy is [Agent_required], not a terminal
    failure. An agent result must be observed and verified before another
    action; it cannot assert that an obligation has been satisfied. Explicit
    authority and exhausted recovery are distinct from the checkout's readiness.
*)
