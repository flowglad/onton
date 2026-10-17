(* @archlint.module interface
   @archlint.domain merge-queue-decision *)

type pending = private
  | Awaiting_removal of {
      entry : Pr_state.merge_queue_entry;
      head_oid : string option;
    }
  | Failed of { head_oid : string option; checks : Types.Ci_check.t list }
[@@deriving show, eq, sexp_of, compare, yojson]

type application = {
  poll_result : Poller.t;
  merge_queue_ejected : bool;
  pending : pending option;
}
[@@deriving show, eq]

val queue_entry_disappeared :
  previous_entry:Pr_state.merge_queue_entry option -> Poller.t -> bool
(** Detect disappearance of an observed queue entry, independently of readiness.
    The removal lookup distinguishes check failures from other removals. *)

val should_fetch :
  pending:pending option ->
  previous_entry:Pr_state.merge_queue_entry option ->
  Poller.t ->
  bool
(** Retry unresolved removal lookups even after the live queue entry is gone. *)

val failed_checks : pending option -> Types.Ci_check.t list

val repair_evidence :
  pending:pending option ->
  previous_head_oid:string option ->
  head_oid:string option ->
  checks:Types.Ci_check.t list ->
  Types.Ci_check.t list * Types.Ci_check.t list
(** Synthetic markers and retained failures for a repair turn, only when the
    recorded head matches the freshly observed head. Legacy markers use the
    previously observed PR head. *)

val repair_completed : pending option -> pending option
(** A successfully completed CI delivery consumes confirmed failure evidence.
    Unresolved removal lookups still require ingestion. *)

val apply :
  ?pending:pending option ->
  ?previous_head_oid:string option ->
  ?removal_checks:Types.Ci_check.t list ->
  previous_entry:Pr_state.merge_queue_entry option ->
  Poller.t ->
  application
(** A missing lookup result retains unresolved state. An empty successful lookup
    resolves a non-CI removal; failures remain available for repair until a
    different head is observed or CI repair completes successfully. Terminal PRs
    clear the state. *)

val removal_processed : body:string -> checks:Types.Ci_check.t list -> bool
(** Whether the removal response and resolved checks supply enough evidence to
    finish ingestion. Unknown reasons, missing events and failed-check reasons
    without checks retry. *)

type removal_evidence = Missing | Non_failure | Needs_checks | Unknown_reason

val removal_evidence : body:string -> removal_evidence
(** Classify the removal event before requesting additional checks. Known
    non-failure removals must not inherit failures from older Actions runs.
    Unrecognized nonempty reasons remain unresolved regardless of checks. *)

val accept_removal_repair : pending option -> pending option
(** Discharge unknown removal evidence after an accepted scheduler diagnosis.
    Known failing checks remain owned by the CI repair path. *)
