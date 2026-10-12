(* @archlint.module interface
   @archlint.domain session-result *)

(** Session outcomes, independent of the effectful session driver. Legacy push
    outcomes remain for callers awaiting reconciliation cutover; a backend
    completion is recorded before publication can change their disposition. *)
type t =
  | Session_ok
  | Session_process_error of { is_fresh : bool; detail : string option }
  | Session_no_resume
  | Session_timed_out of { session_id : string option; detail : string option }
      (** Deadline interruption: preserve the session and retry without
          consuming the fresh/resume failure budget. [session_id] is the ID
          captured by this attempt, or its resumed ID; [None] clears any stale
          ID from a previous failed attempt. *)
  | Session_failed of { is_fresh : bool; detail : string option }
  | Session_wontdo of string
      (** Explicit pre-commit opt-out. Retains the explanation and pauses work
          until a human reprompt or intervention reset. *)
  | Session_give_up
  | Session_no_commits
  | Session_context_exhausted
      (** The session exhausted the model's context window
          ([Run_classification.Context_exhausted]). Clears [llm_session_id] so
          the next session starts fresh (resuming the overflowed thread would
          re-overflow) and bumps [context_exhaustion_count]; at [>= 2] the agent
          surfaces for intervention. *)
[@@deriving show, eq, sexp_of, compare]

type delivery_mode = Start | Respond [@@deriving show, eq, sexp_of, compare]

type completion = {
  session_uuid : string;
  delivery_mode : delivery_mode;
  kind : Types.Operation_kind.t option;
  message_id : Types.Message_id.t option;
  result : t;
  head : string option;
  guidance : string list;
  turn_accepted : bool;
}
[@@deriving eq, sexp_of, compare]

val yojson_of_completion : completion -> Yojson.Safe.t
val decode_completion : Yojson.Safe.t -> (completion, string) result

val publication_intent :
  completion ->
  base:string ->
  policy:Branch_reconcile.policy ->
  Branch_reconcile.intent

val resume_start :
  delivery_mode:delivery_mode ->
  guidance:string list ->
  publication:Branch_reconcile.t ->
  completion ->
  bool
(** Reuse completed implementation only for the same delivered guidance. An
    observed no-work publication permits another implementation attempt. *)

val resume_verified_legacy_start :
  delivery_mode:delivery_mode ->
  guidance:string list ->
  publication:Branch_reconcile.t ->
  completion:completion option ->
  bool
(** A confirmed legacy publication without a saved completion can resume PR
    creation. New guidance or a known backend outcome requires normal handling.
*)

val after_local_work :
  delivery_mode:delivery_mode -> branch_changed:bool -> no_work:bool -> t -> t
(** Classify local progress independently of transport/publication outcomes. *)
