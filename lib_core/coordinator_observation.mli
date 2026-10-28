(* @archlint.module interface
   @archlint.domain coordinator-forge *)
open Types

type scope = {
  ticket : string;
  pr : Pr_number.t;
  head_branch : Branch.t;
  base_branch : Branch.t;
  head_revision : string;
  base_revision : string;
  push_destination : string;
  generation : int;
}
[@@deriving yojson]

type t = private {
  scope : scope;
  state : Pr_state.t option;
  diagnostic : string;
  removal_checks : Types.Ci_check.t list option;
  observed_at : float;
}
[@@deriving yojson]

val make :
  ?removal_checks:Types.Ci_check.t list ->
  ?observed_at:float ->
  scope ->
  Pr_state.t option ->
  diagnostic:string ->
  (t, string) result

val encode : t -> string
val decode : string -> (t, string) result

val apply : Patch_agent.t -> t -> Patch_agent.t
(** Total, revision/generation-scoped owner ingestion. Unknown/unavailable
    observations defer readiness, not conflict history. All policy inputs and
    feedback queues come from the accepted snapshot, never a candidate flag. *)
