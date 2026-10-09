(* @archlint.module interface
   @archlint.domain branch-reconcile *)

type failure = Temporary of string | Unsafe of string

val message : failure -> string
val reconciliation_result : failure -> Branch_reconcile.result
val materialization_failure : string -> failure
val creation_failure : Start_point_plan.refusal -> failure

type decision =
  | Ready
  | Wait of string
  | Stop of string
  | Run of Branch_reconcile.event

val next :
  intent:Branch_reconcile.intent -> at:float -> Branch_reconcile.t -> decision
(** Resume an unfinished provisioning request without superseding other owned
    work or bypassing backoff. Once it settles, a different request must inspect
    again. A terminal owner intervention requires an explicit resume. An empty
    request identity or base is rejected before emitting an owner request. *)
