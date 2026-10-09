(* @archlint.module interface
   @archlint.domain forge-observation *)

val read : (module Forge.S) -> Forge_observation.request -> Forge_observation.t
(** Keep the caller's request identity attached across the provider call. *)

type prepared = {
  patch_id : Types.Patch_id.t;
  requested : Patch_agent.t;
  ticket : Forge_observation.ticket;
}

val prepare :
  runtime:Runtime.t ->
  persist:(Runtime.snapshot -> (unit, string) result) ->
  (Types.Patch_id.t * Patch_agent.t * Forge_observation.request) list ->
  (prepared list, string) result
(** Checkpoint the batch of still-current requests before exposing any for
    network I/O. Persistence failure leaves runtime unchanged and returns no
    dispatchable ticket. Returned snapshots include the pending ticket, so the
    apply-time context check can reject intervening Git or forge transitions. *)
