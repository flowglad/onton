(* @archlint.module interface
   @archlint.domain coordinator-state *)
val pending :
  Coordinator_state.t ->
  Types.Patch_id.t ->
  selected_base:string ->
  Worker_protocol_v3.input option
(** Deterministic cumulative delivery of all unconsumed inputs. Coverage names
    exactly the immutable IDs/versions whose effects the worker checkpoints. *)
