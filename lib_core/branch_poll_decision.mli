(* @archlint.module interface
   @archlint.domain orchestrator *)

type cached = {
  next_probe_at : float;
  checks_observed_at : float option;
  observed_head : string option;
  expected_head : string option;
}

type action = Skip | Probe of { reuse_checks : bool }

val head_interval : float
val checks_interval : float

val plan : now:float -> expected_head:string option -> cached option -> action
(** A new publication forces a complete observation. Otherwise probe HEAD at
    most once per [head_interval] and refresh checks at least once per
    [checks_interval], even while HEAD stays unchanged. *)
