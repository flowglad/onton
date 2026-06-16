(* @archlint.module interface
   @archlint.domain supervisor *)

(** Effectful guard for long-lived supervisor fibers. *)

exception
  Fatal_supervisor_error of {
    name : string;
    reason : Supervisor_decision.fatal_reason;
    message : string;
  }

val wrap :
  ?quit_is_normal:bool ->
  name:string ->
  is_normal_quit:(exn -> bool) ->
  log:(string -> unit) ->
  (unit -> unit) ->
  unit ->
  unit
(** Run a long-lived supervisor fiber. Unexpected exceptions and unexpected
    normal returns become [Fatal_supervisor_error]. Cancellation and explicitly
    normal quit exceptions are re-raised for the caller's structured cleanup. *)
