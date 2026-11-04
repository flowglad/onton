(* @archlint.module interface
   @archlint.domain supervisor *)

(** Effectful guard for long-lived supervisor fibers. *)

val run_after_startup : startup:(unit -> unit) -> (unit -> unit) list -> unit
(** Complete startup before starting any supervisor fibers. A failed or
    cancelled startup propagates without starting them. In particular, restored
    work must wait for startup's forge discovery to apply merged PR
    observations. *)

exception
  Fatal_supervisor_error of {
    name : string;
    reason : Supervisor_decision.fatal_reason;
    message : string;
  }

val wrap :
  ?quit_is_normal:bool ->
  ?return_is_normal:bool ->
  name:string ->
  is_normal_quit:(exn -> bool) ->
  log:(string -> unit) ->
  (unit -> unit) ->
  unit ->
  unit
(** Run a supervisor fiber. Unexpected exceptions become
    [Fatal_supervisor_error]. Normal returns are fatal by default for long-lived
    fibers, but can be allowed for one-shot startup fibers. Cancellation and
    explicitly normal quit exceptions are re-raised for the caller's structured
    cleanup. *)
