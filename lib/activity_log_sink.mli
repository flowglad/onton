(* @archlint.module interface
   @archlint.domain activity-log-sink *)

val sink :
  update:((Activity_log.t -> Activity_log.t) -> unit) ->
  unit ->
  Telemetry.Sink.t
(** Telemetry sink that appends free-form events and stream entries through
    [update]. The callback should apply its function to the current live log. *)
