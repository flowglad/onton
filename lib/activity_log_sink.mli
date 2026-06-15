(* @archlint.module interface
   @archlint.domain activity-log-sink *)

val sink :
  main_branch:Types.Branch.t ->
  update:((Activity_log.t -> Activity_log.t) -> unit) ->
  unit ->
  Telemetry.Sink.t
(** Telemetry sink that appends free-form events, status transitions, and stream
    entries through [update]. The callback should apply its function to the
    current live log. *)
