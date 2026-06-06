(* @archlint.module interface
   @archlint.domain telemetry-dispatch *)

val register_sink : Telemetry.Sink.t -> unit
val unregister_sink : name:string -> unit
val emit : Telemetry.Event.t -> unit
val with_sink : sink:Telemetry.Sink.t -> (unit -> 'a) -> 'a
val with_sinks : sinks:Telemetry.Sink.t list -> (unit -> 'a) -> 'a
