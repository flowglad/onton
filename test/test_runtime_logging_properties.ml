(* @archlint.module test
   @archlint.domain runtime-logging *)

[@@@warning "-4"]

open Onton
open Onton_core

let log_event_emits_free_form_telemetry =
  QCheck2.Test.make ~name:"runtime log_event emits telemetry" ~count:20
    QCheck2.Gen.string_small (fun message ->
      let seen = ref false in
      let sink =
        {
          Telemetry.Sink.name = "runtime-logging-test";
          interested_in =
            (function Telemetry.Event.Free_form _ -> true | _ -> false);
          consume =
            (function
            | Telemetry.Event.Free_form { message = got; _ } ->
                if String.equal got message then seen := true
            | _ -> ());
        }
      in
      Telemetry_dispatch.with_sink ~sink (fun () ->
          Runtime_logging.log_event (Obj.magic ()) message);
      !seen)

let runtime_logging_public_surface_is_linked =
  QCheck2.Test.make ~name:"runtime logging public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Runtime_logging.log_stream_entry;
      true)

let () =
  QCheck2.Test.check_exn log_event_emits_free_form_telemetry;
  QCheck2.Test.check_exn runtime_logging_public_surface_is_linked
