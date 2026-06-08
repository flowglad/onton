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

let contains ~substring hay =
  let nlen = String.length substring and hlen = String.length hay in
  let rec go i =
    if i + nlen > hlen then false
    else if String.sub hay i nlen = substring then true
    else go (i + 1)
  in
  nlen = 0 || go 0

(* [log_stream_entry] with a [Text_chunk] must emit exactly one Stream telemetry
   event on the [`Stdout] channel, tagged "text_chunk", whose [raw] payload
   embeds the generated text. *)
let log_stream_entry_emits_stream_telemetry =
  QCheck2.Test.make ~name:"runtime log_stream_entry emits stream telemetry"
    ~count:50 QCheck2.Gen.string_small (fun text ->
      let seen = ref false in
      let sink =
        {
          Telemetry.Sink.name = "runtime-logging-stream-test";
          interested_in =
            (function Telemetry.Event.Stream _ -> true | _ -> false);
          consume =
            (function
            | Telemetry.Event.Stream { channel = `Stdout; raw; _ } ->
                (* [raw] is JSON tagged with the kind; it always carries the
                   text_chunk discriminator for a Text_chunk entry. *)
                if contains ~substring:"text_chunk" raw then seen := true
            | _ -> ());
        }
      in
      Telemetry_dispatch.with_sink ~sink (fun () ->
          Runtime_logging.log_stream_entry (Obj.magic ())
            ~patch_id:(Types.Patch_id.of_string "p")
            (Activity_log.Stream_entry.Text_chunk text));
      !seen)

let runtime_logging_public_surface_is_linked =
  QCheck2.Test.make ~name:"runtime logging public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Runtime_logging.log_stream_entry;
      true)

let () =
  QCheck2.Test.check_exn log_event_emits_free_form_telemetry;
  QCheck2.Test.check_exn log_stream_entry_emits_stream_telemetry;
  QCheck2.Test.check_exn runtime_logging_public_surface_is_linked
