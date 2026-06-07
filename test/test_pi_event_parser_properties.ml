(* @archlint.module test
   @archlint.domain pi-event-parser *)

open Onton_core

let pi_build_args_include_session_dir =
  QCheck2.Test.make ~name:"pi args include patch-scoped session dir" ~count:200
    QCheck2.Gen.string_small (fun patch_id ->
      let args =
        Pi_event_parser.build_args ~model:None ~cwd_path:"/tmp/work" ~patch_id
          ~prompt:"do work" ~resume_session:None
      in
      List.mem ("/tmp/work/.pi-sessions/" ^ patch_id) args)

let pi_parser_public_surface_is_linked =
  QCheck2.Test.make ~name:"pi parser public surface is linked" QCheck2.Gen.unit
    (fun () ->
      ignore Pi_event_parser.auto_model;
      ignore Pi_event_parser.parse_event;
      true)

let () =
  QCheck2.Test.check_exn pi_build_args_include_session_dir;
  QCheck2.Test.check_exn pi_parser_public_surface_is_linked
