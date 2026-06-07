(* @archlint.module stateTest
   @archlint.domain session-meta *)

open Onton_core

let session_meta_create_is_total_across_event_sequences =
  QCheck2.Test.make ~name:"session metadata creation is stable across events"
    ~count:200
    QCheck2.Gen.(list_size (int_range 0 20) int)
    (fun exit_codes ->
      List.for_all
        (fun exit_code ->
          let meta =
            Session_meta.create ~onton_session_uuid:"session" ~patch_id:"patch"
              ~started_at:0.0 ~ended_at:1.0 ~exit_code
              ~subkind:(Failure_subkind.Other "property") ()
          in
          let open Session_meta in
          meta.schema_version = 1 && meta.exit_code = exit_code)
        exit_codes)

let () =
  QCheck2.Test.check_exn session_meta_create_is_total_across_event_sequences
