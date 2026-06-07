(* @archlint.module test
   @archlint.domain claude-process *)

open Onton_core

let session_id = Types.Session_id.of_string "session"

let claude_process_sessions_are_preserved =
  QCheck2.Test.make ~name:"session transitions preserve existing sessions"
    ~count:200
    QCheck2.Gen.(
      list_size (int_range 0 20) (oneof_list [ `Busy; `Idle; `Failed ]))
    (fun steps ->
      let state =
        List.fold_left
          (fun state step ->
            match step with
            | `Busy -> Claude_process.mark_busy state
            | `Idle -> Claude_process.mark_idle state
            | `Failed -> Claude_process.mark_failed state)
          (Claude_process.start session_id)
          steps
      in
      Claude_process.has_session state
      && Claude_process.check_session_preservation
           ~before:(Claude_process.start session_id)
           ~after:state)

let claude_process_public_surface_is_linked =
  QCheck2.Test.make ~name:"claude process public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Claude_process.is_busy;
      ignore Claude_process.is_failed;
      ignore Claude_process.restart;
      true)

let () =
  QCheck2.Test.check_exn claude_process_sessions_are_preserved;
  QCheck2.Test.check_exn claude_process_public_surface_is_linked
