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

(* After [mark_busy] the state [is_busy] and is not [is_failed]; after
   [mark_failed] the converse. We generate the transition to apply. *)
let claude_process_status_predicates_agree =
  QCheck2.Test.make ~name:"claude process is_busy/is_failed track marks"
    ~count:200
    QCheck2.Gen.(oneof_list [ `Busy; `Idle; `Failed ])
    (fun step ->
      let start = Claude_process.start session_id in
      match step with
      | `Busy ->
          let s = Claude_process.mark_busy start in
          Claude_process.is_busy s && not (Claude_process.is_failed s)
      | `Failed ->
          let s = Claude_process.mark_failed start in
          Claude_process.is_failed s && not (Claude_process.is_busy s)
      | `Idle ->
          let s = Claude_process.mark_idle start in
          (not (Claude_process.is_busy s)) && not (Claude_process.is_failed s))

(* [restart] clears a [Failed] session (back to a live, non-failed session) and
   otherwise leaves an Idle/Busy session untouched. In every case the session is
   preserved and the result is never failed. *)
let claude_process_restart_clears_failure =
  QCheck2.Test.make ~name:"claude process restart clears failure" ~count:200
    QCheck2.Gen.(oneof_list [ `Busy; `Idle; `Failed ])
    (fun step ->
      let prior =
        match step with
        | `Busy -> Claude_process.mark_busy (Claude_process.start session_id)
        | `Idle -> Claude_process.mark_idle (Claude_process.start session_id)
        | `Failed ->
            Claude_process.mark_failed (Claude_process.start session_id)
      in
      let new_id = Types.Session_id.of_string "restarted" in
      let restarted = Claude_process.restart new_id prior in
      Claude_process.has_session restarted
      && (not (Claude_process.is_failed restarted))
      &&
      match step with
      (* A busy session is left untouched by restart. *)
      | `Busy -> Claude_process.is_busy restarted
      | `Idle | `Failed -> not (Claude_process.is_busy restarted))

let claude_process_public_surface_is_linked =
  QCheck2.Test.make ~name:"claude process public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Claude_process.is_busy;
      ignore Claude_process.is_failed;
      ignore Claude_process.restart;
      true)

let () =
  QCheck2.Test.check_exn claude_process_sessions_are_preserved;
  QCheck2.Test.check_exn claude_process_status_predicates_agree;
  QCheck2.Test.check_exn claude_process_restart_clears_failure;
  QCheck2.Test.check_exn claude_process_public_surface_is_linked
