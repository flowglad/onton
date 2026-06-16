(* @archlint.module test
   @archlint.domain supervisor *)

open Base
open Onton_core

let gen_name =
  QCheck2.Gen.(
    map
      (fun s -> if String.is_empty s then "fiber" else s)
      (string_size ~gen:printable (int_range 0 24)))

let gen_detail =
  QCheck2.Gen.(
    map
      (fun s -> if String.is_empty s then "Failure" else s)
      (string_size ~gen:printable (int_range 0 80)))

let gen_termination =
  let open QCheck2.Gen in
  oneof
    [
      return (Supervisor_decision.Returned : Supervisor_decision.termination);
      return Supervisor_decision.Cancelled;
      return Supervisor_decision.Quit;
      map (fun detail -> Supervisor_decision.Raised detail) gen_detail;
    ]

let gen_return_policy =
  QCheck2.Gen.oneof_list
    Supervisor_decision.[ Return_is_fatal; Return_is_normal ]

let is_fatal = function
  | Supervisor_decision.Fatal _ -> true
  | Normal_return | Normal_quit | Propagate_cancel -> false

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"supervisor classify is total" ~count:1_000
       QCheck2.Gen.(quad gen_name bool gen_return_policy gen_termination)
       (fun (name, quit_is_normal, return_policy, termination) ->
         try
           ignore
             (Supervisor_decision.classify ~name ~quit_is_normal ~return_policy
                termination);
           true
         with _ -> false));

  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"normal return is fatal for long-lived fibers"
       ~count:300
       QCheck2.Gen.(pair gen_name bool)
       (fun (name, quit_is_normal) ->
         match
           Supervisor_decision.classify ~name ~quit_is_normal
             ~return_policy:Return_is_fatal Supervisor_decision.Returned
         with
         | Fatal { name = actual; reason } ->
             String.equal actual name
             && Supervisor_decision.equal_fatal_reason reason
                  Returned_unexpectedly
         | Normal_return | Normal_quit | Propagate_cancel -> false));

  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"normal return is normal for one-shot fibers"
       ~count:300
       QCheck2.Gen.(pair gen_name bool)
       (fun (name, quit_is_normal) ->
         Supervisor_decision.equal_decision
           (Supervisor_decision.classify ~name ~quit_is_normal
              ~return_policy:Return_is_normal Supervisor_decision.Returned)
           Supervisor_decision.Normal_return));

  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"unexpected exception is always fatal" ~count:300
       QCheck2.Gen.(quad gen_name bool gen_return_policy gen_detail)
       (fun (name, quit_is_normal, return_policy, detail) ->
         match
           Supervisor_decision.classify ~name ~quit_is_normal ~return_policy
             (Supervisor_decision.Raised detail)
         with
         | Fatal { name = actual; reason } ->
             String.equal actual name
             && Supervisor_decision.equal_fatal_reason reason
                  (Raised_unexpectedly detail)
         | Normal_return | Normal_quit | Propagate_cancel -> false));

  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"cancellation is never fatal" ~count:300
       QCheck2.Gen.(triple gen_name bool gen_return_policy)
       (fun (name, quit_is_normal, return_policy) ->
         Supervisor_decision.equal_decision
           (Supervisor_decision.classify ~name ~quit_is_normal ~return_policy
              Supervisor_decision.Cancelled)
           Supervisor_decision.Propagate_cancel));

  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"quit is normal only when allowed" ~count:300
       QCheck2.Gen.(pair gen_name bool)
       (fun (name, quit_is_normal) ->
         match
           Supervisor_decision.classify ~name ~quit_is_normal
             ~return_policy:Return_is_fatal Supervisor_decision.Quit
         with
         | Normal_quit -> quit_is_normal
         | Fatal _ -> not quit_is_normal
         | Normal_return | Propagate_cancel -> false));

  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"fatal exits are deferred until cleanup has completed" ~count:1
       QCheck2.Gen.unit (fun () ->
         Supervisor_decision.equal_exit_decision
           (Supervisor_decision.exit_after_fatal Cleanup_pending)
           Defer_exit_until_cleanup
         && Supervisor_decision.equal_exit_decision
              (Supervisor_decision.exit_after_fatal Cleanup_done)
              Exit_now));

  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"interleavings cannot stay running after fatal termination"
       ~count:500
       QCheck2.Gen.(list_size (int_range 1 30) (pair gen_name gen_termination))
       (fun events ->
         let fatal_seen =
           List.exists events ~f:(fun (name, termination) ->
               Supervisor_decision.classify ~name ~quit_is_normal:false
                 ~return_policy:Return_is_fatal termination
               |> is_fatal)
         in
         let final_state_running =
           List.fold events ~init:true ~f:(fun running (name, termination) ->
               if not running then false
               else
                 match
                   Supervisor_decision.classify ~name ~quit_is_normal:false
                     ~return_policy:Return_is_fatal termination
                 with
                 | Fatal _ -> false
                 | Normal_return | Normal_quit | Propagate_cancel -> true)
         in
         (not fatal_seen) || not final_state_running));

  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"fatal messages include fiber name and reason"
       ~count:300
       QCheck2.Gen.(pair gen_name gen_detail)
       (fun (name, detail) ->
         let msg =
           Supervisor_decision.message ~name
             (Supervisor_decision.Raised_unexpectedly detail)
         in
         String.is_substring msg ~substring:name
         && String.is_substring msg ~substring:detail));

  Stdlib.print_endline "supervisor_decision properties passed"
