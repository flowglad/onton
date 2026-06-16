(* @archlint.module test
   @archlint.domain supervisor *)

open Base
open Onton
open Onton_core

exception Test_quit

let assert_fatal ?reason name f =
  let logs = ref [] in
  match
    Supervisor_guard.wrap ~name
      ~is_normal_quit:(function Test_quit -> true | _ -> false)
      ~log:(fun msg -> logs := msg :: !logs)
      f ()
  with
  | () -> failwith "expected fatal supervisor error"
  | exception Supervisor_guard.Fatal_supervisor_error fatal ->
      assert (String.equal fatal.name name);
      assert (not (List.is_empty !logs));
      Option.iter reason ~f:(fun expected ->
          assert (Supervisor_decision.equal_fatal_reason fatal.reason expected))

let assert_fatal_raised_contains name ~substring f =
  let logs = ref [] in
  match
    Supervisor_guard.wrap ~name
      ~is_normal_quit:(function Test_quit -> true | _ -> false)
      ~log:(fun msg -> logs := msg :: !logs)
      f ()
  with
  | () -> failwith "expected fatal supervisor error"
  | exception Supervisor_guard.Fatal_supervisor_error fatal -> (
      assert (String.equal fatal.name name);
      assert (not (List.is_empty !logs));
      match fatal.reason with
      | Raised_unexpectedly detail ->
          assert (String.is_substring detail ~substring)
      | Returned_unexpectedly -> failwith "expected raised fatal reason")

let () =
  assert_fatal "poller" (fun () -> ());
  (match
     Supervisor_guard.wrap ~return_is_normal:true ~name:"startup-reconciler"
       ~is_normal_quit:(fun _ -> false)
       ~log:(fun _ -> failwith "normal startup return should not log")
       (fun () -> ())
       ()
   with
  | () -> ()
  | exception _ -> failwith "one-shot startup return should be normal");
  assert_fatal_raised_contains "runner" ~substring:"boom" (fun () ->
      failwith "boom");

  (match
     Supervisor_guard.wrap ~quit_is_normal:true ~name:"tui"
       ~is_normal_quit:(function Test_quit -> true | _ -> false)
       ~log:(fun _ -> failwith "normal quit should not log")
       (fun () -> raise Test_quit)
       ()
   with
  | () -> failwith "expected Test_quit to propagate"
  | exception Test_quit -> ());

  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let sibling_cancelled = ref false in
  (match
     Eio.Fiber.all
       [
         Supervisor_guard.wrap ~name:"returning"
           ~is_normal_quit:(fun _ -> false)
           ~log:(fun _ -> ())
           (fun () -> Eio.Time.sleep clock 0.01);
         (fun () ->
           try Eio.Time.sleep clock 60.0
           with Eio.Cancel.Cancelled _ ->
             sibling_cancelled := true;
             raise (Eio.Cancel.Cancelled (Failure "cancelled")));
       ]
   with
  | () -> failwith "expected fatal supervisor error"
  | exception Supervisor_guard.Fatal_supervisor_error _ -> ());
  assert !sibling_cancelled;

  Stdlib.print_endline "supervisor_guard passed"
