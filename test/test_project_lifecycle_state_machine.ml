(* @archlint.module stateTest
   @archlint.domain project-lifecycle *)

open Onton
module L = Project_lifecycle

type held = Reader of L.use | Writer of L.retirement

let release = function
  | Reader lease -> L.release_use lease
  | Writer lease -> L.release_retirement lease

let get = function
  | Ok value -> value
  | Error error -> failwith (L.error_message error)

let busy = function
  | Error (L.Busy _) -> true
  | Ok _ | Error (L.Io_error _) -> false

let names = [| "sequence-a"; "sequence-b" |]

let histories =
  QCheck2.Test.make
    ~name:
      "lifecycle leases obey generated ownership and stale-release histories"
    ~count:300
    ~print:(fun operations ->
      String.concat "," (List.map string_of_int operations))
    QCheck2.Gen.(list_size (int_range 0 60) (int_range 0 13))
    (fun operations ->
      let registration = ref None and old_registration = ref None in
      let held = Array.make 2 None and old_release = Array.make 2 None in
      let close_registration () =
        Option.iter
          (fun lease ->
            L.release_registration lease;
            old_registration := Some lease)
          !registration;
        registration := None
      in
      try
        Fun.protect
          ~finally:(fun () ->
            Array.iter (Option.iter release) held;
            close_registration ())
          (fun () ->
            List.iter
              (fun operation ->
                let index = operation mod 2 in
                (match operation with
                | 0 -> (
                    match !registration with
                    | None ->
                        registration := Some (get (L.acquire_registration ()))
                    | Some _ -> assert (busy (L.acquire_registration ())))
                | 1 -> close_registration ()
                | 2 | 3 | 4 | 5 | 12 | 13 -> (
                    match !registration with
                    | None -> ()
                    | Some r -> (
                        let result =
                          if operation < 4 then
                            Result.map
                              (fun lease -> Reader lease)
                              (L.acquire_use r ~project_name:names.(index))
                          else if operation >= 12 then
                            Result.map
                              (fun lease -> Reader lease)
                              (L.acquire_writer r ~project_name:names.(index))
                          else
                            Result.map
                              (fun lease -> Writer lease)
                              (L.acquire_retirement r
                                 ~project_name:names.(index))
                        in
                        match (held.(index), result) with
                        | None, Ok lease -> held.(index) <- Some lease
                        | Some _, Error (L.Busy _) -> ()
                        | Some _, Ok lease ->
                            release lease;
                            assert false
                        | None, Error _ | Some _, Error (L.Io_error _) ->
                            assert false))
                | 6 | 7 ->
                    Option.iter
                      (fun lease ->
                        release lease;
                        old_release.(index) <- Some lease)
                      held.(index);
                    held.(index) <- None
                | 8 | 9 -> Option.iter release old_release.(index)
                | 10 ->
                    Option.iter
                      (fun r ->
                        assert (
                          Result.is_error
                            (L.acquire_use r ~project_name:"stale")))
                      !old_registration
                | _ -> Option.iter L.release_registration !old_registration);
                let expected_guards =
                  Array.to_list
                    (Array.mapi
                       (fun index lease ->
                         Option.map
                           (fun _ ->
                             Filename.concat
                               (Unix.realpath (Project_store.lifecycle_dir ()))
                               ("use-" ^ names.(index) ^ ".lock.drain"))
                           lease)
                       held)
                  |> List.filter_map Fun.id |> List.sort String.compare
                in
                assert (L.command_guards () = expected_guards);
                Option.iter
                  (fun r ->
                    assert (busy (L.acquire_registration ()));
                    Array.iteri
                      (fun index lease ->
                        match lease with
                        | None -> ()
                        | Some _ ->
                            assert (
                              busy
                                (L.acquire_retirement r
                                   ~project_name:names.(index))))
                      held)
                  !registration)
              operations;
            close_registration ();
            (* Check the actual kernel locks from another process. A correct
             in-process table alone cannot prove old handles did not close or
             unlock a descriptor later reused for a new lease. *)
            flush stdout;
            flush stderr;
            let child = Unix.fork () in
            (if child = 0 then
               let valid =
                 try
                   assert (L.command_guards () = []);
                   let r = get (L.acquire_registration ()) in
                   let valid =
                     Array.mapi
                       (fun index expected ->
                         match
                           ( L.acquire_retirement r ~project_name:names.(index),
                             expected )
                         with
                         | Ok lease, None ->
                             L.release_retirement lease;
                             true
                         | Error (L.Busy _), Some _ -> true
                         | Ok lease, Some _ ->
                             L.release_retirement lease;
                             false
                         | Error _, None | Error (L.Io_error _), Some _ -> false)
                       held
                     |> Array.for_all Fun.id
                   in
                   L.release_registration r;
                   valid
                 with _ -> false
               in
               exit (if valid then 0 else 2));
            let rec wait () =
              try snd (Unix.waitpid [] child)
              with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
            in
            wait () = Unix.WEXITED 0)
      with _ -> false)

let () =
  let root = Filename.temp_dir "onton-lifecycle-model-" "" in
  let previous = Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "ONTON_DATA_DIR" root;
  let code =
    Fun.protect
      ~finally:(fun () ->
        let directory = Project_store.lifecycle_dir () in
        if Sys.file_exists directory then (
          Array.iter
            (fun name -> Unix.unlink (Filename.concat directory name))
            (Sys.readdir directory);
          Unix.rmdir directory);
        Unix.rmdir root;
        Unix.putenv "ONTON_DATA_DIR" (Option.value previous ~default:""))
      (fun () -> QCheck_base_runner.run_tests [ histories ])
  in
  if code <> 0 then exit code
