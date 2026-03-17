(* Regression test: blocking read wrapped in run_in_systhread must not starve
   the Eio scheduler.

   Creates a pipe (write end kept open so read blocks), then races two fibers:
   - Fiber A: blocking Unix.read inside Eio_unix.run_in_systhread
   - Fiber B: sets a flag, sleeps briefly, then closes both pipe FDs to unblock A

   Eio.Fiber.any lets fiber B win. If someone removes the run_in_systhread
   wrapper, the bare Unix.read blocks the scheduler and the test hangs
   (killed by dune timeout). *)

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let read_fd, write_fd = Unix.pipe () in
  let tui_rendered = ref false in
  Eio.Fiber.any
    [
      (fun () ->
        try
          ignore
            (Eio_unix.run_in_systhread (fun () ->
                 let buf = Bytes.create 1 in
                 Unix.read read_fd buf 0 1))
        with Unix.Unix_error _ | Eio.Cancel.Cancelled _ -> ());
      (fun () ->
        tui_rendered := true;
        Eio.Time.sleep clock 0.5;
        (* Close pipe FDs to unblock the systhread *)
        (try Unix.close read_fd with Unix.Unix_error _ -> ());
        try Unix.close write_fd with Unix.Unix_error _ -> ());
    ];
  (* Clean up in case fiber B didn't close them *)
  (try Unix.close read_fd with Unix.Unix_error _ -> ());
  (try Unix.close write_fd with Unix.Unix_error _ -> ());
  assert !tui_rendered;
  Printf.printf "PASS: blocking read in systhread does not starve scheduler\n"
