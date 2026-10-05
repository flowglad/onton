(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)

(* Tiny launcher shim. Calls setsid(2) so the exec'd process becomes the
   leader of a new session and process group, then execvp's its argv. Onton
   uses this to put each LLM subprocess into its own group so that on
   teardown we can kill(-pgid, SIG) the whole tree — otherwise tool-call
   grandchildren (e.g. Bash-spawned shells) reparent to PID 1 and leak. *)

external become_subreaper : unit -> unit = "caml_onton_become_subreaper"

(* In supervised mode this single-threaded process owns the command's session.
   On Linux it also adopts orphaned descendants, so waiting for Git alone cannot
   leave filters behind. On macOS launchd reaps orphans; wait for the group to
   disappear before acknowledging teardown. *)
let supervise argv =
  become_subreaper ();
  let stopping = ref false in
  let signals = [ Sys.sigterm; Sys.sigint; Sys.sighup ] in
  List.iter
    (fun signal ->
      Sys.set_signal signal (Sys.Signal_handle (fun _ -> stopping := true)))
    signals;
  let ready_r, ready_w = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 -> (
      Unix.close ready_r;
      List.iter (fun signal -> Sys.set_signal signal Sys.Signal_default) signals;
      try
        ignore (Unix.setsid () : int);
        ignore (Unix.write ready_w (Bytes.of_string "1") 0 1 : int);
        Unix.close ready_w;
        Unix.execvp argv.(0) argv
      with exn ->
        prerr_endline (Printexc.to_string exn);
        exit 127)
  | pid -> (
      Unix.close ready_w;
      let rec read_ready () =
        try Unix.read ready_r (Bytes.create 1) 0 1 = 1
        with Unix.Unix_error (Unix.EINTR, _, _) -> read_ready ()
      in
      let have_group = read_ready () in
      Unix.close ready_r;
      let kill_group () =
        if have_group then
          try Unix.kill (-pid) Sys.sigkill
          with Unix.Unix_error (Unix.ESRCH, _, _) -> ()
      in
      let status = ref None in
      let killed = ref false in
      let rec reap () =
        if (!stopping || Option.is_some !status) && not !killed then (
          kill_group ();
          killed := true);
        match Unix.waitpid [ Unix.WNOHANG ] (-1) with
        | 0, _ ->
            Unix.sleepf 0.01;
            reap ()
        | child, result ->
            if child = pid then status := Some result;
            reap ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
        | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
      in
      reap ();
      let rec await_group () =
        if have_group then
          match Unix.kill (-pid) 0 with
          | () ->
              Unix.sleepf 0.01;
              await_group ()
          | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ()
      in
      await_group ();
      match !status with
      | Some (Unix.WEXITED code) -> exit code
      | Some (Unix.WSIGNALED signal) ->
          (* Uncatchable signals cannot have their disposition changed. *)
          if signal <> Sys.sigkill && signal <> Sys.sigstop then
            Sys.set_signal signal Sys.Signal_default;
          Unix.kill (Unix.getpid ()) signal;
          exit 128
      | Some (Unix.WSTOPPED _) | None -> exit 127)

let () =
  let supervised = Array.length Sys.argv > 1 && Sys.argv.(1) = "--supervise" in
  let first = if supervised then 2 else 1 in
  if Array.length Sys.argv <= first then (
    prerr_endline "onton-setsid-exec: missing program to exec";
    exit 2);
  let argv = Array.sub Sys.argv first (Array.length Sys.argv - first) in
  if supervised then supervise argv;
  (try ignore (Unix.setsid () : int)
   with Unix.Unix_error _ ->
     (* Already a session leader: harmless, proceed. *)
     ());
  Unix.execvp argv.(0) argv
