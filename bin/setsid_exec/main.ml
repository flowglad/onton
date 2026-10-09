(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)

(* Tiny launcher shim. Calls setsid(2) so the exec'd process becomes the
   leader of a new session and process group, then execvp's its argv. Onton
   uses this to put each LLM subprocess into its own group so that on
   teardown we can kill(-pgid, SIG) the whole tree — otherwise tool-call
   grandchildren (e.g. Bash-spawned shells) reparent to PID 1 and leak. *)

external become_subreaper : unit -> unit = "caml_onton_become_subreaper"
external leader_exited : int -> bool = "caml_onton_leader_exited"

external group_has_descendants : int -> bool
  = "caml_onton_group_has_descendants"

(* In supervised mode this single-threaded process owns the command's session.
   On Linux it also adopts orphaned descendants, so waiting for Git alone cannot
   leave filters behind. On macOS launchd reaps orphans; wait for the group to
   disappear before acknowledging teardown. *)
let supervise ?parent ~streaming ~guards argv =
  let parent_alive () =
    match parent with
    | None -> true
    | Some expected -> Unix.getppid () = expected
  in
  if not (parent_alive ()) then (
    prerr_endline "onton-setsid-exec: launching parent is no longer present";
    exit 125);
  let guard_fds =
    try
      List.map
        (fun path ->
          let fd =
            Unix.openfile path
              [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ]
              0o600
          in
          Unix.lockf fd Unix.F_TRLOCK 0;
          fd)
        guards
    with exn ->
      prerr_endline
        ("onton-setsid-exec: command guard unavailable: "
       ^ Printexc.to_string exn);
      exit 125
  in
  (* Admission may have run while this helper was opening its guards. A helper
     whose old parent died must never launch after a replacement's admission. *)
  if not (parent_alive ()) then (
    prerr_endline
      "onton-setsid-exec: launching parent disappeared before dispatch";
    exit 125);
  become_subreaper ();
  let stopping = ref false in
  let terminal = ref false in
  let signals = [ Sys.sigterm; Sys.sigint; Sys.sighup ] in
  List.iter
    (fun signal ->
      Sys.set_signal signal (Sys.Signal_handle (fun _ -> stopping := true)))
    signals;
  if streaming then
    Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> terminal := true));
  let ready_r, ready_w = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 -> (
      Unix.close ready_r;
      List.iter Unix.close guard_fds;
      List.iter (fun signal -> Sys.set_signal signal Sys.Signal_default) signals;
      if streaming then Sys.set_signal Sys.sigusr1 Sys.Signal_default;
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
      let signal_group signal =
        if have_group then
          try Unix.kill (-pid) signal
          with Unix.Unix_error (Unix.ESRCH, _, _) -> ()
      in
      let grace_ticks = ref None in
      let terminal_ticks = ref 0 in
      let rec await_leader () =
        if not (parent_alive ()) then stopping := true;
        if leader_exited pid then ()
        else (
          (* Git gets a bounded chance to remove its own index/ref locks. *)
          if !stopping && Option.is_none !grace_ticks then (
            signal_group Sys.sigterm;
            grace_ticks := Some 25);
          if !grace_ticks = Some 0 then signal_group Sys.sigkill;
          (* A terminal stream event starts graceful CLI shutdown. Signal only
             the leader here: its helpers retain their separate flush window. *)
          if !terminal && not !stopping then (
            if !terminal_ticks = 200 then Unix.kill pid Sys.sigterm;
            if !terminal_ticks >= 400 then Unix.kill pid Sys.sigkill;
            incr terminal_ticks);
          Unix.sleepf 0.01;
          grace_ticks :=
            Option.map (fun ticks -> max 0 (ticks - 1)) !grace_ticks;
          await_leader ())
      in
      await_leader ();
      let descendant_ticks = ref (if streaming then 200 else 0) in
      let rec await_descendants () =
        if not (parent_alive ()) then stopping := true;
        if have_group && group_has_descendants pid then (
          (* Keep the unreaped leader as the group identity anchor. Repeated
             kills cover children forked during the preceding observation. *)
          if !stopping || !descendant_ticks = 0 then signal_group Sys.sigkill
          else decr descendant_ticks;
          Unix.sleepf 0.01;
          await_descendants ())
      in
      await_descendants ();
      let rec reap_leader () =
        try snd (Unix.waitpid [] pid)
        with Unix.Unix_error (Unix.EINTR, _, _) -> reap_leader ()
      in
      (* No group signal or observation may follow this reap. *)
      match reap_leader () with
      | Unix.WEXITED code -> exit code
      | Unix.WSIGNALED signal ->
          (* Uncatchable signals cannot have their disposition changed. *)
          if signal <> Sys.sigkill && signal <> Sys.sigstop then
            Sys.set_signal signal Sys.Signal_default;
          Unix.kill (Unix.getpid ()) signal;
          exit 128
      | Unix.WSTOPPED _ -> exit 127)

let () =
  let streaming =
    Array.length Sys.argv > 1 && Sys.argv.(1) = "--supervise-stream-parent"
  in
  let parent_bound =
    streaming
    || (Array.length Sys.argv > 1 && Sys.argv.(1) = "--supervise-parent")
  in
  let supervised =
    parent_bound || (Array.length Sys.argv > 1 && Sys.argv.(1) = "--supervise")
  in
  let parent =
    if parent_bound then (
      match
        if Array.length Sys.argv > 2 then int_of_string_opt Sys.argv.(2)
        else None
      with
      | Some pid when pid > 0 -> Some pid
      | Some _ | None ->
          prerr_endline "onton-setsid-exec: positive parent PID required";
          exit 2)
    else None
  in
  let first = if parent_bound then 3 else if supervised then 2 else 1 in
  let first, guards =
    if
      supervised
      && Array.length Sys.argv > first
      && Sys.argv.(first) = "--command-guards"
    then (
      match
        if Array.length Sys.argv > first + 1 then
          int_of_string_opt Sys.argv.(first + 1)
        else None
      with
      | Some count when count >= 0 && count <= Array.length Sys.argv - first - 3
        ->
          ( first + 2 + count,
            Array.to_list (Array.sub Sys.argv (first + 2) count) )
      | Some _ | None ->
          prerr_endline "onton-setsid-exec: invalid command guard count";
          exit 2)
    else (first, [])
  in
  if Array.length Sys.argv <= first then (
    prerr_endline "onton-setsid-exec: missing program to exec";
    exit 2);
  let argv = Array.sub Sys.argv first (Array.length Sys.argv - first) in
  if supervised then supervise ?parent ~streaming ~guards argv;
  (try ignore (Unix.setsid () : int)
   with Unix.Unix_error _ ->
     (* Already a session leader: harmless, proceed. *)
     ());
  Unix.execvp argv.(0) argv
