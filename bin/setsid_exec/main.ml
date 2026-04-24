(* Tiny launcher shim. Calls setsid(2) so the exec'd process becomes the
   leader of a new session and process group, then execvp's its argv. Onton
   uses this to put each LLM subprocess into its own group so that on
   teardown we can kill(-pgid, SIG) the whole tree — otherwise tool-call
   grandchildren (e.g. Bash-spawned shells) reparent to PID 1 and leak. *)

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "onton-setsid-exec: missing program to exec";
    exit 2);
  (try ignore (Unix.setsid () : int)
   with Unix.Unix_error _ ->
     (* Already a session leader: harmless, proceed. *)
     ());
  let prog = Sys.argv.(1) in
  let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  Unix.execvp prog argv
