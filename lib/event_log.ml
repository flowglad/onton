open Base
open Types

type t = { path : string }

let max_file_size = 50 * 1024 * 1024 (* 50 MB *)

let create ~path =
  (try
     let stats = Unix.stat path in
     if stats.Unix.st_size > max_file_size then (
       (try Stdlib.Sys.remove (path ^ ".1") with Sys_error _ -> ());
       Stdlib.Sys.rename path (path ^ ".1"))
   with Unix.Unix_error _ | Sys_error _ -> ());
  { path }

let timestamp () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let agent_json (a : Patch_agent.t) = Persistence.patch_agent_to_yojson a
let poll_json (p : Poller.t) = Poller.yojson_of_t p

let write_entry t (json : Yojson.Safe.t) =
  try
    let oc =
      Stdlib.open_out_gen
        [ Stdlib.Open_append; Stdlib.Open_creat; Stdlib.Open_wronly ]
        0o644 t.path
    in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_out_noerr oc)
      (fun () ->
        Yojson.Safe.to_string json |> Stdlib.output_string oc;
        Stdlib.output_char oc '\n';
        Stdlib.flush oc)
  with Sys_error _ | Unix.Unix_error _ -> ()

let patch_id_json pid = Patch_id.yojson_of_t pid

let log_poll t ~patch_id ~poll_result ~agent_before ~agent_after ~logs =
  write_entry t
    (`Assoc
       [
         ("ts", `String (timestamp ()));
         ("kind", `String "poll");
         ("patch_id", patch_id_json patch_id);
         ("poll_result", poll_json poll_result);
         ("agent_before", agent_json agent_before);
         ("agent_after", agent_json agent_after);
         ("logs", `List (List.map logs ~f:(fun s -> `String s)));
       ])

let action_patch_id (action : Orchestrator.action) =
  match action with Start (pid, _) | Respond (pid, _) | Rebase (pid, _) -> pid

let log_action t ~action ~agent_before =
  write_entry t
    (`Assoc
       [
         ("ts", `String (timestamp ()));
         ("kind", `String "action");
         ("patch_id", patch_id_json (action_patch_id action));
         ("action", `String (Orchestrator.show_action action));
         ("agent_before", agent_json agent_before);
       ])

let log_complete t ~patch_id ~result ~agent_before ~agent_after =
  write_entry t
    (`Assoc
       [
         ("ts", `String (timestamp ()));
         ("kind", `String "complete");
         ("patch_id", patch_id_json patch_id);
         ("result", `String (Orchestrator.show_session_result result));
         ("agent_before", agent_json agent_before);
         ("agent_after", agent_json agent_after);
       ])

let log_conflict_rebase t ~patch_id ~result ~decision ~agent_before ~agent_after
    =
  write_entry t
    (`Assoc
       [
         ("ts", `String (timestamp ()));
         ("kind", `String "conflict_rebase");
         ("patch_id", patch_id_json patch_id);
         ("result", `String (Worktree.show_rebase_result result));
         ( "decision",
           `String (Orchestrator.show_conflict_rebase_decision decision) );
         ("agent_before", agent_json agent_before);
         ("agent_after", agent_json agent_after);
       ])

let log_conflict_delivery t ~patch_id ~path ~rebase_in_progress ~git_status
    ~git_diff =
  write_entry t
    (`Assoc
       [
         ("ts", `String (timestamp ()));
         ("kind", `String "conflict_delivery");
         ("patch_id", patch_id_json patch_id);
         ("path", `String path);
         ("rebase_in_progress", `Bool rebase_in_progress);
         ("git_status", `String git_status);
         ("git_diff_len", `Int (String.length git_diff));
       ])

let log_rebase t ~patch_id ~result ~agent_before ~agent_after =
  write_entry t
    (`Assoc
       [
         ("ts", `String (timestamp ()));
         ("kind", `String "rebase");
         ("patch_id", patch_id_json patch_id);
         ("result", `String (Worktree.show_rebase_result result));
         ("agent_before", agent_json agent_before);
         ("agent_after", agent_json agent_after);
       ])
