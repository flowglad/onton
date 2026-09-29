(* @archlint.module shell
   @archlint.domain control-command *)

(* The socket is local to one Onton process and its supervising task. Durable
   delivery belongs to the caller; this server acknowledges each command. *)
open Base

let response ~id ~status =
  Yojson.Safe.to_string
    (`Assoc [ ("id", `String id); ("status", `String status) ])
  ^ "\n"

external peer_uid : Unix.file_descr -> int = "caml_onton_control_peer_uid"

let authorized_peer flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> false
  | Some fd ->
      Eio_unix.Fd.use fd
        ~if_closed:(fun () -> false)
        (fun unix_fd -> Int.equal (peer_uid unix_fd) (Unix.geteuid ()))

let execute runtime ~snapshot_path = function
  | command ->
      let id = Control_command.id command in
      let outcome =
        Runtime.update_persisting runtime
          ~persist:(Persistence.save_snapshot ~path:snapshot_path)
          (fun snapshot ->
            if List.mem snapshot.applied_control_ids id ~equal:String.equal then
              (snapshot, "already_applied")
            else
              let orch = snapshot.Runtime.orchestrator in
              let patch_id =
                match command with
                | Control_command.Set_automerge { patch_id; _ }
                | Control_command.Bump { patch_id; _ }
                | Control_command.Send_human_message { patch_id; _ } ->
                    patch_id
              in
              match Orchestrator.find_agent orch patch_id with
              | None -> (snapshot, "unknown_patch")
              | Some agent -> (
                  let next =
                    match command with
                    | Control_command.Set_automerge { enabled; _ } ->
                        if
                          Bool.equal agent.Patch_agent.automerge_enabled enabled
                        then None
                        else
                          Some
                            (Orchestrator.set_automerge_enabled orch patch_id
                               enabled)
                    | Control_command.Bump _ ->
                        if Patch_agent.needs_intervention agent then
                          Some
                            (Orchestrator.reset_intervention_state orch patch_id)
                        else None
                    | Control_command.Send_human_message { message; _ } ->
                        if agent.Patch_agent.merged then None
                        else
                          Some
                            (Orchestrator.send_human_message orch patch_id
                               message)
                  in
                  match next with
                  | None -> (
                      match command with
                      | Control_command.Set_automerge _ ->
                          ( {
                              snapshot with
                              applied_control_ids =
                                Control_command.record_id id
                                  snapshot.applied_control_ids;
                            },
                            "already_applied" )
                      | Control_command.Bump _
                      | Control_command.Send_human_message _ ->
                          (snapshot, "not_applicable"))
                  | Some orchestrator ->
                      ( {
                          snapshot with
                          orchestrator;
                          applied_control_ids =
                            Control_command.record_id id
                              snapshot.applied_control_ids;
                        },
                        "applied" )))
      in
      let outcome =
        match outcome with
        | Ok outcome -> outcome
        | Error _ -> "persistence_failed"
      in
      (if String.equal outcome "applied" then
         let patch_id, message =
           match command with
           | Control_command.Set_automerge { patch_id; enabled; _ } ->
               ( patch_id,
                 if enabled then "Automerge enabled by control command"
                 else "Automerge disabled by control command" )
           | Control_command.Bump { patch_id; _ } ->
               ( patch_id,
                 "Bumped — cleared intervention state by control command" )
           | Control_command.Send_human_message { patch_id; _ } ->
               (patch_id, "Human message accepted by control command")
         in
         Runtime_logging.log_event runtime ~patch_id message);
      outcome

let handle runtime ~snapshot_path flow =
  let reply =
    if not (authorized_peer flow) then response ~id:"" ~status:"unauthorized"
    else
      try
        let reader = Eio.Buf_read.of_flow ~max_size:4096 flow in
        let line = Eio.Buf_read.line reader in
        match Yojson.Safe.from_string line with
        | json -> (
            match Control_command.decode json with
            | Error _ -> response ~id:"" ~status:"invalid_command"
            | Ok command ->
                response
                  ~id:(Control_command.id command)
                  ~status:(execute runtime ~snapshot_path command))
      with
      | End_of_file | Eio.Buf_read.Buffer_limit_exceeded | Yojson.Json_error _
      ->
        response ~id:"" ~status:"invalid_command"
  in
  Eio.Flow.copy_string reply flow

let run ~net ~runtime ~snapshot_path ~path () =
  let parent = Stdlib.Filename.dirname path in
  if Stdlib.Filename.is_relative path then
    invalid_arg "control socket path must be absolute";
  (try Unix.mkdir parent 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let stat = Unix.lstat parent in
  if
    (not (Poly.equal stat.Unix.st_kind Unix.S_DIR))
    || stat.Unix.st_uid <> Unix.geteuid ()
    || stat.Unix.st_perm land 0o077 <> 0
  then invalid_arg "control socket parent must be an owner-only directory";
  Project_store.ensure_dir (Stdlib.Filename.dirname snapshot_path);
  let bound = ref false in
  Stdlib.Fun.protect
    ~finally:(fun () ->
      if !bound then
        try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let socket =
        Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net (`Unix path)
      in
      bound := true;
      Unix.chmod path 0o600;
      Eio.Net.run_server ~max_connections:16
        ~on_error:(fun ex ->
          Stdlib.Printf.eprintf "onton control socket: %s\n%!"
            (Exn.to_string ex))
        socket
        (fun flow _ -> handle runtime ~snapshot_path flow))
