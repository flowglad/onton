(* The socket is local to one Onton process and its supervising task. Durable
   delivery belongs to the caller; this server acknowledges each command. *)
open Base

let response ~id ~status =
  Yojson.Safe.to_string
    (`Assoc [ ("id", `String id); ("status", `String status) ])
  ^ "\n"

let execute runtime ~snapshot_path = function
  | Control_command.Set_automerge { patch_id; enabled; _ } -> (
      let outcome =
        Runtime.update_orchestrator_returning runtime (fun orch ->
            match Orchestrator.find_agent orch patch_id with
            | None -> (orch, "unknown_patch")
            | Some agent
              when Bool.equal agent.Patch_agent.automerge_enabled enabled ->
                (orch, "already_applied")
            | Some _ ->
                ( Orchestrator.set_automerge_enabled orch patch_id enabled,
                  "applied" ))
      in
      if String.equal outcome "applied" then
        Runtime_logging.log_event runtime ~patch_id
          (if enabled then "Automerge enabled by control command"
           else "Automerge disabled by control command");
      match outcome with
      | "applied" | "already_applied" -> (
          let snapshot = Runtime.read runtime Fn.id in
          match Persistence.save ~path:snapshot_path snapshot with
          | Ok () -> outcome
          | Error _ -> "persistence_failed")
      | _ -> outcome)

let handle runtime ~snapshot_path flow =
  let reply =
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
    | End_of_file | Eio.Buf_read.Buffer_limit_exceeded | Yojson.Json_error _ ->
      response ~id:"" ~status:"invalid_command"
  in
  Eio.Flow.copy_string reply flow

let run ~net ~runtime ~snapshot_path ~path () =
  Project_store.ensure_dir (Stdlib.Filename.dirname snapshot_path);
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net (`Unix path)
  in
  Unix.chmod path 0o600;
  Eio.Net.run_server ~max_connections:16
    ~on_error:(fun ex ->
      Stdlib.Printf.eprintf "onton control socket: %s\n%!" (Exn.to_string ex))
    socket
    (fun flow _ -> handle runtime ~snapshot_path flow)
