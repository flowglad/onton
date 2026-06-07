(* @archlint.module shell
   @archlint.domain orchestrator *)

open Base

module type ENV = sig
  val runtime : Runtime.t
  val event_log : Event_log.t
end

module Make (Env : ENV) = struct
  let run ~patch_id f =
    let cancelled = ref false in
    let exception_raised = ref false in
    Stdlib.Fun.protect
      ~finally:(fun () ->
        let reason =
          if !cancelled then Orchestrator.Cancelled
          else if !exception_raised then Orchestrator.Unexpected_exception
          else Orchestrator.Cancelled
        in
        let snapshot = ref None in
        Runtime.update_orchestrator Env.runtime (fun orch ->
            match Orchestrator.find_agent orch patch_id with
            | None -> orch
            | Some before ->
                if before.Patch_agent.busy then (
                  let orch' =
                    Orchestrator.apply_force_complete orch patch_id reason
                  in
                  let after = Orchestrator.agent orch' patch_id in
                  snapshot := Some (before, after);
                  orch')
                else orch);
        Option.iter !snapshot ~f:(fun (before, after) ->
            Event_log.log_force_complete Env.event_log ~patch_id ~reason
              ~agent_before:before ~agent_after:after;
            Runtime_logging.log_event Env.runtime ~patch_id
              (Printf.sprintf
                 "Forced complete (%s) — runner fiber exited with busy=true"
                 (Orchestrator.show_force_complete_reason reason))))
      (fun () ->
        try f () with
        | Eio.Cancel.Cancelled _ as exn ->
            cancelled := true;
            raise exn
        | exn ->
            exception_raised := true;
            Runtime_logging.log_event Env.runtime ~patch_id
              (Printf.sprintf "Unexpected action exception — %s"
                 (Exn.to_string exn)))
end
