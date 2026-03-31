open Base
open Types

(** Pure application of poll results to orchestrator state.

    Extracts the decision logic from the poll result handler in main.ml so it
    can be tested without Runtime or mutable state. *)

type log_entry = { message : string; patch_id : Patch_id.t }
[@@deriving show, eq]

let apply t patch_id (poll_result : Poller.t) =
  let logs = ref [] in
  let log msg = logs := { message = msg; patch_id } :: !logs in
  let t =
    if poll_result.merged then (
      let agent = Orchestrator.agent t patch_id in
      if not agent.Patch_agent.merged then log "merged";
      Orchestrator.mark_merged t patch_id)
    else t
  in
  let t =
    if poll_result.has_conflict then (
      let agent = Orchestrator.agent t patch_id in
      if not agent.Patch_agent.has_conflict then log "merge conflict detected";
      Orchestrator.set_has_conflict t patch_id)
    else
      let agent = Orchestrator.agent t patch_id in
      let local_merge_conflict_active =
        List.mem agent.Patch_agent.queue Operation_kind.Merge_conflict
          ~equal:Operation_kind.equal
        || Option.equal Operation_kind.equal agent.Patch_agent.current_op
             (Some Operation_kind.Merge_conflict)
      in
      if local_merge_conflict_active then t
      else Orchestrator.clear_has_conflict t patch_id
  in
  let agent_before = Orchestrator.agent t patch_id in
  let t =
    List.fold poll_result.queue ~init:t ~f:(fun acc kind ->
        let cur_agent = Orchestrator.agent acc patch_id in
        let already_queued =
          List.mem cur_agent.Patch_agent.queue kind ~equal:Operation_kind.equal
        in
        let is_new =
          (not already_queued)
          && not
               (Option.equal Operation_kind.equal
                  agent_before.Patch_agent.current_op (Some kind))
        in
        match kind with
        | Operation_kind.Ci -> (
            match Patch_decision.on_ci_failure agent_before with
            | Patch_decision.Enqueue_ci ->
                if is_new then
                  log
                    (Printf.sprintf "enqueued %s"
                       (Operation_kind.to_label kind));
                Orchestrator.enqueue acc patch_id kind
            | Patch_decision.Ci_already_queued -> acc
            | Patch_decision.Ci_fix_in_progress -> acc
            | Patch_decision.Cap_reached ->
                log "CI failure cap reached (>=3), skipping CI enqueue";
                acc)
        | Operation_kind.Review_comments ->
            if is_new then
              log (Printf.sprintf "enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind
        | Operation_kind.Rebase | Operation_kind.Human
        | Operation_kind.Merge_conflict | Operation_kind.Implementation_notes ->
            if is_new then
              log (Printf.sprintf "enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind)
  in
  let t = Orchestrator.set_merge_ready t patch_id poll_result.merge_ready in
  let t = Orchestrator.set_ci_checks t patch_id poll_result.ci_checks in
  (* Clear ci_fix_running when CI passes after a fix attempt.
     Matches Elixir reference: clear_ci_fix_running when failed == [] and
     not all_pending and has_pending_ci. *)
  let t =
    let agent = Orchestrator.agent t patch_id in
    if agent.Patch_agent.ci_fix_running && poll_result.checks_passing then (
      log "CI checks passed, clearing ci_fix_running";
      Orchestrator.clear_ci_fix_running t patch_id)
    else t
  in
  (t, List.rev !logs)
