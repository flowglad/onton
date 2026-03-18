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
            | Patch_decision.Cap_reached ->
                log "CI failure cap reached (>=3), skipping CI enqueue";
                acc)
        | Operation_kind.Review_comments ->
            if is_new then
              log
                (Printf.sprintf "enqueued %s (%d new comments)"
                   (Operation_kind.to_label kind)
                   (List.length poll_result.new_comments));
            Orchestrator.enqueue acc patch_id kind
        | Operation_kind.Rebase | Operation_kind.Human
        | Operation_kind.Merge_conflict ->
            if is_new then
              log (Printf.sprintf "enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind)
  in
  let t = Orchestrator.set_merge_ready t patch_id poll_result.merge_ready in
  let t = Orchestrator.set_ci_checks t patch_id poll_result.ci_checks in
  (* Don't add comments while the agent is actively handling review comments —
     they'll be re-discovered on the next poll after complete, at which point
     addressed_comment_ids will filter out the handled ones. *)
  let t =
    let agent = Orchestrator.agent t patch_id in
    if
      Option.equal Operation_kind.equal agent.Patch_agent.current_op
        (Some Operation_kind.Review_comments)
    then t
    else
      List.fold poll_result.new_comments ~init:t ~f:(fun acc comment ->
          Orchestrator.add_pending_comment acc patch_id comment ~valid:true)
  in
  (t, List.rev !logs)
