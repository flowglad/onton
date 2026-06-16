(* @archlint.module core
   @archlint.domain automerge-state *)

open Base

let merge_queue_timer_invariant (agent : Patch_agent.t) =
  Option.is_none agent.merge_queue_entry
  || Option.is_none agent.automerge_deadline

let clear_deadline_if_enqueued agent =
  if Option.is_some agent.Patch_agent.merge_queue_entry then
    Patch_agent.clear_automerge_deadline agent
  else agent

let entered_merge_queue agent entry =
  let agent = Patch_agent.set_merge_queue_required agent true in
  let agent = Patch_agent.set_merge_queue_entry agent (Some entry) in
  let agent = Patch_agent.clear_automerge_deadline agent in
  let agent = Patch_agent.set_automerge_inflight agent false in
  Patch_agent.reset_automerge_failure_count agent

let observe_merge_queue agent ~required ~entry =
  match entry with
  | Some entry -> entered_merge_queue agent entry
  | None ->
      let agent = Patch_agent.set_merge_queue_required agent required in
      let agent = clear_deadline_if_enqueued agent in
      Patch_agent.set_merge_queue_entry agent None

let arm_deadline agent deadline =
  if Option.is_some agent.Patch_agent.merge_queue_entry then
    Patch_agent.clear_automerge_deadline agent
  else Patch_agent.set_automerge_deadline agent deadline

let merge_call_failed agent ~retry_deadline ~max_failures =
  let agent = Patch_agent.set_automerge_inflight agent false in
  let agent = Patch_agent.increment_automerge_failure_count agent in
  if Option.is_some agent.Patch_agent.merge_queue_entry then
    Patch_agent.clear_automerge_deadline agent
  else if agent.Patch_agent.automerge_failure_count >= max_failures then
    Patch_agent.clear_automerge_deadline agent
  else if not agent.Patch_agent.automerge_enabled then agent
  else arm_deadline agent retry_deadline
