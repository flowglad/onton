(* @archlint.module test
   @archlint.domain automerge-state *)

open Base
open Onton_core
open Onton_core.Types

let pid = Patch_id.of_string "am-state"
let branch = Branch.of_string "feat/am-state"
let retry_deadline = 600.0
let max_failures = 3

let gen_string =
  QCheck2.Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 1 16))

let gen_merge_queue_entry =
  let open QCheck2.Gen in
  let states =
    Pr_state.
      [ Mq_queued; Mq_awaiting_checks; Mq_mergeable; Mq_unmergeable; Mq_locked ]
  in
  map3
    (fun id state position -> Pr_state.{ id; state; position })
    gen_string (oneof_list states) (int_range 0 99)

type op =
  | Enable of bool
  | Arm of float
  | Set_inflight of bool
  | Entered of Pr_state.merge_queue_entry
  | Observe_none of bool
  | Observe_some of Pr_state.merge_queue_entry
  | Merge_failure
  | Reset_failure
  | Clear_deadline

let gen_op =
  let open QCheck2.Gen in
  oneof
    [
      map (fun v -> Enable v) bool;
      map (fun n -> Arm (Float.of_int n)) (int_range 0 10_000);
      map (fun v -> Set_inflight v) bool;
      map (fun entry -> Entered entry) gen_merge_queue_entry;
      map (fun required -> Observe_none required) bool;
      map (fun entry -> Observe_some entry) gen_merge_queue_entry;
      pure Merge_failure;
      pure Reset_failure;
      pure Clear_deadline;
    ]

let apply_op agent = function
  | Enable v -> Patch_agent.set_automerge_enabled agent v
  | Arm deadline -> Automerge_state.arm_deadline agent deadline
  | Set_inflight v -> Patch_agent.set_automerge_inflight agent v
  | Entered entry -> Automerge_state.entered_merge_queue agent entry
  | Observe_none required ->
      Automerge_state.observe_merge_queue agent ~required ~entry:None
  | Observe_some entry ->
      Automerge_state.observe_merge_queue agent ~required:false
        ~entry:(Some entry)
  | Merge_failure ->
      Automerge_state.merge_call_failed agent ~retry_deadline ~max_failures
  | Reset_failure -> Patch_agent.reset_automerge_failure_count agent
  | Clear_deadline -> Patch_agent.clear_automerge_deadline agent

let base_agent () =
  Patch_agent.create ~branch pid |> fun agent ->
  Patch_agent.set_automerge_enabled agent true

let prop_entered_merge_queue_shape =
  QCheck2.Test.make
    ~name:
      "automerge_state MQ-IN: entering merge queue clears \
       timer/inflight/failures"
    ~count:1000
    QCheck2.Gen.(pair (list_size (int_range 0 30) gen_op) gen_merge_queue_entry)
    (fun (ops, entry) ->
      let agent = List.fold ops ~init:(base_agent ()) ~f:apply_op in
      let agent = Automerge_state.entered_merge_queue agent entry in
      agent.Patch_agent.merge_queue_required
      && Option.equal Pr_state.equal_merge_queue_entry agent.merge_queue_entry
           (Some entry)
      && Option.is_none agent.automerge_deadline
      && (not agent.automerge_inflight)
      && agent.automerge_failure_count = 0
      && Automerge_state.merge_queue_timer_invariant agent)

let prop_observe_entry_is_entered_merge_queue =
  QCheck2.Test.make
    ~name:
      "automerge_state MQ-OBS: observing an entry has entered-queue semantics"
    ~count:1000
    QCheck2.Gen.(pair (list_size (int_range 0 30) gen_op) gen_merge_queue_entry)
    (fun (ops, entry) ->
      let agent = List.fold ops ~init:(base_agent ()) ~f:apply_op in
      let observed =
        Automerge_state.observe_merge_queue agent ~required:false
          ~entry:(Some entry)
      in
      let entered = Automerge_state.entered_merge_queue agent entry in
      Patch_agent.equal observed entered)

let prop_observe_none_ejection_clears_stale_deadline =
  QCheck2.Test.make
    ~name:
      "automerge_state MQ-EJECT: observing no entry clears stale queue timer"
    ~count:1000
    QCheck2.Gen.(triple bool gen_merge_queue_entry (int_range 0 10_000))
    (fun (required, entry, deadline) ->
      let agent =
        base_agent () |> fun agent ->
        Patch_agent.set_merge_queue_required agent true |> fun agent ->
        Patch_agent.set_merge_queue_entry agent (Some entry) |> fun agent ->
        Patch_agent.set_automerge_deadline agent (Float.of_int deadline)
      in
      let agent =
        Automerge_state.observe_merge_queue agent ~required ~entry:None
      in
      Bool.equal agent.Patch_agent.merge_queue_required required
      && Option.is_none agent.Patch_agent.merge_queue_entry
      && Option.is_none agent.Patch_agent.automerge_deadline
      && Automerge_state.merge_queue_timer_invariant agent)

let prop_failure_does_not_rearm_when_already_enqueued =
  QCheck2.Test.make
    ~name:"automerge_state MQ-FAIL: failure cannot re-arm an enqueued patch"
    ~count:1000
    QCheck2.Gen.(pair (list_size (int_range 0 30) gen_op) gen_merge_queue_entry)
    (fun (ops, entry) ->
      let agent = List.fold ops ~init:(base_agent ()) ~f:apply_op in
      let agent =
        Automerge_state.entered_merge_queue agent entry |> fun agent ->
        Patch_agent.set_automerge_inflight agent true |> fun agent ->
        Automerge_state.arm_deadline agent 1.0 |> fun agent ->
        Automerge_state.merge_call_failed agent ~retry_deadline ~max_failures
      in
      Option.is_none agent.Patch_agent.automerge_deadline
      && Automerge_state.merge_queue_timer_invariant agent)

let prop_interleavings_preserve_invariant =
  QCheck2.Test.make
    ~name:
      "automerge_state MQ-INT: transition interleavings never leave entry+timer"
    ~count:2000
    QCheck2.Gen.(list_size (int_range 0 80) gen_op)
    (fun ops ->
      let _agent, ok =
        List.fold ops
          ~init:(base_agent (), true)
          ~f:(fun (agent, ok) op ->
            try
              let agent = apply_op agent op in
              (agent, ok && Automerge_state.merge_queue_timer_invariant agent)
            with _ -> (agent, false))
      in
      ok)

let () =
  List.iter
    [
      prop_entered_merge_queue_shape;
      prop_observe_entry_is_entered_merge_queue;
      prop_observe_none_ejection_clears_stale_deadline;
      prop_failure_does_not_rearm_when_already_enqueued;
      prop_interleavings_preserve_invariant;
    ] ~f:(fun test -> QCheck2.Test.check_exn test)
