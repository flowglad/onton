open Base
open Onton_core

(** Property tests for {!Display_status} — totality + priority dominance.

    The 17 inline [%test] blocks in [display_status.ml] hand-pick one example
    per status. These properties cover the unbounded cross-product of state
    flags + current_op, asserting:

    - {!Display_status.derive} is total (never raises) for any combination.
    - The priority ladder dominates: e.g. [Merged] beats every other flag, and
      [Needs_help] beats [Approved] which beats [Pending]. *)

(* ─────────────────────────────────────────────────────────────────────────
   Generators
   ───────────────────────────────────────────────────────────────────────── *)

let patch_id = Types.Patch_id.of_string "1"
let main_branch = Types.Branch.of_string "main"
let gen_bool = QCheck2.Gen.bool
let gen_int_small = QCheck2.Gen.int_range 0 5

let gen_op =
  let open QCheck2.Gen in
  oneof_list
    [
      None;
      Some Types.Operation_kind.Ci;
      Some Review_comments;
      Some Merge_conflict;
      Some Human;
      Some Pr_body;
      Some Rebase;
    ]

(* Build a Patch_ctx by replaying setter calls — reaches every reachable state
   shape without poking at private fields. *)
type flag =
  | Set_merged of bool
  | Set_needs_intervention of bool
  | Set_approved of bool
  | Set_busy of bool
  | Set_has_pr of bool
  | Set_queued_ci of bool
  | Set_queued_review of bool
  | Set_failure_count of int
  | Set_base_branch of string

let gen_flag =
  let open QCheck2.Gen in
  oneof
    [
      map (fun b -> Set_merged b) gen_bool;
      map (fun b -> Set_needs_intervention b) gen_bool;
      map (fun b -> Set_approved b) gen_bool;
      map (fun b -> Set_busy b) gen_bool;
      map (fun b -> Set_has_pr b) gen_bool;
      map (fun b -> Set_queued_ci b) gen_bool;
      map (fun b -> Set_queued_review b) gen_bool;
      map (fun n -> Set_failure_count n) gen_int_small;
      map (fun s -> Set_base_branch s) (oneof_list [ "main"; "feature/foo" ]);
    ]

let apply_flag ctx = function
  | Set_merged value -> State.Patch_ctx.set_merged ctx ~patch_id ~value
  | Set_needs_intervention value ->
      State.Patch_ctx.set_needs_intervention ctx ~patch_id ~value
  | Set_approved value -> State.Patch_ctx.set_approved ctx ~patch_id ~value
  | Set_busy value -> State.Patch_ctx.set_busy ctx ~patch_id ~value
  | Set_has_pr value -> State.Patch_ctx.set_has_pr ctx ~patch_id ~value
  | Set_queued_ci value ->
      State.Patch_ctx.set_queued ctx ~patch_id ~kind:Ci ~value
  | Set_queued_review value ->
      State.Patch_ctx.set_queued ctx ~patch_id ~kind:Review_comments ~value
  | Set_failure_count count ->
      State.Patch_ctx.set_ci_failure_count ctx ~patch_id ~count
  | Set_base_branch name ->
      State.Patch_ctx.set_base_branch ctx ~patch_id
        ~branch:(Types.Branch.of_string name)

let gen_ctx =
  let open QCheck2.Gen in
  map
    (List.fold ~init:State.Patch_ctx.empty ~f:apply_flag)
    (list_size (int_range 0 8) gen_flag)

(* ─────────────────────────────────────────────────────────────────────────
   Properties
   ───────────────────────────────────────────────────────────────────────── *)

let prop_total =
  QCheck2.Test.make ~name:"derive is total over arbitrary ctx + current_op"
    ~count:1000
    QCheck2.Gen.(pair gen_ctx gen_op)
    (fun (ctx, current_op) ->
      try
        ignore (Display_status.derive ctx ~patch_id ~current_op ~main_branch);
        true
      with _ -> false)

let prop_merged_dominates =
  QCheck2.Test.make
    ~name:"set_merged true ⇒ derive returns Merged regardless of other flags"
    ~count:500
    QCheck2.Gen.(pair gen_ctx gen_op)
    (fun (ctx, current_op) ->
      let ctx = State.Patch_ctx.set_merged ctx ~patch_id ~value:true in
      Display_status.equal Merged
        (Display_status.derive ctx ~patch_id ~current_op ~main_branch))

let prop_needs_help_beats_approved =
  QCheck2.Test.make
    ~name:"needs_intervention=true ⇒ Needs_help even when approved + busy + ..."
    ~count:500
    QCheck2.Gen.(pair gen_ctx gen_op)
    (fun (ctx, current_op) ->
      (* needs_intervention beats every flag except merged. *)
      let ctx = State.Patch_ctx.set_merged ctx ~patch_id ~value:false in
      let ctx =
        State.Patch_ctx.set_needs_intervention ctx ~patch_id ~value:true
      in
      Display_status.equal Needs_help
        (Display_status.derive ctx ~patch_id ~current_op ~main_branch))

let prop_approved_busy_is_running =
  QCheck2.Test.make
    ~name:"approved + busy ⇒ Approved_running regardless of current_op"
    ~count:500 gen_op (fun current_op ->
      let ctx =
        ( ( ( State.Patch_ctx.empty |> fun c ->
              State.Patch_ctx.set_merged c ~patch_id ~value:false )
          |> fun c ->
            State.Patch_ctx.set_needs_intervention c ~patch_id ~value:false )
        |> fun c -> State.Patch_ctx.set_approved c ~patch_id ~value:true )
        |> fun c -> State.Patch_ctx.set_busy c ~patch_id ~value:true
      in
      Display_status.equal Approved_running
        (Display_status.derive ctx ~patch_id ~current_op ~main_branch))

let prop_pending_when_no_pr =
  QCheck2.Test.make
    ~name:"empty ctx (no PR, not busy, not approved, ...) ⇒ Pending" ~count:1
    QCheck2.Gen.unit (fun () ->
      Display_status.equal Pending
        (Display_status.derive State.Patch_ctx.empty ~patch_id ~current_op:None
           ~main_branch))

let prop_blocked_by_dep_requires_off_main =
  QCheck2.Test.make ~name:"has_pr + base_branch ≠ main + idle ⇒ Blocked_by_dep"
    ~count:200 QCheck2.Gen.unit (fun () ->
      let ctx =
        ( State.Patch_ctx.empty |> fun c ->
          State.Patch_ctx.set_has_pr c ~patch_id ~value:true )
        |> fun c ->
        State.Patch_ctx.set_base_branch c ~patch_id
          ~branch:(Types.Branch.of_string "feature/dep")
      in
      Display_status.equal Blocked_by_dep
        (Display_status.derive ctx ~patch_id ~current_op:None ~main_branch))

let () =
  let suite =
    [
      prop_total;
      prop_merged_dominates;
      prop_needs_help_beats_approved;
      prop_approved_busy_is_running;
      prop_pending_when_no_pr;
      prop_blocked_by_dep_requires_off_main;
    ]
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true suite in
  if exit_code <> 0 then Stdlib.exit exit_code
