(* @archlint.module test
   @archlint.domain anchor *)

open Base
open Onton_core

(** Interleaving state-machine tests for the rebase anchor pipeline.

    Each named scenario (SM-N) simulates a realistic sequence of orchestrator
    actions + external git events against an in-memory commit DAG. After each
    Rebase action, the test asserts that the [Rebase_decision.plan] invocation
    produces a [<upstream>] argument that would NOT cause
    [git rebase --onto <target> <upstream> HEAD] to replay commits already
    present on the new base. Together with the property suite in
    [test_rebase_decision_properties], these tests pin down the integration
    between {!Patch_agent.record_anchor}, {!Rebase_decision.plan}, and
    {!Rebase_decision.anchor_after_result} over sequences. *)

module Branch = Types.Branch

(* ── In-memory commit DAG model ────────────────────────────────────────── *)

module Dag = struct
  type t = {
    parents : (string, string option) Hashtbl.t;
    refs : (string, string) Hashtbl.t;
  }

  let create () =
    {
      parents = Hashtbl.create (module String);
      refs = Hashtbl.create (module String);
    }

  let add_commit t sha ~parent =
    Hashtbl.set t.parents ~key:sha ~data:parent;
    sha

  let set_ref t name sha = Hashtbl.set t.refs ~key:name ~data:sha
  let resolve t name = Hashtbl.find t.refs name

  let rec is_ancestor t sha ~descendant =
    if String.equal sha descendant then true
    else
      match Hashtbl.find t.parents descendant with
      | None | Some None -> false
      | Some (Some parent) -> is_ancestor t sha ~descendant:parent

  (* Pure predicate: would [git rebase --onto target upstream HEAD] try to
     replay any commit reachable from [upstream] but ALSO reachable from
     [target]? If yes, those commits are "already there" and replaying them
     produces "both added" or duplicate-application conflicts. This is the
     production bug the anchor pipeline exists to prevent. *)
  let _rebase_would_replay_merged_commits _t ~upstream:_ ~target:_ ~head:_ =
    (* Not directly checked in these scenarios — instead we check the
       stronger property: the chosen upstream is itself reachable from
       head, which means [upstream..head] is exactly the patch's own
       commits (per git's range semantics). *)
    false

  let oracle t sha ~descendant = is_ancestor t sha ~descendant
end

(* ── Helpers ────────────────────────────────────────────────────────────── *)

let mk_anchor ~base ~sha =
  match Anchor.make ~base ~sha ~observed_at_remote:true with
  | Some a -> a
  | None -> failwith ("mk_anchor: bad SHA " ^ sha)

let sha_for n = Printf.sprintf "%040x" n

let run_plan ~history ~base_branch ~head ~dag =
  let input : Rebase_decision.input =
    {
      anchor = Anchor_history.newest history;
      recorded_history = Anchor_history.to_list history;
      base_branch;
      head_sha = Some head;
    }
  in
  Rebase_decision.plan input ~ancestor_oracle:(Dag.oracle dag)

(* Plan accessors — strict-warnings disallow wildcard variants. *)
let plan_upstream : Rebase_decision.plan -> string = function
  | Onto { upstream; _ } -> upstream
  | Plain { target; _ } -> target

let plan_reason : Rebase_decision.plan -> Rebase_decision.reason = function
  | Onto { reason; _ } | Plain { reason; _ } -> reason

let reason_is_anchor_matches_head : Rebase_decision.reason -> bool = function
  | Anchor_matches_head -> true
  | History_fallback _ | No_anchor | Anchor_unreachable_from_head
  | Head_unobservable ->
      false

let reason_is_history_fallback : Rebase_decision.reason -> bool = function
  | History_fallback _ -> true
  | Anchor_matches_head | No_anchor | Anchor_unreachable_from_head
  | Head_unobservable ->
      false

let reason_is_no_anchor : Rebase_decision.reason -> bool = function
  | No_anchor -> true
  | Anchor_matches_head | History_fallback _ | Anchor_unreachable_from_head
  | Head_unobservable ->
      false

(* ── SM-2: production-bug scenario, end-to-end ─────────────────────────── *)
(* p4 was branched off patch-2 at Start. patch-2 squash-merges to main while
   p4 is idle. p4's first rebase must produce Onto with the old patch-2 tip
   as upstream — git's `upstream..HEAD` is then exactly p4's own work and
   replays cleanly onto post-squash main. *)
let test_sm2_production_bug () =
  let dag = Dag.create () in
  let main_branch = Branch.of_string "main" in
  let dep_branch = Branch.of_string "patch-2" in

  (* Initial main: m1. patch-2 = m1 -> x1 -> x2. p4 branched off patch-2 at
     x2 with one commit z on top. *)
  let m1 = Dag.add_commit dag (sha_for 1) ~parent:None in
  let x1 = Dag.add_commit dag (sha_for 2) ~parent:(Some m1) in
  let x2 = Dag.add_commit dag (sha_for 3) ~parent:(Some x1) in
  let z = Dag.add_commit dag (sha_for 4) ~parent:(Some x2) in
  Dag.set_ref dag "origin/main" m1;
  Dag.set_ref dag "origin/patch-2" x2;
  Dag.set_ref dag "HEAD@p4" z;

  (* Step 1: p4's Start runs for_start which captures origin/patch-2 = x2.
     The anchor (base=patch-2, sha=x2) goes into anchor_history. *)
  let history =
    Anchor_history.push Anchor_history.empty
      (mk_anchor ~base:dep_branch ~sha:x2)
  in

  (* Step 2: patch-2 squash-merges. Main advances to m1 -> m3 (the squash).
     origin/patch-2 may still exist (we model it as still pointing at x2). *)
  let m3 = Dag.add_commit dag (sha_for 5) ~parent:(Some m1) in
  Dag.set_ref dag "origin/main" m3;

  (* Step 3: orchestrator's refresh_base_branch (post-merge) shifts p4's
     base_branch from patch-2 to main. anchor_history is preserved per
     the step-8 design. *)
  let plan = run_plan ~history ~base_branch:main_branch ~head:z ~dag in

  let upstream = plan_upstream plan in
  let reason = plan_reason plan in

  (* Assert: upstream = x2 (the old patch-2 tip). *)
  assert (String.equal upstream x2);
  (* Assert: reason = Anchor_matches_head (the anchor is reachable from HEAD). *)
  assert (reason_is_anchor_matches_head reason);
  (* Critical invariant: upstream is an ancestor of HEAD. This is what
     makes [git rebase --onto main x2 z] sound — the range x2..z is
     exactly {z}, p4's own commit. The dep's commits (x1, x2) are NOT
     replayed because they're at-or-below upstream. *)
  assert (Dag.is_ancestor dag upstream ~descendant:z);
  Stdlib.print_endline "SM-2 (production bug) passed"

(* ── SM-3: force-push of dep branch; history fallback ──────────────────── *)
(* Setup: same as SM-2 but the dep branch (patch-2) is force-pushed so its
   origin tip moves to a divergent SHA. p4's HEAD still descends from the
   pre-force-push tip (force-pushing a dep doesn't affect a dependent's
   local HEAD). The newest anchor is still reachable from HEAD; SM-3 verifies
   that the plan picks Anchor_matches_head, not History_fallback (because
   the newest anchor IS still good). *)
let test_sm3_force_push_dep () =
  let dag = Dag.create () in
  let main_branch = Branch.of_string "main" in
  let dep_branch = Branch.of_string "patch-2" in
  let m1 = Dag.add_commit dag (sha_for 1) ~parent:None in
  let x1 = Dag.add_commit dag (sha_for 2) ~parent:(Some m1) in
  let x2 = Dag.add_commit dag (sha_for 3) ~parent:(Some x1) in
  let z = Dag.add_commit dag (sha_for 4) ~parent:(Some x2) in
  Dag.set_ref dag "origin/main" m1;
  Dag.set_ref dag "origin/patch-2" x2;

  let history =
    Anchor_history.push Anchor_history.empty
      (mk_anchor ~base:dep_branch ~sha:x2)
  in

  (* Force-push patch-2 to a new SHA divergent from p4's HEAD. *)
  let divergent = Dag.add_commit dag (String.make 40 'a') ~parent:None in
  Dag.set_ref dag "origin/patch-2" divergent;

  (* p4's HEAD still descends from x2 (force-push doesn't touch local p4). *)
  let plan = run_plan ~history ~base_branch:main_branch ~head:z ~dag in
  let upstream = plan_upstream plan in
  assert (String.equal upstream x2);
  assert (reason_is_anchor_matches_head (plan_reason plan));
  Stdlib.print_endline "SM-3a (dep force-push, anchor still in HEAD) passed";

  (* Scenario variant: p4's HEAD ITSELF gets reset (e.g. via `git reset
     --hard`) such that x2 is no longer an ancestor of HEAD. Then the
     anchor diverges; without a history fallback, plan returns
     Plain {Anchor_unreachable_from_head}. *)
  let fresh_head = Dag.add_commit dag (String.make 40 'b') ~parent:None in
  let plan2 =
    run_plan ~history ~base_branch:main_branch ~head:fresh_head ~dag
  in
  assert (
    match plan2 with
    | Plain { reason; _ } -> (
        match reason with
        | Anchor_unreachable_from_head -> true
        | Anchor_matches_head | History_fallback _ | No_anchor
        | Head_unobservable ->
            false)
    | Onto _ -> false);
  Stdlib.print_endline "SM-3b (HEAD reset, anchor unreachable) passed"

(* ── SM-7: Noop rebase refreshes the anchor ────────────────────────────── *)
(* When a rebase returns Noop (local HEAD already contains origin/<base>),
   anchor_after_result must still produce a fresh anchor — previously this
   was a blind spot because the SHA was captured only on Ok. *)
let test_sm7_noop_refreshes_anchor () =
  let main_branch = Branch.of_string "main" in
  let prev = Some (mk_anchor ~base:main_branch ~sha:(sha_for 1)) in

  let after =
    Rebase_decision.anchor_after_result ~prev ~result:Worktree_parser.Noop
      ~resolved_remote_sha:(Some (sha_for 2))
      ~base_branch:main_branch
  in
  (match after with
  | None -> failwith "SM-7: Noop should refresh anchor"
  | Some a ->
      assert (String.equal (Anchor.sha a) (sha_for 2));
      assert (Branch.equal (Anchor.base a) main_branch));
  Stdlib.print_endline "SM-7 (Noop refreshes anchor) passed"

(* ── SM-8: Conflict preserves prior anchor; retry uses the same one ───── *)
(* A Conflict result must NOT clear or replace the prior anchor. The next
   rebase attempt sees the same input and picks the same plan. *)
let test_sm8_conflict_preserves_prior () =
  let main_branch = Branch.of_string "main" in
  let prev = Some (mk_anchor ~base:main_branch ~sha:(sha_for 1)) in
  let conflict : Worktree_parser.rebase_result =
    Worktree_parser.Conflict
      {
        target = "main";
        old_base = "";
        unique_commits = [];
        strategy = Worktree_parser.Plain;
        orig_head = "";
      }
  in
  let after =
    Rebase_decision.anchor_after_result ~prev ~result:conflict
      ~resolved_remote_sha:(Some (sha_for 2))
      ~base_branch:main_branch
  in
  assert (Option.equal Anchor.equal after prev);
  Stdlib.print_endline "SM-8 (Conflict preserves prior anchor) passed"

(* ── SM-1: dep merges WHILE dependent's rebase is in flight ───────────── *)
(* Sequence:
   1. p2's anchor recorded at Start as {patch-1, p1_tip}.
   2. p2's Rebase begins; planner reads anchor_history at this point.
   3. EXTERNALLY: patch-1 squash-merges to main (changes origin/main but
      NOT origin/patch-1).
   4. The in-flight rebase completes Ok; anchor_after_result on Ok records
      a fresh anchor against the (now-updated) origin/main tip.
   Invariant: even if step 3 happens between steps 2 and 4, the recorded
   anchor remains coherent: it's either the pre-merge p1_tip (correct
   for the rebase that was in flight) or the post-merge main_tip (correct
   for subsequent rebases). It is NEVER a fabricated value or empty. *)
let test_sm1_concurrent_merge_during_rebase () =
  let dag = Dag.create () in
  let main_branch = Branch.of_string "main" in
  let dep_branch = Branch.of_string "patch-1" in
  let m1 = Dag.add_commit dag (sha_for 1) ~parent:None in
  let x = Dag.add_commit dag (sha_for 2) ~parent:(Some m1) in
  Dag.set_ref dag "origin/main" m1;
  Dag.set_ref dag "origin/patch-1" x;

  (* Step 1: p2 Start records anchor on dep at x. *)
  let history =
    Anchor_history.push Anchor_history.empty (mk_anchor ~base:dep_branch ~sha:x)
  in
  assert (
    Option.equal Anchor.equal
      (Anchor_history.newest history)
      (Some (mk_anchor ~base:dep_branch ~sha:x)));

  (* Step 2: Rebase dispatched against base=patch-1. The planner is going
     to be called from inside the executor; here we simulate one execution. *)
  let plan_during_rebase =
    run_plan ~history ~base_branch:dep_branch ~head:(sha_for 3) ~dag
  in
  (* Sanity: we have a coherent plan. (No assertion on outcome; the patch's
     HEAD = sha_for 3 isn't in the DAG so the plan can be either Plain
     unreachable or otherwise — both are coherent.) *)
  let _ = plan_during_rebase in

  (* Step 3 (external): patch-1 squash-merges to main. *)
  let m3 = Dag.add_commit dag (sha_for 4) ~parent:(Some m1) in
  Dag.set_ref dag "origin/main" m3;

  (* Step 4: the rebase completes Ok against the new main. Record fresh
     anchor via anchor_after_result. *)
  let after =
    Rebase_decision.anchor_after_result
      ~prev:(Anchor_history.newest history)
      ~result:Worktree_parser.Ok
      ~resolved_remote_sha:(Dag.resolve dag "origin/main")
      ~base_branch:main_branch
  in
  let history =
    match after with Some a -> Anchor_history.push history a | None -> history
  in
  (* Invariant: history now contains BOTH the pre-merge anchor (still
     reachable as the second-newest) and the fresh post-merge anchor. *)
  let entries = Anchor_history.to_list history in
  assert (List.length entries = 2);
  assert (String.equal (Anchor.sha (List.nth_exn entries 0)) m3);
  assert (String.equal (Anchor.sha (List.nth_exn entries 1)) x);
  Stdlib.print_endline "SM-1 (concurrent merge during rebase) passed"

(* ── SM-4: 4-deep stack, multiple concurrent merges ─────────────────────── *)
(* p1 ← p2 ← p3 ← p4. p1, p2, p3 squash-merge in sequence. p4's anchor
   history grows; at each step, plan picks the right anchor for the
   currently-targeted base, never replaying merged work. *)
let test_sm4_deep_stack_sequential_merges () =
  let dag = Dag.create () in
  let main_branch = Branch.of_string "main" in
  let p1_branch = Branch.of_string "p1" in
  let p2_branch = Branch.of_string "p2" in
  let p3_branch = Branch.of_string "p3" in

  let m0 = Dag.add_commit dag (sha_for 100) ~parent:None in
  let p1_tip = Dag.add_commit dag (sha_for 101) ~parent:(Some m0) in
  let p2_tip = Dag.add_commit dag (sha_for 102) ~parent:(Some p1_tip) in
  let p3_tip = Dag.add_commit dag (sha_for 103) ~parent:(Some p2_tip) in
  let p4_commit = Dag.add_commit dag (sha_for 104) ~parent:(Some p3_tip) in
  Dag.set_ref dag "origin/main" m0;

  (* p4 Start: anchor on p3 at p3_tip. *)
  let history =
    Anchor_history.push Anchor_history.empty
      (mk_anchor ~base:p3_branch ~sha:p3_tip)
  in

  (* p1 squash-merges into main. p4 still has p1_tip..p4 in its history. *)
  let m1 = Dag.add_commit dag (sha_for 200) ~parent:(Some m0) in
  Dag.set_ref dag "origin/main" m1;
  let plan1 = run_plan ~history ~base_branch:main_branch ~head:p4_commit ~dag in
  assert (String.equal (plan_upstream plan1) p3_tip);
  assert (reason_is_anchor_matches_head (plan_reason plan1));

  (* p2 squash-merges. *)
  let m2 = Dag.add_commit dag (sha_for 201) ~parent:(Some m1) in
  Dag.set_ref dag "origin/main" m2;
  let plan2 = run_plan ~history ~base_branch:main_branch ~head:p4_commit ~dag in
  assert (String.equal (plan_upstream plan2) p3_tip);

  (* p3 squash-merges. Now p3_tip is no longer the dep tip but it's
     STILL an ancestor of p4's HEAD (since p4 was branched off p3 at
     p3_tip and hasn't been rebased). Anchor still valid. *)
  let m3 = Dag.add_commit dag (sha_for 202) ~parent:(Some m2) in
  Dag.set_ref dag "origin/main" m3;
  let plan3 = run_plan ~history ~base_branch:main_branch ~head:p4_commit ~dag in
  assert (String.equal (plan_upstream plan3) p3_tip);
  assert (Dag.is_ancestor dag p3_tip ~descendant:p4_commit);
  let _ = (p1_branch, p2_branch) in
  Stdlib.print_endline "SM-4 (deep stack sequential merges) passed"

(* ── SM-no-anchor: bare baseline ───────────────────────────────────────── *)
(* No anchor has ever been recorded (legacy agent, no Start ever ran since
   the upgrade). Plan returns Plain {No_anchor}, which the executor
   translates to upstream = target → the 2-arg legacy form. This is
   bit-equivalent to today's behavior. *)
let test_sm_no_anchor_baseline () =
  let dag = Dag.create () in
  let main_branch = Branch.of_string "main" in
  let m = Dag.add_commit dag (sha_for 1) ~parent:None in
  Dag.set_ref dag "origin/main" m;
  let plan =
    run_plan ~history:Anchor_history.empty ~base_branch:main_branch ~head:m ~dag
  in
  assert (reason_is_no_anchor (plan_reason plan));
  Stdlib.print_endline "SM-no-anchor (baseline) passed"

let () =
  test_sm1_concurrent_merge_during_rebase ();
  test_sm2_production_bug ();
  test_sm3_force_push_dep ();
  test_sm4_deep_stack_sequential_merges ();
  test_sm7_noop_refreshes_anchor ();
  test_sm8_conflict_preserves_prior ();
  test_sm_no_anchor_baseline ();
  let _ = reason_is_history_fallback in
  Stdlib.print_endline "All rebase state-machine tests passed"
