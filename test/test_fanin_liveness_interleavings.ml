open Base
open Onton
open Onton_core
open Onton_core.Types

(** Fan-in start-liveness (FLI) property-interleaving tests.

    Spec: a gameplan patch whose dependencies are satisfied — at most one
    unmerged dependency, every dependency merged or holding a PR
    ([Graph.deps_satisfied]) — must eventually Start under fair scheduling. When
    the sole open dependency [B]'s branch is missing a merged *sibling*
    dependency's squash commit, the Start is correctly deferred by
    [Start_eligibility] ([Base_missing_merged_sibling]); liveness then requires
    [Reconciler.detect_sibling_stale_bases] to create the demand that closes the
    gap — an [Enqueue_rebase B] — whether or not the fan-in patch itself has
    started yet. An unstarted fan-in patch is precisely the one that needs the
    detector: it has no PR of its own for any other detector to act on.

    The harness drives the *real* production functions over a pure model:

    - A pure git-containment model replaces [git merge-base --is-ancestor]: each
      branch maps to the set of synthetic merge SHAs it has absorbed. [main]
      absorbs a patch's SHA when its PR merges; a patch's branch absorbs a
      snapshot of its base's current set when the patch Starts (the cut) and
      when a Rebase onto that base completes.
    - [tick] mirrors the poller's reconcile phase ([poller_fiber]): recompute
      every agent's [base_contains_merged_siblings] via the real
      [Base_containment.contains_merged_siblings] (backed by the model oracle),
      build [Reconciler.patch_view]s, run [Reconciler.reconcile], and apply
      [Mark_merged]/[Enqueue_rebase] actions.
    - Start/Rebase execution flows through the real gated planner
      ([Patch_controller.plan_messages] → [Orchestrator.runnable_messages]), so
      the [Start_eligibility] deferral path is exercised, not re-derived.

    Liveness oracle: after a bounded fair quiescence loop (tick + fire every
    runnable Start/Rebase, repeatedly), no patch may remain unstarted while
    [Graph.deps_satisfied] holds for it — a startable-but-never-started patch at
    the fixpoint is exactly the liveness violation. *)

let main = Branch.of_string "main"
let main_s = Branch.to_string main
let mk_patches = Onton_test_support.Test_generators.mk_linear_patches

(* -- Model -- *)

type model = {
  orch : Orchestrator.t;
  patches : Patch.t list;
  absorbed : Set.M(String).t Map.M(String).t;
      (** Pure git-ancestry model: branch name -> merge SHAs the branch
          contains. Branches absent from the map have absorbed nothing. *)
}

let merge_sha_of pid = "sha-" ^ Patch_id.to_string pid

let branch_of_patches patches =
  let map =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) ->
        Map.set acc ~key:p.Patch.id ~data:p.Patch.branch)
  in
  fun pid -> Option.value (Map.find map pid) ~default:main

let pid_of_idx patches i =
  let p = List.nth_exn patches i in
  p.Patch.id

let idx_of_pid patches pid =
  match
    List.findi patches ~f:(fun _ (p : Patch.t) -> Patch_id.equal p.Patch.id pid)
  with
  | Some (i, _) -> i
  | None -> invalid_arg "idx_of_pid: unknown patch"

let absorbed_of m branch_s =
  Option.value
    (Map.find m.absorbed branch_s)
    ~default:(Set.empty (module String))

(** Snapshot-absorb: [branch] now sits on [from], so it contains exactly the
    merge SHAs [from] contains right now (a cut or a completed rebase). *)
let absorb m ~branch ~from =
  {
    m with
    absorbed =
      Map.set m.absorbed ~key:(Branch.to_string branch)
        ~data:(absorbed_of m (Branch.to_string from));
  }

let has_merged_in orch pid = (Orchestrator.agent orch pid).Patch_agent.merged

(** Structural base for [pid] given the current merge state. Callers must guard
    on [open_pr_deps <= 1] ([Graph.initial_base] raises above that). *)
let initial_base_of m pid =
  Graph.initial_base
    (Orchestrator.graph m.orch)
    pid ~has_merged:(has_merged_in m.orch)
    ~branch_of:(branch_of_patches m.patches)
    ~main

let deps_satisfied m pid =
  Graph.deps_satisfied (Orchestrator.graph m.orch)
    pid ~has_merged:(has_merged_in m.orch) ~has_pr:(fun dep ->
      Patch_agent.is_pr_present (Orchestrator.agent m.orch dep))

(** Start [pid] from [base]: PR appears and the session completes, mirroring the
    SBI bootstrap (fire Start → set_pr_number → complete). The branch absorbs
    its base's current merge SHAs (the cut). No-op when already started or when
    dependencies are not satisfied (cannot cut a worktree off a missing base).
*)
let do_start m pid base =
  let agent = Orchestrator.agent m.orch pid in
  if Patch_agent.has_pr agent || agent.Patch_agent.merged then m
  else if not (deps_satisfied m pid) then m
  else
    let orch = Orchestrator.fire m.orch (Orchestrator.Start (pid, base)) in
    let orch =
      Orchestrator.set_pr_number orch pid
        (Pr_number.of_int (idx_of_pid m.patches pid + 1))
    in
    let orch = Orchestrator.complete orch pid in
    absorb { m with orch } ~branch:agent.Patch_agent.branch ~from:base

(** Complete a rebase of [pid] onto [base]: the branch re-cuts onto the target,
    absorbing its current merge SHAs. *)
let do_rebase m pid base =
  let agent = Orchestrator.agent m.orch pid in
  if
    (not (Patch_agent.has_pr agent))
    || agent.Patch_agent.merged || agent.Patch_agent.busy
  then m
  else
    let orch = Orchestrator.fire m.orch (Orchestrator.Rebase (pid, base)) in
    let orch, _effects =
      Orchestrator.apply_rebase_result orch pid Worktree.Ok base
    in
    absorb { m with orch } ~branch:agent.Patch_agent.branch ~from:base

(** Bootstrap a model where the first [started] patches (in dependency order;
    [gen_patch_dag]/[mk_linear_patches] only depend backwards, so a prefix is
    dependency-closed) have Started and hold PRs, and the rest are unstarted.
    [do_start]'s [deps_satisfied] guard skips fan-in patches whose >1 open deps
    make them unstartable, exactly as production would. *)
let bootstrap patches ~started =
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let m =
    {
      orch;
      patches;
      absorbed =
        Map.singleton (module String) main_s (Set.empty (module String));
    }
  in
  List.foldi patches ~init:m ~f:(fun i m (p : Patch.t) ->
      if i < started then
        let open_deps =
          Graph.open_pr_deps
            (Orchestrator.graph m.orch)
            p.Patch.id ~has_merged:(has_merged_in m.orch)
        in
        match open_deps with
        | [] | [ _ ] -> do_start m p.Patch.id (initial_base_of m p.Patch.id)
        | _ -> m
      else m)

(* -- Commands -- *)

type command =
  | Pr_merged of int
      (** The PR merges (squash to main). Feasible only when the PR exists and
          every dependency is already merged — mirroring the automerge
          discipline that a PR merges from a main base. [main] absorbs the
          synthetic merge SHA; [mark_merged]'s eager enqueue puts [Rebase] in
          dependents' queues. *)
  | Rebase_complete of int
      (** The runner completes a queued rebase onto the patch's current
          structural base, which absorbs that base's merge SHAs. *)
  | Start_via_gate of int
      (** Fire the patch's Start if (and only if) the gated planner surfaces a
          runnable Start for it right now — the production path. Keeps every
          reachable model state production-reachable. *)

let apply_command m cmd =
  match cmd with
  | Pr_merged i ->
      if i >= List.length m.patches then m
      else
        let pid = pid_of_idx m.patches i in
        let agent = Orchestrator.agent m.orch pid in
        let open_deps =
          Graph.open_pr_deps
            (Orchestrator.graph m.orch)
            pid ~has_merged:(has_merged_in m.orch)
        in
        if
          (not (Patch_agent.has_pr agent))
          || agent.Patch_agent.merged
          || not (List.is_empty open_deps)
        then m
        else
          let sha = merge_sha_of pid in
          let orch = Orchestrator.set_merge_commit_sha m.orch pid (Some sha) in
          let orch = Orchestrator.mark_merged orch pid in
          {
            m with
            orch;
            absorbed =
              Map.set m.absorbed ~key:main_s
                ~data:(Set.add (absorbed_of m main_s) sha);
          }
  | Rebase_complete i ->
      if i >= List.length m.patches then m
      else
        let pid = pid_of_idx m.patches i in
        let agent = Orchestrator.agent m.orch pid in
        let rebase_in_queue =
          List.mem agent.Patch_agent.queue Operation_kind.Rebase
            ~equal:Operation_kind.equal
        in
        let open_deps =
          Graph.open_pr_deps
            (Orchestrator.graph m.orch)
            pid ~has_merged:(has_merged_in m.orch)
        in
        if
          (not (Patch_agent.has_pr agent))
          || agent.Patch_agent.merged || agent.Patch_agent.busy
          || (not rebase_in_queue)
          || List.length open_deps > 1
        then m
        else do_rebase m pid (initial_base_of m pid)
  | Start_via_gate i ->
      if i >= List.length m.patches then m
      else
        let pid = pid_of_idx m.patches i in
        let msgs = Patch_controller.plan_messages m.orch ~patches:m.patches in
        let base_opt =
          List.find_map msgs ~f:(fun msg ->
              match Orchestrator.message_action msg with
              | Orchestrator.Start (p, base) when Patch_id.equal p pid ->
                  Some base
              | Orchestrator.Start _ | Orchestrator.Rebase _
              | Orchestrator.Respond _ ->
                  None)
        in
        Option.value_map base_opt ~default:m ~f:(do_start m pid)

(* -- Tick: the poller's reconcile phase over the pure model -- *)

let view_of_agent (a : Patch_agent.t) : Reconciler.patch_view =
  {
    Reconciler.id = a.Patch_agent.patch_id;
    has_pr = Patch_agent.has_pr a;
    merged = a.Patch_agent.merged;
    busy = a.Patch_agent.busy;
    needs_intervention = Patch_agent.needs_intervention a;
    branch_blocked = a.Patch_agent.branch_blocked;
    queue = a.Patch_agent.queue;
    base_branch = Option.value a.Patch_agent.base_branch ~default:main;
    branch_rebased_onto = a.Patch_agent.branch_rebased_onto;
    base_contains_merged_siblings = a.Patch_agent.base_contains_merged_siblings;
  }

(** One reconcile tick, mirroring [poller_fiber]'s reconcile phase: containment
    cache recompute (real [Base_containment] over the model's ancestor oracle) →
    patch views → [Reconciler.reconcile] → apply [Mark_merged] /
    [Enqueue_rebase]. Returns the reconcile actions so properties can assert on
    the demand the reconciler created. *)
let tick m =
  let graph = Orchestrator.graph m.orch in
  let has_merged = has_merged_in m.orch in
  let merge_sha pid =
    (Orchestrator.agent m.orch pid).Patch_agent.merge_commit_sha
  in
  let branch_of = branch_of_patches m.patches in
  let ancestor_oracle sha ~descendant =
    Set.mem (absorbed_of m descendant) sha
  in
  let orch =
    List.fold (Orchestrator.all_agents m.orch) ~init:m.orch
      ~f:(fun orch (a : Patch_agent.t) ->
        let contains =
          Base_containment.contains_merged_siblings ~graph
            ~patch_id:a.Patch_agent.patch_id ~has_merged ~merge_sha ~branch_of
            ~main ~ancestor_oracle
        in
        Orchestrator.set_base_contains_merged_siblings orch
          a.Patch_agent.patch_id contains)
  in
  let m = { m with orch } in
  let views = List.map (Orchestrator.all_agents m.orch) ~f:view_of_agent in
  let merged_patches =
    List.filter_map (Orchestrator.all_agents m.orch)
      ~f:(fun (a : Patch_agent.t) ->
        if a.Patch_agent.merged then Some a.Patch_agent.patch_id else None)
  in
  let actions =
    Reconciler.reconcile ~graph ~main ~merged_pr_patches:merged_patches
      ~branch_of views
  in
  let orch =
    List.fold actions ~init:m.orch ~f:(fun orch action ->
        match action with
        | Reconciler.Mark_merged pid -> Orchestrator.mark_merged orch pid
        | Reconciler.Enqueue_rebase pid ->
            Orchestrator.enqueue orch pid Operation_kind.Rebase
        | Reconciler.Start_operation _ -> orch)
  in
  ({ m with orch }, actions)

(** Fire every Start/Rebase the gated planner surfaces as runnable right now — a
    fair runner draining one planning round. *)
let fire_runnable m =
  let msgs = Patch_controller.plan_messages m.orch ~patches:m.patches in
  List.fold msgs ~init:m ~f:(fun m msg ->
      match Orchestrator.message_action msg with
      | Orchestrator.Start (pid, base) -> do_start m pid base
      | Orchestrator.Rebase (pid, base) -> do_rebase m pid base
      | Orchestrator.Respond _ -> m)

(** Fair quiescence: with external events stopped, run [n] rounds of tick +
    fire-runnable. A correct system converges well within [2 * n_patches + 3]
    rounds (one freshening rebase plus one Start per dependency layer, plus
    slack for the final containment recompute). *)
let quiesce m ~rounds =
  Fn.apply_n_times ~n:rounds (fun m -> fire_runnable (fst (tick m))) m

(* -- Liveness oracle -- *)

(** A startable-but-unstarted patch at the quiescence fixpoint is a liveness
    violation: its dependencies are satisfied right now ([Graph.deps_satisfied]
    over actual PR state), yet it never Started. The bottom-most patch of any
    stuck chain satisfies this, so checking "none exists" catches stuck states
    at every depth. *)
let no_startable_unstarted m =
  List.for_all m.patches ~f:(fun (p : Patch.t) ->
      let agent = Orchestrator.agent m.orch p.Patch.id in
      Patch_agent.has_pr agent || agent.Patch_agent.merged
      || not (deps_satisfied m p.Patch.id))

(* -- The diamond: the production shape (4 <- {2, 3}, both <- 1) -- *)

let mk_patch ~id ~deps : Patch.t =
  {
    id = Patch_id.of_string id;
    title = "";
    description = "";
    branch = Branch.of_string ("branch-" ^ id);
    dependencies = List.map deps ~f:Patch_id.of_string;
    spec = "";
    acceptance_criteria = [];
    files = [];
    classification = "";
    changes = [];
    test_stubs_introduced = [];
    test_stubs_implemented = [];
    complexity = None;
    precedents = [];
    required_context = [];
  }

(* Index:        0       1            2            3
   Shape:        p0  <-  p1, p0 <- p2,    {p1,p2} <- p3 *)
let diamond_patches () =
  [
    mk_patch ~id:"p0" ~deps:[];
    mk_patch ~id:"p1" ~deps:[ "p0" ];
    mk_patch ~id:"p2" ~deps:[ "p0" ];
    mk_patch ~id:"p3" ~deps:[ "p1"; "p2" ];
  ]

let enqueues_rebase_of actions pid =
  List.exists actions ~f:(function
    | Reconciler.Enqueue_rebase p -> Patch_id.equal p pid
    | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> false)

(* -- FLI-1 (witness): the exact production scenario -- *)

(** 1. Bootstrap p0..p2 with PRs; p3 (the fan-in patch) stays unstarted. 2. p0
    merges; p1 and p2 absorb main (now carrying p0's squash). 3. p2 merges. p1
    is *not* a dependent of p2, so no event/stale/drift detector will ever
    rebase it — its branch is structurally fresh on a pre-p2 main, missing p2's
    squash. 4. The reconcile tick must create the demand: [Enqueue_rebase p1],
    even though p3 has no PR. 5. After p1's rebase completes, p3's containment
    cache flips true and Start(p3, base=branch-p1) surfaces as runnable
    (eligibility Allow). *)
let prop_diamond_witness =
  QCheck2.Test.make ~count:1
    ~name:
      "FLI-1: unstarted fan-in patch's stale base gets a freshening rebase, \
       then its Start becomes runnable" (QCheck2.Gen.return ()) (fun () ->
      let patches = diamond_patches () in
      let p1 = pid_of_idx patches 1 in
      let p3 = pid_of_idx patches 3 in
      let branch_p1 = (List.nth_exn patches 1).Patch.branch in
      let m = bootstrap patches ~started:3 in
      let m = apply_command m (Pr_merged 0) in
      let m = apply_command m (Rebase_complete 1) in
      let m = apply_command m (Rebase_complete 2) in
      let m = apply_command m (Pr_merged 2) in
      let m, actions = tick m in
      let demand_created = enqueues_rebase_of actions p1 in
      let m = apply_command m (Rebase_complete 1) in
      let m, _actions = tick m in
      let containment_flipped =
        (Orchestrator.agent m.orch p3).Patch_agent.base_contains_merged_siblings
      in
      let start_runnable =
        Patch_controller.plan_messages m.orch ~patches:m.patches
        |> List.exists ~f:(fun msg ->
            match Orchestrator.message_action msg with
            | Orchestrator.Start (pid, base) ->
                Patch_id.equal pid p3 && Branch.equal base branch_p1
            | Orchestrator.Rebase _ | Orchestrator.Respond _ -> false)
      in
      demand_created && containment_flipped && start_runnable)

(* -- FLI-2 (exhaustive small-scope): all interleavings over the diamond -- *)

let rec permutations = function
  | [] -> [ [] ]
  | l ->
      List.concat_mapi l ~f:(fun i x ->
          let rest = List.filteri l ~f:(fun j _ -> j <> i) in
          List.map (permutations rest) ~f:(fun p -> x :: p))

(** Every ordering of a fixed event multiset over the diamond, with a tick after
    each event (infeasible events are no-ops, as in [apply_command]). Two
    multisets, each keeping one of [p1]/[p2] open so the fan-in patch must start
    *stacked* on the open dep. 2 x 4! = 48 orderings, run exhaustively.
    Orderings whose merges no-op (dep not yet merged) leave the fan-in patch
    legitimately unstartable; the [no_startable_unstarted] oracle is sound for
    those too. *)
let prop_diamond_exhaustive =
  QCheck2.Test.make ~count:1
    ~name:
      "FLI-2: all diamond interleavings (one dep left open) quiesce with no \
       startable-but-unstarted patch" (QCheck2.Gen.return ()) (fun () ->
      let event_sets =
        [
          [ Pr_merged 0; Pr_merged 2; Rebase_complete 1; Rebase_complete 2 ];
          [ Pr_merged 0; Pr_merged 1; Rebase_complete 1; Rebase_complete 2 ];
        ]
      in
      List.for_all event_sets ~f:(fun events ->
          List.for_all (permutations events) ~f:(fun ordering ->
              let patches = diamond_patches () in
              let m = bootstrap patches ~started:3 in
              let m =
                List.fold ordering ~init:m ~f:(fun m cmd ->
                    fst (tick (apply_command m cmd)))
              in
              let m = quiesce m ~rounds:((2 * List.length patches) + 3) in
              no_startable_unstarted m)))

(* -- Generators for random-DAG interleavings -- *)

module Gen = QCheck2.Gen

let gen_command ~n_patches =
  let open Gen in
  let idx = int_range 0 (n_patches - 1) in
  oneof
    [
      map (fun i -> Pr_merged i) idx;
      map (fun i -> Rebase_complete i) idx;
      map (fun i -> Start_via_gate i) idx;
    ]

let gen_scenario =
  Gen.(
    let* patches =
      oneof
        [
          Onton_test_support.Test_generators.gen_patch_dag;
          map mk_patches (int_range 2 5);
        ]
    in
    let n = List.length patches in
    (* Leave at least one patch unstarted: the liveness gap is specific to
       patches that must Start mid-flight. *)
    let* started = int_range 0 (n - 1) in
    let* cmds = list_size (int_range 1 30) (gen_command ~n_patches:n) in
    return (patches, started, cmds))

(* -- FLI-3 (random interleavings): liveness under fair quiescence -- *)

let prop_random_dag_liveness =
  QCheck2.Test.make ~count:300
    ~name:
      "FLI-3: random DAG interleavings quiesce with no startable-but-unstarted \
       patch" gen_scenario (fun (patches, started, cmds) ->
      let m = bootstrap patches ~started in
      let m =
        List.fold cmds ~init:m ~f:(fun m cmd ->
            fst (tick (apply_command m cmd)))
      in
      let m = quiesce m ~rounds:((2 * List.length patches) + 3) in
      no_startable_unstarted m)

(* -- FLI-4 (safety at every step): sibling-Defer implies demand -- *)

(** The precise property the [has_pr] guard violated: whenever an unstarted
    patch P's hypothetical Start is deferred on [Base_missing_merged_sibling],
    the freshening demand must already exist — its sole open dep B either has a
    [Rebase] queued or this very reconcile round emitted [Enqueue_rebase B].
    Checked after every command's tick.

    (A Defer on this arm presupposes B is structurally fresh — [decide] checks
    [base_structurally_fresh] first — which in this model implies B has Started
    and holds a PR, so [base_rebasable]'s B-side guard cannot be the blocker.)
*)
let sibling_defer_implies_demand m actions =
  List.for_all m.patches ~f:(fun (p : Patch.t) ->
      let agent = Orchestrator.agent m.orch p.Patch.id in
      if Patch_agent.has_pr agent || agent.Patch_agent.merged then true
      else
        let open_deps =
          Graph.open_pr_deps
            (Orchestrator.graph m.orch)
            p.Patch.id ~has_merged:(has_merged_in m.orch)
        in
        match open_deps with
        | [ b ] -> (
            let b_agent = Orchestrator.agent m.orch b in
            match
              Orchestrator.start_eligibility m.orch
                ~base_contains_merged_siblings:
                  agent.Patch_agent.base_contains_merged_siblings
                b_agent.Patch_agent.branch
            with
            | Start_eligibility.Defer
                (Start_eligibility.Base_missing_merged_sibling _) ->
                List.mem b_agent.Patch_agent.queue Operation_kind.Rebase
                  ~equal:Operation_kind.equal
                || enqueues_rebase_of actions b
            | Start_eligibility.Allow
            | Start_eligibility.Defer
                ( Start_eligibility.Base_patch_busy_with_rebase _
                | Start_eligibility.Base_not_fresh_for_cut _ ) ->
                true)
        | _ -> true)

let prop_sibling_defer_implies_demand =
  QCheck2.Test.make ~count:300
    ~name:
      "FLI-4: an unstarted patch deferred on Base_missing_merged_sibling has \
       rebase demand for its base that same tick" gen_scenario
    (fun (patches, started, cmds) ->
      let m = bootstrap patches ~started in
      let _final, ok =
        List.fold cmds ~init:(m, true) ~f:(fun (m, ok) cmd ->
            let m = apply_command m cmd in
            let m, actions = tick m in
            (m, ok && sibling_defer_implies_demand m actions))
      in
      ok)

let () =
  let runner = QCheck_base_runner.run_tests_main in
  ignore
    (runner
       [
         prop_diamond_witness;
         prop_diamond_exhaustive;
         prop_random_dag_liveness;
         prop_sibling_defer_implies_demand;
       ])
