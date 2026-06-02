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
    the fixpoint is exactly the liveness violation.

    The NUR properties (no unnecessary rebases) are the {e inverse}: freshness
    must be available at the transitions where it matters (FLI), and {e only}
    there (NUR). The historical failure mode they pin down is the
    rebase-on-every-main-advance herd (main-SHA-scoped freshness force-pushed
    and re-ran CI on every open PR each time main moved): NUR-1 asserts the
    quiescence fixpoint is rebase-silent, NUR-2 asserts main advances from work
    outside the gameplan are completely inert, and NUR-3 asserts every emitted
    rebase demand and every rewriting rebase is justified by dependency-scoped
    need — a missing relevant squash (own merged lineage or a fan-in consumer's
    merged sibling), a structural misplacement, or un-stranding from a rewritten
    patch-branch base. Together the two suites bracket the scheduler: rebases
    happen exactly when freshness demands them. *)

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

(** Start [pid] from [base]: PR appears, implementation notes are delivered, and
    the session completes, mirroring the SBI bootstrap plus the deps-notes-ready
    contract (fire Start → set_pr_number → Pr_body delivered → complete). The
    notes step matters: the deps-notes-ready Start gate
    ([Patch_controller.plan_action_for_patch]) defers a child's Start until
    every open dep has [pr_body_delivered] — these tests target rebase freshness
    liveness, so the model publishes notes promptly; the notes gate's own
    liveness has its own properties ([test_interleaving_properties.ml]). The
    branch absorbs its base's current merge SHAs (the cut). No-op when already
    started or when dependencies are not satisfied (cannot cut a worktree off a
    missing base). *)
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
    let orch = Orchestrator.set_pr_body_delivered orch pid true in
    let orch = Orchestrator.complete orch pid in
    absorb { m with orch } ~branch:agent.Patch_agent.branch ~from:base

type fired_rebase = {
  fired_pid : Patch_id.t;
  fired_outcome : [ `Ok | `Noop ];
  fired_delta : Set.M(String).t;
      (** Merge SHAs newly absorbed — empty for [`Noop]. *)
  fired_anchor_before : Branch.t option;
  fired_target : Branch.t;
}
(** One executed rebase, as observed by the no-unnecessary-rebase (NUR)
    properties: which patch fired, whether it rewrote anything, the merge SHAs
    it absorbed, where its branch sat beforehand, and the target. *)

(** Complete a rebase of [pid] onto [base], mirroring production's
    [Worktree.rebase_onto] outcome split: when the branch already contains
    everything the target has (production:
    [merge-base --is-ancestor target HEAD]; model: the target's absorbed SHA set
    is a subset of the branch's), the result is [Noop] — the anchor is still
    recorded but nothing is rewritten and the rewrite cascade does not fire.
    Otherwise [Ok]: the branch re-cuts onto the target, absorbing its current
    merge SHAs, and stacked children are cascade-enqueued. The Ok/Noop fidelity
    is what makes the NUR (parsimony) properties meaningful: a redundant queued
    rebase on an already-fresh branch must register as no-work, exactly as in
    production. Returns the {!fired_rebase} observation alongside the new model.
*)
let do_rebase_traced m pid base =
  let agent = Orchestrator.agent m.orch pid in
  if
    (not (Patch_agent.has_pr agent))
    || agent.Patch_agent.merged || agent.Patch_agent.busy
  then (m, None)
  else
    let base_set = absorbed_of m (Branch.to_string base) in
    let own_set = absorbed_of m (Branch.to_string agent.Patch_agent.branch) in
    let anchor_before = agent.Patch_agent.branch_rebased_onto in
    let orch = Orchestrator.fire m.orch (Orchestrator.Rebase (pid, base)) in
    if Set.is_subset base_set ~of_:own_set then
      let orch, _effects =
        Orchestrator.apply_rebase_result orch pid Worktree.Noop base
      in
      ( { m with orch },
        Some
          {
            fired_pid = pid;
            fired_outcome = `Noop;
            fired_delta = Set.empty (module String);
            fired_anchor_before = anchor_before;
            fired_target = base;
          } )
    else
      let orch, _effects =
        Orchestrator.apply_rebase_result orch pid Worktree.Ok base
      in
      ( absorb { m with orch } ~branch:agent.Patch_agent.branch ~from:base,
        Some
          {
            fired_pid = pid;
            fired_outcome = `Ok;
            fired_delta = Set.diff base_set own_set;
            fired_anchor_before = anchor_before;
            fired_target = base;
          } )

let do_rebase m pid base = fst (do_rebase_traced m pid base)

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
  | Unrelated_main_advance of int
      (** Main absorbs a merge SHA that is no gameplan patch's squash — a human
          merging unrelated work. Freshness is dependency-scoped, so this must
          never create rebase demand or defer anything (the historical failure
          mode was rebasing every open patch on every main advance). The NUR
          properties assert exactly that; the liveness properties assert it
          never breaks convergence either. *)

let apply_command m cmd =
  match cmd with
  | Unrelated_main_advance i ->
      {
        m with
        absorbed =
          Map.set m.absorbed ~key:main_s
            ~data:
              (Set.add (absorbed_of m main_s) (Printf.sprintf "unrelated-%d" i));
      }
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

let view_of_agent ~sibling_rebase_target (a : Patch_agent.t) :
    Reconciler.patch_view =
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
    sibling_rebase_target;
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
  let views =
    List.map (Orchestrator.all_agents m.orch) ~f:(fun (a : Patch_agent.t) ->
        (* Mirror [poller_fiber]: the frontier is computed (same oracle as the
           containment fold) only when containment just read false. *)
        let sibling_rebase_target =
          if a.Patch_agent.base_contains_merged_siblings then None
          else
            Base_containment.stale_chain_rebase_target ~graph
              ~patch_id:a.Patch_agent.patch_id ~has_merged ~merge_sha ~branch_of
              ~main ~ancestor_oracle
        in
        view_of_agent ~sibling_rebase_target a)
  in
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
    fair runner draining one planning round. Returns the rebases that actually
    executed so the NUR properties can audit them. *)
let fire_runnable_traced m =
  let msgs = Patch_controller.plan_messages m.orch ~patches:m.patches in
  List.fold msgs ~init:(m, []) ~f:(fun (m, fired) msg ->
      match Orchestrator.message_action msg with
      | Orchestrator.Start (pid, base) -> (do_start m pid base, fired)
      | Orchestrator.Rebase (pid, base) -> (
          let open_deps =
            Graph.open_pr_deps
              (Orchestrator.graph m.orch)
              pid ~has_merged:(has_merged_in m.orch)
          in
          if List.length open_deps > 1 then (m, fired)
          else
            match do_rebase_traced m pid base with
            | m, Some f -> (m, f :: fired)
            | m, None -> (m, fired))
      | Orchestrator.Respond _ -> (m, fired))

let fire_runnable m = fst (fire_runnable_traced m)

(** Fair quiescence: with external events stopped, run [n] rounds of tick +
    fire-runnable. A correct system converges well within [2 * n_patches + 3]
    rounds (one freshening rebase plus one Start per dependency layer, plus
    slack for the final containment recompute). *)
let quiesce m ~rounds =
  Fn.apply_n_times ~n:rounds (fun m -> fire_runnable (fst (tick m))) m

(** [b]'s sole-open-dep chain, [b] included, down to the main-based layer.
    Mirrors the frontier walk's domain; a multi-open-dep layer ends it. *)
let open_chain_of m b =
  let rec go acc pid =
    match
      Graph.open_pr_deps
        (Orchestrator.graph m.orch)
        pid ~has_merged:(has_merged_in m.orch)
    with
    | [ d ] -> go (d :: acc) d
    | [] | _ :: _ :: _ -> acc
  in
  go [ b ] b

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
      map (fun i -> Unrelated_main_advance i) idx;
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

let show_command = function
  | Pr_merged i -> Printf.sprintf "Pr_merged %d" i
  | Rebase_complete i -> Printf.sprintf "Rebase_complete %d" i
  | Start_via_gate i -> Printf.sprintf "Start_via_gate %d" i
  | Unrelated_main_advance i -> Printf.sprintf "Unrelated_main_advance %d" i

let show_scenario (patches, started, cmds) =
  let patch_s (p : Patch.t) =
    Printf.sprintf "%s<-[%s]"
      (Patch_id.to_string p.Patch.id)
      (String.concat ~sep:","
         (List.map p.Patch.dependencies ~f:Patch_id.to_string))
  in
  Printf.sprintf "patches=[%s] started=%d cmds=[%s]"
    (String.concat ~sep:"; " (List.map patches ~f:patch_s))
    started
    (String.concat ~sep:"; " (List.map cmds ~f:show_command))

let prop_random_dag_liveness =
  QCheck2.Test.make ~count:300 ~print:show_scenario
    ~name:
      "FLI-3: random DAG interleavings quiesce with no startable-but-unstarted \
       patch" gen_scenario (fun (patches, started, cmds) ->
      try
        let m = bootstrap patches ~started in
        let m =
          List.fold cmds ~init:m ~f:(fun m cmd ->
              fst (tick (apply_command m cmd)))
        in
        let m = quiesce m ~rounds:((2 * List.length patches) + 3) in
        no_startable_unstarted m
      with exn ->
        Stdlib.Printf.eprintf "FLI-3 raised: %s\n%!" (Exn.to_string exn);
        false)

(* -- FLI-4 (safety at every step): sibling-Defer implies demand -- *)

(** The precise property the [has_pr] guard violated: whenever an unstarted
    patch P's hypothetical Start is deferred on [Base_missing_merged_sibling],
    the freshening demand must already exist {e somewhere on B's open chain} —
    the missing squash flows up from main one layer at a time
    ([Base_containment.stale_chain_rebase_target]), so the demand may
    legitimately sit on a layer below B (the frontier) rather than on B itself.
    Demand means: some chain layer has a [Rebase] queued, or this very reconcile
    round emitted [Enqueue_rebase] for one, or a chain layer has no PR yet (its
    future Start cuts fresh from its base, healing that layer without a rebase).
    Checked after every command's tick.

    (A Defer on this arm presupposes B is structurally fresh — [decide] checks
    [base_structurally_fresh] first — which in this model implies B has Started
    and holds a PR.) *)
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
                List.exists (open_chain_of m b) ~f:(fun c ->
                    let c_agent = Orchestrator.agent m.orch c in
                    (not (Patch_agent.has_pr c_agent))
                    || List.mem c_agent.Patch_agent.queue Operation_kind.Rebase
                         ~equal:Operation_kind.equal
                    || enqueues_rebase_of actions c)
            | Start_eligibility.Allow
            | Start_eligibility.Defer
                ( Start_eligibility.Base_patch_busy_with_rebase _
                | Start_eligibility.Base_resolving_conflict _
                | Start_eligibility.Base_not_fresh_for_cut _ ) ->
                true)
        | _ -> true)

let prop_sibling_defer_implies_demand =
  QCheck2.Test.make ~count:300
    ~name:
      "FLI-4: an unstarted patch deferred on Base_missing_merged_sibling has \
       rebase demand for its base that same tick" gen_scenario
    (fun (patches, started, cmds) ->
      try
        let m = bootstrap patches ~started in
        let _final, ok =
          List.fold cmds ~init:(m, true) ~f:(fun (m, ok) cmd ->
              let m = apply_command m cmd in
              let m, actions = tick m in
              (m, ok && sibling_defer_implies_demand m actions))
        in
        ok
      with _ -> false)

(* -- FLI-5 / FLI-6 (witnesses): the two chain-under-fan-in livelocks FLI-3
   found at seeds 440463877 and 7 -- *)

let started_everywhere m =
  List.for_all m.patches ~f:(fun (p : Patch.t) ->
      let a = Orchestrator.agent m.orch p.Patch.id in
      Patch_agent.has_pr a || a.Patch_agent.merged)

(** Seed-440463877 shape — {e rewrite stranding}. p4 fans in on the merged root
    p0 and the top of an open chain p3 ← p2 ← p1 ← p0. When p0 merges, p1's
    freshening rebase onto main rewrites branch-p1, stranding p2 (and
    transitively p3) on dead history that lacks p0's squash: every name-based
    detector reads p2 as fresh, and the sibling detector used to rebase only p3
    — onto a branch-p2 that still lacked the squash — re-firing forever while p4
    never became startable. The rewrite cascade
    ([Orchestrator.apply_rebase_result] → stranded dependents) plus the
    frontier-targeted sibling demand drive the squash up the chain one layer per
    round; p4 then starts. *)
let prop_chain_fanin_rewrite_witness =
  QCheck2.Test.make ~count:1
    ~name:
      "FLI-5: rewrite-stranded chain under a fan-in patch heals \
       (seed-440463877 witness)" (QCheck2.Gen.return ()) (fun () ->
      let patches =
        [
          mk_patch ~id:"p0" ~deps:[];
          mk_patch ~id:"p1" ~deps:[ "p0" ];
          mk_patch ~id:"p2" ~deps:[ "p1" ];
          mk_patch ~id:"p3" ~deps:[ "p2" ];
          mk_patch ~id:"p4" ~deps:[ "p0"; "p3" ];
          mk_patch ~id:"p5" ~deps:[ "p0" ];
        ]
      in
      let m = bootstrap patches ~started:3 in
      let m = fst (tick (apply_command m (Pr_merged 0))) in
      let m = quiesce m ~rounds:((2 * List.length patches) + 3) in
      no_startable_unstarted m && started_everywhere m)

(** Seed-7 shape — {e merged-sibling content deep in the chain}. p5 fans in on
    p1 and p3, with p3 ← p2 ← p0 and p1 merging straight to main (its squash
    never enters p2's lineage — p1 is not a dependency of p2, so no detector
    ever saw a reason to rebase p2). The squash can only reach branch-p3 by p2
    first rebasing onto main; rebasing p3 alone re-fires forever. The frontier
    ([Base_containment.stale_chain_rebase_target]) targets p2 first, then p3,
    and p5 starts. *)
let prop_chain_fanin_merge_witness =
  QCheck2.Test.make ~count:1
    ~name:
      "FLI-6: merged-sibling squash flows up the open chain under a fan-in \
       patch (seed-7 witness)" (QCheck2.Gen.return ()) (fun () ->
      let patches =
        [
          mk_patch ~id:"p0" ~deps:[];
          mk_patch ~id:"p1" ~deps:[ "p0" ];
          mk_patch ~id:"p2" ~deps:[ "p0" ];
          mk_patch ~id:"p3" ~deps:[ "p2" ];
          mk_patch ~id:"p4" ~deps:[ "p0" ];
          mk_patch ~id:"p5" ~deps:[ "p1"; "p3" ];
        ]
      in
      let m = bootstrap patches ~started:3 in
      let m = fst (tick (apply_command m (Pr_merged 0))) in
      let m = fst (tick (apply_command m (Rebase_complete 1))) in
      let m = fst (tick (apply_command m (Rebase_complete 2))) in
      let m = fst (tick (apply_command m (Pr_merged 1))) in
      let m = quiesce m ~rounds:((2 * List.length patches) + 3) in
      no_startable_unstarted m && started_everywhere m)

(* -- NUR (no unnecessary rebases): the inverse of the liveness suite -- *)

(** The merge SHAs whose absence from [pid]'s branch would justify rebasing it,
    derived from the dependency-scoped freshness spec:

    - squashes of [pid]'s own merged {e transitive} ancestors (its branch must
      carry its lineage's merged form — covers direct dep merges and the
      rewrite-stranding case, where the content a stranded child re-absorbs is
      its rewritten ancestor's merged deps);
    - squashes of the merged deps of any fan-in patch [P] whose open base chain
      runs through [pid] (the frontier consumers: [pid] must carry the sibling
      content so [P] can eventually cut from a complete base).

    Everything else — in particular a main advance from work outside these sets
    — is noise that must never produce rebase demand. *)
let relevant_shas m pid =
  let graph = Orchestrator.graph m.orch in
  let has_merged = has_merged_in m.orch in
  let own_lineage =
    Graph.transitive_ancestors graph pid
    |> List.filter ~f:has_merged |> List.map ~f:merge_sha_of
  in
  let consumer_siblings =
    List.concat_map m.patches ~f:(fun (p : Patch.t) ->
        let consumer = p.Patch.id in
        if Patch_id.equal consumer pid then []
        else
          match Graph.open_pr_deps graph consumer ~has_merged with
          | [ b ] when List.mem (open_chain_of m b) pid ~equal:Patch_id.equal ->
              Graph.deps graph consumer |> List.filter ~f:has_merged
              |> List.map ~f:merge_sha_of
          | [] | [ _ ] | _ :: _ :: _ -> [])
  in
  Set.of_list (module String) (own_lineage @ consumer_siblings)

(** Is rebase demand for [pid] justified right now? Either the branch is
    structurally misplaced (its recorded anchor is not its structural base — the
    dep-merge / retarget cases), or a relevant squash is missing from the
    branch. An unstarted patch (no anchor) is exempt: an inert queued rebase
    cannot waste work, and its eventual Start cuts fresh. *)
let rebase_demand_justified m pid =
  let agent = Orchestrator.agent m.orch pid in
  match agent.Patch_agent.branch_rebased_onto with
  | None -> true
  | Some anchor ->
      let structurally_misplaced =
        match
          Graph.open_pr_deps
            (Orchestrator.graph m.orch)
            pid ~has_merged:(has_merged_in m.orch)
        with
        | [] | [ _ ] -> not (Branch.equal anchor (initial_base_of m pid))
        | _ :: _ :: _ -> false
      in
      let own_set = absorbed_of m (Branch.to_string agent.Patch_agent.branch) in
      let missing_relevant =
        Set.exists (relevant_shas m pid) ~f:(fun sha ->
            not (Set.mem own_set sha))
      in
      structurally_misplaced || missing_relevant

let enqueued_rebases actions =
  List.filter_map actions ~f:(function
    | Reconciler.Enqueue_rebase pid -> Some pid
    | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> None)

(** NUR-1 (fixpoint rebase-silence): once a scenario quiesces, further fair
    rounds plan no Start/Rebase, the reconciler emits no rebase demand, and no
    rebase executes — not even a [`Noop]. The frontier livelock this suite
    started from manifested as exactly this kind of perpetual churn (an endless
    force-push per tick); so would the historical rebase-on-every-main-advance
    regime. *)
let prop_fixpoint_rebase_silence =
  QCheck2.Test.make ~count:200 ~print:show_scenario
    ~name:
      "NUR-1: at the quiescence fixpoint, no rebase is demanded, planned, or \
       executed" gen_scenario (fun (patches, started, cmds) ->
      try
        let m = bootstrap patches ~started in
        let m =
          List.fold cmds ~init:m ~f:(fun m cmd ->
              fst (tick (apply_command m cmd)))
        in
        let m = quiesce m ~rounds:((2 * List.length patches) + 3) in
        let _m, ok =
          Fn.apply_n_times ~n:3
            (fun (m, ok) ->
              let m, actions = tick m in
              let no_demand = List.is_empty (enqueued_rebases actions) in
              let no_plan =
                Patch_controller.plan_messages m.orch ~patches:m.patches
                |> List.for_all ~f:(fun msg ->
                    match Orchestrator.message_action msg with
                    | Orchestrator.Start _ | Orchestrator.Rebase _ -> false
                    | Orchestrator.Respond _ -> true)
              in
              let m, fired = fire_runnable_traced m in
              (m, ok && no_demand && no_plan && List.is_empty fired))
            (m, true)
        in
        ok
      with exn ->
        Stdlib.Printf.eprintf "NUR-1 raised: %s\n%!" (Exn.to_string exn);
        false)

(** NUR-2 (unrelated main advances are inert): from a quiesced state, main
    absorbing squashes of work outside the gameplan must not move anything — no
    rebase demand, nothing planned, nothing executed. This is the exact
    historical failure mode (main-SHA-scoped freshness rebased every open patch
    on every main advance) pinned as a regression test. *)
let prop_unrelated_main_advance_inert =
  QCheck2.Test.make ~count:200 ~print:show_scenario
    ~name:
      "NUR-2: unrelated main advances at the fixpoint demand, plan, and \
       execute no rebases" gen_scenario (fun (patches, started, cmds) ->
      try
        let m = bootstrap patches ~started in
        let m =
          List.fold cmds ~init:m ~f:(fun m cmd ->
              fst (tick (apply_command m cmd)))
        in
        let m = quiesce m ~rounds:((2 * List.length patches) + 3) in
        let _m, ok =
          List.fold [ 1000; 1001; 1002 ] ~init:(m, true) ~f:(fun (m, ok) i ->
              let m = apply_command m (Unrelated_main_advance i) in
              let m, actions = tick m in
              let no_demand = List.is_empty (enqueued_rebases actions) in
              let m, fired = fire_runnable_traced m in
              (m, ok && no_demand && List.is_empty fired))
        in
        ok
      with exn ->
        Stdlib.Printf.eprintf "NUR-2 raised: %s\n%!" (Exn.to_string exn);
        false)

(** NUR-3 (all rebase work is justified): at every step of a random scenario —
    command ticks and quiescence rounds alike —

    + every [Enqueue_rebase] the reconciler emits targets a patch whose rebase
      demand is justified ({!rebase_demand_justified}); and
    + every rebase that actually {e rewrites} ([`Ok]) is necessary:

    - onto {e main}: it absorbed at least one relevant squash or fixed a
      structural misplacement (its anchor moved). This is where the historical
      herd lived — main is append-only, so an Ok rebase onto it absorbing only
      unrelated content is pure waste.
    - onto a {e patch branch}: always necessary. In this model a patch branch
      only ever changes by rewrite (cut/rebase snapshots), so [`Ok] means the
      child's history references commits the base force-replaced — the
      un-stranding rebase the cascade exists for. (Its absorbed delta can be
      entirely sibling content relevant to a fan-in consumer elsewhere, e.g. a
      child hanging off a frontier-healed mid-chain layer, so SHA-relevance is
      the wrong oracle here.)

    [`Noop] executions are tolerated mid-convergence (a queued demand can be
    healed from elsewhere before it fires — e.g. a fresh cut absorbed the
    content first); they rewrite nothing and push no new commits, and NUR-1
    asserts they die out at the fixpoint.

    Together with the liveness suite this brackets the scheduler from both
    sides: rebases happen exactly when freshness demands them. *)
let prop_all_rebase_work_justified =
  QCheck2.Test.make ~count:200 ~print:show_scenario
    ~name:
      "NUR-3: every emitted rebase demand and every rewriting rebase is \
       justified" gen_scenario (fun (patches, started, cmds) ->
      try
        let step (m, ok) pre =
          let m = pre m in
          (* Demand justification is evaluated on the pre-tick model state:
             [tick] both emits the demand and (for [Mark_merged]) updates the
             state the demand was derived from. *)
          let demand_justified_in m_before actions =
            List.for_all (enqueued_rebases actions) ~f:(fun pid ->
                rebase_demand_justified m_before pid)
          in
          let m_before = m in
          let m, actions = tick m in
          let demand_ok = demand_justified_in m_before actions in
          let m_pre_fire = m in
          let m, fired = fire_runnable_traced m in
          let fired_ok =
            List.for_all fired ~f:(fun f ->
                match f.fired_outcome with
                | `Noop -> true
                | `Ok when not (Branch.equal f.fired_target main) ->
                    (* Un-stranding: patch branches only move by rewrite, so
                       an Ok rebase onto one means the base was force-replaced
                       under this child. *)
                    true
                | `Ok ->
                    let relevant = relevant_shas m_pre_fire f.fired_pid in
                    (not (Set.is_empty (Set.inter f.fired_delta relevant)))
                    || not
                         (Option.equal Branch.equal f.fired_anchor_before
                            (Some f.fired_target)))
          in
          (m, ok && demand_ok && fired_ok)
        in
        let acc =
          List.fold cmds
            ~init:(bootstrap patches ~started, true)
            ~f:(fun acc cmd -> step acc (fun m -> apply_command m cmd))
        in
        let _m, ok =
          Fn.apply_n_times
            ~n:((2 * List.length patches) + 3)
            (fun acc -> step acc Fn.id)
            acc
        in
        ok
      with exn ->
        Stdlib.Printf.eprintf "NUR-3 raised: %s\n%!" (Exn.to_string exn);
        false)

(** NUR-4 (witness): the stale-eager-enqueue waste NUR-3 first caught.
    [mark_merged] used to enqueue a [Rebase] on PR-less dependents; the demand
    sat inert through the patch's (fresh) cut and then fired anyway — and if
    main had absorbed unrelated work in between, the rebase was an [`Ok] rewrite
    (force-push + CI re-run) whose entire delta was content nobody needed. The
    [has_pr] guard on the eager enqueue (mirroring [detect_rebases]) removes the
    demand; this witness replays the shrunk counterexample and asserts no
    unjustified rewrite ever fires.

    NOTE the ordering dependency this guard assumes: a PR-less dependent's
    eventual cut must itself be fresh (the base-fetch fix for main-based cuts).
    Before that, the inert queued rebase was accidentally load-bearing — it
    healed stale cuts shortly after Start (the connector-adapter patch-4 trace).
*)
let prop_stale_eager_enqueue_witness =
  QCheck2.Test.make ~count:1
    ~name:
      "NUR-4: a dep merge while the dependent is PR-less creates no rebase \
       demand that later fires as an unrelated-content rewrite"
    (QCheck2.Gen.return ()) (fun () ->
      let patches =
        [ mk_patch ~id:"p0" ~deps:[]; mk_patch ~id:"p1" ~deps:[ "p0" ] ]
      in
      let step (m, ok) pre =
        let m = pre m in
        let m_before = m in
        let m, actions = tick m in
        let demand_ok =
          List.for_all (enqueued_rebases actions) ~f:(fun pid ->
              rebase_demand_justified m_before pid)
        in
        let m_pre_fire = m in
        let m, fired = fire_runnable_traced m in
        let fired_ok =
          List.for_all fired ~f:(fun f ->
              match f.fired_outcome with
              | `Noop -> true
              | `Ok when not (Branch.equal f.fired_target main) -> true
              | `Ok ->
                  let relevant = relevant_shas m_pre_fire f.fired_pid in
                  (not (Set.is_empty (Set.inter f.fired_delta relevant)))
                  || not
                       (Option.equal Branch.equal f.fired_anchor_before
                          (Some f.fired_target)))
        in
        (m, ok && demand_ok && fired_ok)
      in
      (* p0 starts; p0 merges while p1 is still PR-less; p1 starts (fresh
         cut); main then absorbs unrelated work. Nothing may rebase p1. *)
      let acc =
        List.fold
          [ Pr_merged 0; Pr_merged 0; Unrelated_main_advance 0 ]
          ~init:(bootstrap patches ~started:0, true)
          ~f:(fun acc cmd -> step acc (fun m -> apply_command m cmd))
      in
      let _m, ok = Fn.apply_n_times ~n:7 (fun acc -> step acc Fn.id) acc in
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
         prop_chain_fanin_rewrite_witness;
         prop_chain_fanin_merge_witness;
         prop_fixpoint_rebase_silence;
         prop_unrelated_main_advance_inert;
         prop_all_rebase_work_justified;
         prop_stale_eager_enqueue_witness;
       ])
