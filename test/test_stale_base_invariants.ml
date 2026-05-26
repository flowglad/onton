open Base
open Onton
open Onton_core
open Onton_core.Types

(** Stale-Base Invariant (SBI) property tests.

    Freshness is dependency-scoped, not main-scoped. For every reachable
    orchestrator state and every [Start (c, base)] action that actually fires
    from [Orchestrator.runnable_messages] → [Orchestrator.accept_message], the
    base is fresh — meaning at fire time:

    + [base = main_branch], OR
    + the base patch (looked up by branch) is [merged], OR
    + the base patch's local branch is rebased onto its structurally-correct
      base for the current merge state
      ([branch_rebased_onto = Graph.initial_base]).

    Crucially, an unrelated advance of [origin/main] never makes a base stale:
    the gate does not consult any main sha. A base sitting directly on main, or
    a base that already absorbed its merged deps, stays eligible no matter how
    far main moves.

    The harness:

    - Models a linear A→B→C→… dep chain of length 2 or 3 with
      [mk_linear_patches], so each non-root patch has exactly one dep.
    - Models an unrelated advance of main via [Main_advanced]. Since freshness
      is no longer main-scoped, this is a no-op on orchestrator state — included
      to assert that interleaving main movement never trips the gate.
    - Models PR merges via [Pr_merged i], which calls [Orchestrator.mark_merged]
      and (per [Orchestrator.mark_merged]'s eager enqueue) implicitly enqueues a
      [Rebase] for the dependent. This is the only thing that makes a
      dependent's base stale.
    - Models rebase completion via [Rebase_complete i], which applies a
      successful rebase onto the patch's current structural base
      ([Graph.initial_base]); [apply_rebase_result] records
      [branch_rebased_onto = that base]. This is the operation that closes the
      freshness gap.
    - Models the controller's Start-firing loop indirectly: only
      eligibility-passing Starts reach [runnable_messages]; the test asserts
      freshness directly from the model. *)

let main = Branch.of_string "main"
let mk_patches = Onton_test_support.Test_generators.mk_linear_patches

(* -- Model state -- *)

type model = { orch : Orchestrator.t; patches : Patch.t list }

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

(** Structural base for [pid] given the current orchestrator merge state. *)
let initial_base_of orch patches pid =
  let branch_of = branch_of_patches patches in
  let has_merged dep_pid =
    (Orchestrator.agent orch dep_pid).Patch_agent.merged
  in
  Graph.initial_base (Orchestrator.graph orch) pid ~has_merged ~branch_of ~main

(** Bootstrap mirroring the patch-controller boot sequence: each patch starts
    with base = its structural base (its dep's branch, or main for the root),
    receives a PR number, and completes. [Patch_agent.start] records
    [branch_rebased_onto = base], so every base is structurally fresh at boot.
*)
let bootstrap patches =
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let orch =
    List.foldi patches ~init:orch ~f:(fun i o _p ->
        let pid = pid_of_idx patches i in
        let base = initial_base_of o patches pid in
        let o = Orchestrator.fire o (Orchestrator.Start (pid, base)) in
        let o = Orchestrator.set_pr_number o pid (Pr_number.of_int (i + 1)) in
        Orchestrator.complete o pid)
  in
  { orch; patches }

(* -- Commands -- *)

type command =
  | Main_advanced
      (** An unrelated advance of [origin/main]. No-op on orchestrator state —
          freshness is dependency-scoped, so this must never defer a Start. *)
  | Pr_merged of int
      (** Mark patch idx as merged. [mark_merged] eagerly enqueues a [Rebase]
          for the dependent, whose base is now stale until that rebase lands. *)
  | Rebase_complete of int
      (** Apply a successful rebase onto patch idx's current structural base;
          records [branch_rebased_onto = that base]. Closes the freshness gap.
      *)
  | Enqueue_start of int
      (** Place a [Start] in patch idx's outbox so the freshness gate can be
          observed against it. Bootstrap already placed and consumed the initial
          Start; this re-enqueues one for testing. *)

let apply_command m cmd =
  match cmd with
  | Main_advanced -> m
  | Pr_merged i ->
      if i >= List.length m.patches then m
      else
        let pid = pid_of_idx m.patches i in
        (* The reconciler only ever marks a *newly* merged patch
           ([detect_merges] guards on [not v.merged]); re-marking is never
           issued. Mirror that here, else a redundant [mark_merged] would
           re-enqueue a [Rebase] on an already-fresh dependent. *)
        if (Orchestrator.agent m.orch pid).Patch_agent.merged then m
        else
          let orch = Orchestrator.mark_merged m.orch pid in
          { m with orch }
  | Rebase_complete i ->
      if i >= List.length m.patches then m
      else
        let pid = pid_of_idx m.patches i in
        let agent = Orchestrator.agent m.orch pid in
        (* Apply rebase only if its preconditions hold: PR present,
           not merged/busy, and Rebase is queued. Otherwise this command is
           a no-op (the generator picks random patches; many won't be ready). *)
        let rebase_in_queue =
          List.mem agent.Patch_agent.queue Operation_kind.Rebase
            ~equal:Operation_kind.equal
        in
        if
          (not (Patch_agent.has_pr agent))
          || agent.Patch_agent.merged || agent.Patch_agent.busy
          || not rebase_in_queue
        then m
        else
          (* Drives the agent state-machine directly (fire → apply result)
             rather than the message lifecycle; safe for the SBI property, which
             only reads base freshness from agent fields — [merged],
             [branch_rebased_onto], and (via [start_eligibility]'s busy-rebase
             check) [busy]/[current_op]/[queue]. The rebase target is the
             patch's current structural base, which [apply_rebase_result] then
             records as [branch_rebased_onto]. *)
          let target = initial_base_of m.orch m.patches pid in
          let orch =
            Orchestrator.fire m.orch (Orchestrator.Rebase (pid, target))
          in
          let orch, _effects =
            Orchestrator.apply_rebase_result orch pid Worktree.Ok target
          in
          { m with orch }
  | Enqueue_start i ->
      if i >= List.length m.patches then m
      else
        let pid = pid_of_idx m.patches i in
        let agent = Orchestrator.agent m.orch pid in
        let base = initial_base_of m.orch m.patches pid in
        (* Inject a real [Pending] Start(pid, base) into the outbox so the
           freshness gate in [runnable_messages] is actually exercised against
           it. [base] varies as deps merge / rebase, so the gate is observed
           against fresh and stale bases alike. The id is keyed on
           (patch, base, generation) — mirroring the controller — so a base
           change re-keys the message and [reconcile_message] obsoletes the
           prior Start for this patch. *)
        let message_id =
          Message_id.of_string
            (Printf.sprintf "%s:sbi-start:%s:%d" (Patch_id.to_string pid)
               (Branch.to_string base) agent.Patch_agent.generation)
        in
        let msg =
          {
            Orchestrator.message_id;
            patch_id = pid;
            generation = agent.Patch_agent.generation;
            action = Orchestrator.Start (pid, base);
            payload_hash = "sbi";
            status = Orchestrator.Pending;
          }
        in
        { m with orch = Orchestrator.reconcile_message m.orch msg }

(* -- Invariants -- *)

(** Independent freshness oracle: [true] iff [base] is fresh per the documented
    SBI invariant — recomputed directly from agent fields and the dep graph
    rather than delegating back to [start_eligibility], so the property
    genuinely checks that [runnable_messages]' gate enforces freshness (a
    regression that let a stale Start through would be caught) instead of
    re-deriving the same verdict. *)
let base_is_fresh m base =
  if Branch.equal base main then true
  else
    let base_entry =
      Map.to_alist (Orchestrator.agents_map m.orch)
      |> List.find ~f:(fun (_, a) -> Branch.equal a.Patch_agent.branch base)
    in
    match base_entry with
    | None -> false
    | Some (bpid, a) -> (
        if a.Patch_agent.merged then true
        else
          (* Bind [open_deps] once and pattern-match to derive the structural
             base directly ([] -> main, [d] -> d's branch), treating >1 open
             deps as not-fresh — mirroring [Orchestrator.start_eligibility].
             Keeping the guard and the base derivation in a single match means
             no reordering can leave [Graph.initial_base] (which raises on >1
             open deps) called unguarded; deriving the base inline also keeps
             this oracle independent of [start_eligibility]. *)
          let open_deps =
            Graph.open_pr_deps (Orchestrator.graph m.orch) bpid
              ~has_merged:(fun dep ->
                (Orchestrator.agent m.orch dep).Patch_agent.merged)
          in
          match (open_deps, a.Patch_agent.branch_rebased_onto) with
          | [], Some b -> Branch.equal b main
          | [ d ], Some b -> Branch.equal b (branch_of_patches m.patches d)
          | _ -> false)

(** SBI: every [Start (_, base)] surfaced by [runnable_messages] has a fresh
    [base] per [base_is_fresh]. [Enqueue_start] seeds the outbox with real
    Pending Starts whose base varies, so this is non-vacuous. *)
let sbi_runnable_starts_are_fresh m =
  let runnable = Orchestrator.runnable_messages m.orch in
  List.for_all runnable ~f:(fun msg ->
      match Orchestrator.message_action msg with
      | Orchestrator.Start (_, base) -> base_is_fresh m base
      | Orchestrator.Rebase _ | Orchestrator.Respond _ -> true)

(** SBI-2 (liveness): any patch deferred by the freshness gate is making
    progress toward Allow — either a [Rebase] is queued/in-flight for its base
    patch, or the base patch is already merged (and a [refresh_base_branch] can
    shift the base to main). This rules out states where Defer fires with no
    path to Allow. *)
let sbi_defer_implies_progress m =
  let agents = Orchestrator.all_agents m.orch in
  List.for_all agents ~f:(fun (a : Patch_agent.t) ->
      match a.Patch_agent.base_branch with
      | None -> true
      | Some base -> (
          match Orchestrator.start_eligibility m.orch base with
          | Start_eligibility.Allow -> true
          | Start_eligibility.Defer
              (Start_eligibility.Base_patch_busy_with_rebase _) ->
              true (* by definition: a Rebase is in flight or queued *)
          | Start_eligibility.Defer (Start_eligibility.Base_not_fresh_for_cut _)
            -> (
              (* There must be an actual path to Allow: the base patch either has
                 a [Rebase] queued (which will close the freshness gap) or has a
                 PR the reconciler's stale-base detectors can enqueue a [Rebase]
                 against on the next tick. A base patch that exists but has
                 neither — e.g. needs_intervention with an empty queue and no PR
                 — is genuinely stuck, so a bare existence check would pass
                 vacuously. *)
              let bpid_opt =
                Map.to_alist (Orchestrator.agents_map m.orch)
                |> List.find_map ~f:(fun (pid, ag) ->
                    if Branch.equal ag.Patch_agent.branch base then Some pid
                    else None)
              in
              match bpid_opt with
              | None -> false
              | Some bpid ->
                  let base_ag = Orchestrator.agent m.orch bpid in
                  List.mem base_ag.Patch_agent.queue Operation_kind.Rebase
                    ~equal:Operation_kind.equal
                  || Patch_agent.has_pr base_ag)))

(** SBI-3 (rebase drains the queue): the freshness gate's
    [Base_patch_busy_with_rebase] arm only stays sound if a completed rebase
    actually removes [Rebase] from the agent's queue — otherwise
    [runnable_rebase] would latch [true] forever and a just-rebased base would
    keep deferring. Assert the postcondition directly: any idle (not [busy])
    agent that is structurally fresh (its local branch sits on its current
    structural base) must carry no queued [Rebase]. [Patch_agent.complete] after
    a successful rebase enforces this; this locks it in so a regression in
    [fire]/[apply_rebase_result]/[complete] would be caught here rather than
    hidden behind a vacuous liveness pass. *)
let sbi_completed_rebase_drains_queue m =
  List.for_all
    (Map.to_alist (Orchestrator.agents_map m.orch))
    ~f:(fun (pid, a) ->
      let structurally_fresh =
        match a.Patch_agent.branch_rebased_onto with
        | Some b -> Branch.equal b (initial_base_of m.orch m.patches pid)
        | None -> false
      in
      if (not a.Patch_agent.busy) && structurally_fresh then
        not
          (List.mem a.Patch_agent.queue Operation_kind.Rebase
             ~equal:Operation_kind.equal)
      else true)

(* -- Generators -- *)

module Gen = QCheck2.Gen

let gen_command ~n_patches =
  let open Gen in
  let idx = int_range 0 (n_patches - 1) in
  oneof
    [
      return Main_advanced;
      map (fun i -> Pr_merged i) idx;
      map (fun i -> Rebase_complete i) idx;
      map (fun i -> Enqueue_start i) idx;
    ]

let gen_command_seq ~n_patches =
  Gen.list_size (Gen.int_range 1 30) (gen_command ~n_patches)

(* -- Properties -- *)

let prop_sbi_holds =
  QCheck2.Test.make ~count:300
    ~name:"SBI: every Start in runnable_messages is freshness-eligible (Allow)"
    Gen.(
      let* n_patches = int_range 2 4 in
      let* cmds = gen_command_seq ~n_patches in
      return (n_patches, cmds))
    (fun (n_patches, cmds) ->
      let m = bootstrap (mk_patches n_patches) in
      let _final, ok =
        List.fold cmds ~init:(m, true) ~f:(fun (m, ok) cmd ->
            let m = apply_command m cmd in
            (m, ok && sbi_runnable_starts_are_fresh m))
      in
      ok)

let prop_defer_implies_progress =
  QCheck2.Test.make ~count:300
    ~name:
      "SBI-2 (liveness): every Defer corresponds to a state that has a path to \
       Allow"
    Gen.(
      let* n_patches = int_range 2 4 in
      let* cmds = gen_command_seq ~n_patches in
      return (n_patches, cmds))
    (fun (n_patches, cmds) ->
      let m = bootstrap (mk_patches n_patches) in
      let _final, ok =
        List.fold cmds ~init:(m, true) ~f:(fun (m, ok) cmd ->
            let m = apply_command m cmd in
            (m, ok && sbi_defer_implies_progress m))
      in
      ok)

let prop_completed_rebase_drains_queue =
  QCheck2.Test.make ~count:300
    ~name:
      "SBI-3: a completed rebase drains [Rebase] from the queue (no stale \
       busy-rebasing latch)"
    Gen.(
      let* n_patches = int_range 2 4 in
      let* cmds = gen_command_seq ~n_patches in
      return (n_patches, cmds))
    (fun (n_patches, cmds) ->
      let m = bootstrap (mk_patches n_patches) in
      let _final, ok =
        List.fold cmds ~init:(m, true) ~f:(fun (m, ok) cmd ->
            let m = apply_command m cmd in
            (m, ok && sbi_completed_rebase_drains_queue m))
      in
      ok)

(** PI-SBI-1 (witness): the exact event-stream-pages scenario, now driven by a
    dependency merge rather than a main advance.

    1. Bootstrap [a; b; c] with c → b → a. 2. a merges ([Pr_merged 0]);
    [mark_merged] eagerly enqueues a Rebase on b and b's structural base becomes
    main. 3. b has NOT rebased yet. 4. Start(c, base=b) must be deferred. 5.
    After b rebases, Start(c, base=b) returns to Allow. *)
let prop_event_stream_pages_witness =
  QCheck2.Test.make ~count:1
    ~name:
      "PI-SBI-1: event-stream-pages witness — Start(c, base=b) deferred while \
       b unrebased after a merges" (Gen.return ()) (fun () ->
      let patches = mk_patches 3 in
      let m = bootstrap patches in
      let pid_b = pid_of_idx patches 1 in
      let branch_b = (Orchestrator.agent m.orch pid_b).Patch_agent.branch in
      (* Initially: bootstrap fired Start(c, base=b) with b's
         branch_rebased_onto = its structural base. Eligibility = Allow. *)
      let allow0 = Orchestrator.start_eligibility m.orch branch_b in
      let initial_allow =
        match allow0 with
        | Start_eligibility.Allow -> true
        | Start_eligibility.Defer _ -> false
      in
      (* a merges. mark_merged eagerly enqueues Rebase on b and b's structural
         base flips to main, but b's branch_rebased_onto still points at a's
         branch. *)
      let m = apply_command m (Pr_merged 0) in
      let eligibility_after_merge =
        Orchestrator.start_eligibility m.orch branch_b
      in
      (* The gate must defer Start(c, base=b): either [Base_not_fresh_for_cut]
         (branch still on a's branch, not main) or [Base_patch_busy_with_rebase]
         (mark_merged's eager enqueue already put Rebase highest in b's queue).
         Both are correct — the assertion is that some Defer fires. *)
      let deferred =
        match eligibility_after_merge with
        | Start_eligibility.Defer
            ( Start_eligibility.Base_not_fresh_for_cut _
            | Start_eligibility.Base_patch_busy_with_rebase _ ) ->
            true
        | Start_eligibility.Allow -> false
      in
      (* And b's queue contains Rebase, from the eager enqueue. *)
      let b_rebase_queued =
        let a = Orchestrator.agent m.orch pid_b in
        List.mem a.Patch_agent.queue Operation_kind.Rebase
          ~equal:Operation_kind.equal
      in
      (* Now simulate b rebasing onto its structural base (main): eligibility
         returns to Allow. *)
      let m = apply_command m (Rebase_complete 1) in
      let eligibility_after_rebase =
        Orchestrator.start_eligibility m.orch branch_b
      in
      let unblocked =
        match eligibility_after_rebase with
        | Start_eligibility.Allow -> true
        | Start_eligibility.Defer _ -> false
      in
      initial_allow && deferred && b_rebase_queued && unblocked)

(** PI-SBI-2 (witness): an unrelated advance of main never defers a Start. A
    base sitting directly on main (the root patch) stays eligible for cutting
    its downstream no matter how many times main advances — the regression the
    old SHA-equality gate would have flagged. *)
let prop_unrelated_main_advance_does_not_defer =
  QCheck2.Test.make ~count:1
    ~name:
      "PI-SBI-2: unrelated main advance does not defer a Start off a \
       main-based base" (Gen.return ()) (fun () ->
      let patches = mk_patches 2 in
      let m = bootstrap patches in
      let pid_root = pid_of_idx patches 0 in
      let branch_root =
        (Orchestrator.agent m.orch pid_root).Patch_agent.branch
      in
      let allow_before =
        match Orchestrator.start_eligibility m.orch branch_root with
        | Start_eligibility.Allow -> true
        | Start_eligibility.Defer _ -> false
      in
      (* main advances repeatedly with nothing of the root's merging. *)
      let m =
        List.fold
          [ Main_advanced; Main_advanced; Main_advanced ]
          ~init:m ~f:apply_command
      in
      let allow_after =
        match Orchestrator.start_eligibility m.orch branch_root with
        | Start_eligibility.Allow -> true
        | Start_eligibility.Defer _ -> false
      in
      allow_before && allow_after)

let () =
  let runner = QCheck_base_runner.run_tests_main in
  ignore
    (runner
       [
         prop_sbi_holds;
         prop_defer_implies_progress;
         prop_completed_rebase_drains_queue;
         prop_event_stream_pages_witness;
         prop_unrelated_main_advance_does_not_defer;
       ])
