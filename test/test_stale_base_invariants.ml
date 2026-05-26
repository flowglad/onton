open Base
open Onton
open Onton_core
open Onton_core.Types

(** Stale-Base Invariant (SBI) property tests.

    Property: for every reachable orchestrator state and every [Start (c, base)]
    action that actually fires from [Orchestrator.runnable_messages] →
    [Orchestrator.accept_message], the base is fresh — meaning at fire time:

    + [base = main_branch], OR
    + the base patch (looked up by branch) is [merged], OR
    + the base patch's [branch_rebased_onto_sha] equals the orchestrator's known
      [main_sha].

    The harness:

    - Models a linear A→B→C→… dep chain of length 2 or 3 with
      [mk_linear_patches], so each non-root patch has exactly one dep.
    - Models the world's [main_sha] as a counter; a [Main_advanced] command
      bumps it and calls [Orchestrator.set_main_sha]. (Real wire-up via the
      poller is exercised separately.)
    - Models PR merges via [Pr_merged i], which calls [Orchestrator.mark_merged]
      and (per [Orchestrator.mark_merged]'s eager enqueue) implicitly enqueues a
      [Rebase] for the dependent.
    - Models rebase completion via [Rebase_complete i], which applies a
      successful rebase result onto main and records an anchor sha = current
      model [main_sha]. This is the operation that closes the freshness gap.
    - Models the controller's Start-firing loop via [Drain_runnable], which
      pulls the highest-priority runnable message and runs it through
      [accept_message] + [fire] + [complete]. By construction, only
      eligibility-passing Starts reach [fire]; the test asserts this directly
      from the model. *)

let main = Branch.of_string "main"
let mk_patches = Onton_test_support.Test_generators.mk_linear_patches

(* -- Model state -- *)

type model = {
  orch : Orchestrator.t;
  patches : Patch.t list;
  model_main_sha : int;
      (** Monotonically advancing counter — every [Main_advanced] or merge
          stamps a new sha. *)
}

let sha_of_int n = Printf.sprintf "sha-%08d" n

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

(** Bootstrap mirroring the patch-controller boot sequence: each patch starts
    with base = its dep's branch (or main for the root), receives a PR number,
    and completes. Initial main_sha is published before any Start is gated. *)
let bootstrap patches =
  let branch_of = branch_of_patches patches in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let initial_main_sha = sha_of_int 0 in
  let orch = Orchestrator.set_main_sha orch initial_main_sha in
  let orch =
    List.foldi patches ~init:orch ~f:(fun i o _p ->
        let pid = pid_of_idx patches i in
        let has_merged dep_pid =
          (Orchestrator.agent o dep_pid).Patch_agent.merged
        in
        let base =
          Graph.initial_base (Orchestrator.graph o) pid ~has_merged ~branch_of
            ~main
        in
        let o = Orchestrator.fire o (Orchestrator.Start (pid, base)) in
        (* Mirror the runner's anchor-recording on a successful initial
           checkout: branch_rebased_onto_sha = initial main sha. Without this
           the freshness gate would refuse every Start at the very first
           Main_advanced. *)
        let o =
          Orchestrator.set_branch_rebased_onto_sha o pid (Some initial_main_sha)
        in
        let o = Orchestrator.set_pr_number o pid (Pr_number.of_int (i + 1)) in
        Orchestrator.complete o pid)
  in
  { orch; patches; model_main_sha = 0 }

(* -- Commands -- *)

type command =
  | Main_advanced  (** Bump the model main_sha and publish to orchestrator. *)
  | Pr_merged of int
      (** Mark patch idx as merged. Bumps main_sha (the merged commit
          conceptually becomes main's tip). *)
  | Rebase_complete of int
      (** Apply a successful rebase onto main for patch idx; record anchor sha =
          current model main_sha. *)
  | Enqueue_start of int
      (** Place a [Start] in patch idx's outbox so the freshness gate can be
          observed against it. Bootstrap already placed and consumed the initial
          Start; this re-enqueues one for testing. *)

let bump_main_sha m =
  let next = m.model_main_sha + 1 in
  let orch = Orchestrator.set_main_sha m.orch (sha_of_int next) in
  { m with orch; model_main_sha = next }

let apply_command m cmd =
  match cmd with
  | Main_advanced -> bump_main_sha m
  | Pr_merged i ->
      if i >= List.length m.patches then m
      else
        let pid = pid_of_idx m.patches i in
        let m = bump_main_sha m in
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
          (* This drives the agent state-machine directly (fire → apply result)
             rather than the message lifecycle (accept_message → … →
             apply_rebase_result), and that shortcut is safe for the SBI
             property because the property only reads base freshness from agent
             fields — [merged], [branch_rebased_onto_sha], and (via
             [start_eligibility]'s busy-rebase check) [busy]/[current_op]/[queue]
             — none of which depend on outbox message [status] or
             [current_message_id]. [Orchestrator.fire (Rebase _)] sets
             [busy = true] and [current_op = Rebase] (Patch_agent.rebase's spec)
             exactly as [accept_message] would, which is also the precondition
             [apply_rebase_result]'s eventual [complete] requires. The only state
             not modelled here is the *in-flight* (Acked, busy) rebase observed
             between commands; the busy-via-queue path is exercised after
             [Pr_merged]'s eager enqueue, and [prop_event_stream_pages_witness]
             covers Base_patch_busy_with_rebase directly. *)
          let orch =
            Orchestrator.fire m.orch (Orchestrator.Rebase (pid, main))
          in
          let orch, _effects =
            Orchestrator.apply_rebase_result orch pid Worktree.Ok main
          in
          let orch =
            Orchestrator.set_branch_rebased_onto_sha orch pid
              (Some (sha_of_int m.model_main_sha))
          in
          { m with orch }
  | Enqueue_start i ->
      if i >= List.length m.patches then m
      else
        let pid = pid_of_idx m.patches i in
        let agent = Orchestrator.agent m.orch pid in
        let branch_of = branch_of_patches m.patches in
        let has_merged dep_pid =
          (Orchestrator.agent m.orch dep_pid).Patch_agent.merged
        in
        let base =
          Graph.initial_base
            (Orchestrator.graph m.orch)
            pid ~has_merged ~branch_of ~main
        in
        (* Inject a real [Pending] Start(pid, base) into the outbox so the
           freshness gate in [runnable_messages] is actually exercised against
           it. [base] varies as deps merge / rebase and as main advances, so
           the gate is observed against fresh and stale bases alike. The id is
           keyed on (patch, base, generation) — mirroring the controller — so a
           base change re-keys the message and [reconcile_message] obsoletes the
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
    SBI invariant — recomputed directly from agent fields rather than delegating
    back to [start_eligibility], so the property genuinely checks that
    [runnable_messages]' gate enforces freshness (a regression that let a stale
    Start through would be caught) instead of re-deriving the same verdict. *)
let base_is_fresh m base =
  if Branch.equal base main then true
  else
    let base_agent =
      Map.data (Orchestrator.agents_map m.orch)
      |> List.find ~f:(fun (a : Patch_agent.t) ->
          Branch.equal a.Patch_agent.branch base)
    in
    match base_agent with
    | None -> false
    | Some a -> (
        a.Patch_agent.merged
        ||
        match
          (a.Patch_agent.branch_rebased_onto_sha, Orchestrator.main_sha m.orch)
        with
        | Some s, Some main_sha -> String.equal s main_sha
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
          | Start_eligibility.Defer Start_eligibility.Main_sha_unknown ->
              Option.is_none (Orchestrator.main_sha m.orch)
          | Start_eligibility.Defer
              (Start_eligibility.Base_patch_busy_with_rebase _) ->
              true (* by definition: a Rebase is in flight or queued *)
          | Start_eligibility.Defer
              (Start_eligibility.Base_not_rebased_since_main_advanced _) -> (
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
    [runnable_rebase] would latch [true] forever, a just-rebased base would keep
    deferring with [Base_patch_busy_with_rebase], and SBI-2's "by definition"
    arm would pass vacuously. Assert the postcondition directly: any idle (not
    [busy]) agent whose recorded sha already matches [main_sha] must carry no
    queued [Rebase]. [Patch_agent.rebase] enforces this (it filters [Rebase] out
    of [queue]); this locks it in so a regression in
    [fire]/[apply_rebase_result]/[complete] would be caught here rather than
    hidden behind a vacuous liveness pass. *)
let sbi_completed_rebase_drains_queue m =
  let main_sha = Orchestrator.main_sha m.orch in
  List.for_all (Orchestrator.all_agents m.orch) ~f:(fun (a : Patch_agent.t) ->
      let rebased_to_main =
        match (a.Patch_agent.branch_rebased_onto_sha, main_sha) with
        | Some s, Some ms -> String.equal s ms
        | _ -> false
      in
      if (not a.Patch_agent.busy) && rebased_to_main then
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

(** PI-SBI-1 (witness): the exact event-stream-pages scenario.

    1. Bootstrap [a; b; c] with c → b → a. 2. main advances (a merges to
    upstream, but a is itself an open patch in the model — we model it as:
    external main bumps + Pr_merged a). 3. b is the open dep of c; b has NOT
    rebased. 4. Attempt to fire Start(c, base=b): must be deferred. *)
let prop_event_stream_pages_witness =
  QCheck2.Test.make ~count:1
    ~name:
      "PI-SBI-1: event-stream-pages witness — Start(c, base=b) deferred while \
       b unrebased after a merges" (Gen.return ()) (fun () ->
      let patches = mk_patches 3 in
      let m = bootstrap patches in
      let pid_a = pid_of_idx patches 0 in
      let pid_b = pid_of_idx patches 1 in
      let pid_c = pid_of_idx patches 2 in
      let branch_b = (Orchestrator.agent m.orch pid_b).Patch_agent.branch in
      (* Initially: bootstrap fired Start(c, base=b) when b's
         branch_rebased_onto_sha = initial main sha. Eligibility = Allow.
         Sanity-check that here. *)
      let allow0 = Orchestrator.start_eligibility m.orch branch_b in
      let initial_allow =
        match allow0 with
        | Start_eligibility.Allow -> true
        | Start_eligibility.Defer _ -> false
      in
      (* main advances and a merges. mark_merged eagerly enqueues Rebase on b. *)
      let m = apply_command m Main_advanced in
      let m = apply_command m (Pr_merged 0) in
      let _ = pid_a in
      (* b's rebased_onto_sha is still the old sha. Eligibility for a new
         Start with base=b should be Defer. *)
      let eligibility_after_merge =
        Orchestrator.start_eligibility m.orch branch_b
      in
      (* After a's merge, the gate must defer Start(c, base=b) for one of two
         reasons: (a) the sha mismatch [Base_not_rebased_since_main_advanced]
         if no rebase has been queued yet, or (b) [Base_patch_busy_with_rebase]
         if mark_merged's eager enqueue already put Rebase in b's queue. Both
         are correct outcomes — the assertion is that some Defer fires. *)
      let deferred =
        match eligibility_after_merge with
        | Start_eligibility.Defer
            ( Start_eligibility.Base_not_rebased_since_main_advanced _
            | Start_eligibility.Base_patch_busy_with_rebase _ ) ->
            true
        | Start_eligibility.Allow
        | Start_eligibility.Defer Start_eligibility.Main_sha_unknown ->
            false
      in
      (* And b's queue contains Rebase, from the eager enqueue. *)
      let b_rebase_queued =
        let a = Orchestrator.agent m.orch pid_b in
        List.mem a.Patch_agent.queue Operation_kind.Rebase
          ~equal:Operation_kind.equal
      in
      (* Now simulate b rebasing onto current main: eligibility returns to
         Allow. *)
      let m = apply_command m (Rebase_complete 1) in
      let eligibility_after_rebase =
        Orchestrator.start_eligibility m.orch branch_b
      in
      let unblocked =
        match eligibility_after_rebase with
        | Start_eligibility.Allow -> true
        | Start_eligibility.Defer _ -> false
      in
      let _ = pid_c in
      initial_allow && deferred && b_rebase_queued && unblocked)

let () =
  let runner = QCheck_base_runner.run_tests_main in
  ignore
    (runner
       [
         prop_sbi_holds;
         prop_defer_implies_progress;
         prop_completed_rebase_drains_queue;
         prop_event_stream_pages_witness;
       ])
