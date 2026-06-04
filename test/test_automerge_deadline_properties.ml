open Base
open Onton
open Onton_core
open Onton_core.Types

(** Property tests for the automerge idle-window ([automerge_deadline]).

    [merge_ready] is now component-derived ([Pr_state.merge_ready_of]: mergeable
    + CI passing + non-blocking review), independent of GitHub's
      [mergeStateStatus]. Two distinct concerns are pinned down here:

    - The sibling-merge flap (the original reason this suite exists): when the
      base branch advances (e.g. a sibling patch merges) GitHub re-computes
      mergeability for every other open PR, dropping [merge_ready] to false for
      a poll or two (mergeability reads [UNKNOWN]) before it settles. The old
      [reconcile_automerge] treated that transient drop as a loss of approval,
      *cleared* the automerge deadline, and re-armed a fresh window each time —
      so with sibling merges recurring faster than the idle timeout, the
      countdown reset perpetually and automerge never fired. The fix holds the
      existing deadline through a transient [UNKNOWN]
      ([automerge_transient_hold]) instead of resetting it.

    - The stale-[BLOCKED] regression (PR #4026, AM-STALE-BLOCK): now that
      [merge_ready] no longer keys off [mergeStateStatus], a patch that is ready
      by the component facts but still carries a stale GitHub [BLOCKED] must arm
      and fire exactly like a CLEAN one.

    These properties cover the per-tick transition table, the single-patch flap
    sequence, and a multi-patch interleaving where each merge induces an
    [UNKNOWN] blip on its siblings. *)

let main = Branch.of_string "main"
let other_branch = Branch.of_string "release"
let timeout = Patch_controller.automerge_idle_timeout
let max_failures = Patch_controller.automerge_max_failures

(* -- Helpers -- *)

let make_patch pid branch =
  Patch.
    {
      id = pid;
      title = "Test patch";
      description = "";
      branch;
      dependencies = [];
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

(* Build an agent with a PR present, parameterized over exactly the fields
   [reconcile_automerge] reads. Routed through [Patch_agent.restore] so the
   record stays exhaustive as fields are added. *)
let make_agent ~patch_id ~branch ~merge_ready ~mergeability_unknown
    ~checks_passing ~queue ~is_draft ~busy ~merged ~branch_blocked ~base_branch
    ~merge_queue_required ~merge_queue_entry ~automerge_enabled
    ~automerge_deadline ~automerge_inflight ~automerge_failure_count =
  Patch_agent.restore ~patch_id ~branch
    ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
    ~has_session:busy ~busy ~merged ~queue ~satisfies:false ~changed:false
    ~has_conflict:false ~base_branch ~notified_base_branch:base_branch
    ~ci_failure_count:0 ~session_fallback:Patch_agent.Fresh_available
    ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[] ~merge_ready
    ~mergeability_unknown ~merge_queue_required ~merge_queue_entry ~is_draft
    ~pr_body_delivered:true ~pr_body_artifact_miss_count:0
    ~start_attempts_without_pr:0 ~conflict_noop_count:0 ~no_commits_push_count:0
    ~context_exhaustion_count:0 ~push_failure_count:0 ~branch_rebased_onto:None
    ~branch_rebased_onto_sha:None ~merge_commit_sha:None
    ~base_contains_merged_siblings:true
    ~anchor_history:Onton_core.Anchor_history.empty ~checks_passing
    ~current_op:None
    ~current_op_state:(if busy then Patch_agent.Running else Patch_agent.Queued)
    ~current_message_id:None ~generation:0 ~worktree_path:None ~branch_blocked
    ~llm_session_id:None ~automerge_enabled ~automerge_deadline
    ~automerge_inflight ~automerge_failure_count ~delivered_ci_run_ids:[]

let make_orch agents_alist =
  let patches =
    List.map agents_alist ~f:(fun (pid, agent) ->
        make_patch pid agent.Patch_agent.branch)
  in
  let graph = Graph.of_patches patches in
  let agents = Map.of_alist_exn (module Patch_id) agents_alist in
  Orchestrator.restore ~graph ~agents
    ~outbox:(Map.empty (module Message_id))
    ~main_branch:main ()

let deadline_of orch pid =
  (Orchestrator.agent orch pid).Patch_agent.automerge_deadline

let fired_for decisions pid =
  List.exists decisions ~f:(fun (d : Patch_controller.automerge_decision) ->
      Patch_id.equal d.merge_patch_id pid)

(* -- Generators -- *)

(* Realistic [(merge_ready, mergeability_unknown)] pairs for the direct-merge
   generator. Both are now component-derived (see [Pr_state.merge_ready_of] /
   [merge_state = Unknown]), independent of GitHub's [mergeStateStatus]. The
   three reachable combinations:

   - [(true, false)]  — ready: mergeable, CI passing, review non-blocking. A
                        stale GitHub [BLOCKED] would land here too (PR #4026):
                        readiness no longer reads that field, so it stays ready.
   - [(false, true)]  — GitHub is recomputing mergeability ([Unknown]); not
                        ready, and the transient-hold window applies.
   - [(false, false)] — a known not-ready state (conflict, failing CI, blocking
                        review, or a stale block that leaves merge_ready false).

   [(true, true)] is unreachable: ready requires [Mergeable], which contradicts
   [Unknown]. *)
let gen_ready_unknown =
  QCheck2.Gen.oneof_list [ (true, false); (false, true); (false, false) ]

(* The flap/multi runs below toggle only between "mergeable" and "recomputing"
   (a sibling-merge mergeability recompute). [is_clean] true => ready
   ([(true, false)]); false => recomputing ([(false, true)]). *)
let ready_unknown_of_clean is_clean = (is_clean, not is_clean)

let gen_deadline ~now =
  (* A mix of: no deadline, an already-elapsed one, and a still-future one,
     anchored around [now] so [now >= deadline] is exercised both ways. *)
  QCheck2.Gen.oneof_list
    [
      None; Some (now -. 1.0); Some now; Some (now +. 1.0); Some (now +. timeout);
    ]

(* An agent on the direct-merge path (no merge queue), with the automerge-
   relevant dimensions varied independently. This is where the fix lives and
   where the bug occurred; merge-queue paths are covered by
   [test_patch_controller]. *)
let gen_direct_agent ~now =
  QCheck2.Gen.(
    let* merge_ready, mergeability_unknown = gen_ready_unknown in
    let* checks_passing = bool in
    let* has_feedback = bool in
    let* is_draft = bool in
    let* busy = bool in
    let* branch_blocked = bool in
    let* base_is_main = bool in
    let* automerge_enabled = bool in
    let* automerge_inflight = bool in
    let* automerge_failure_count = int_range 0 (max_failures + 1) in
    let* automerge_deadline = gen_deadline ~now in
    let pid = Patch_id.of_string "p" in
    return
      (make_agent ~patch_id:pid
         ~branch:(Branch.of_string "feat/p")
         ~merge_ready ~mergeability_unknown ~checks_passing
         ~queue:
           (if has_feedback then [ Operation_kind.Review_comments ] else [])
         ~is_draft ~busy ~merged:false ~branch_blocked
         ~base_branch:(Some (if base_is_main then main else other_branch))
         ~merge_queue_required:false ~merge_queue_entry:None ~automerge_enabled
         ~automerge_deadline ~automerge_inflight ~automerge_failure_count))

let pid = Patch_id.of_string "p"

(* -- Per-tick transition properties -- *)

let () =
  let open QCheck2 in
  (* AM-SAFE: a merge decision is only ever issued for a patch whose
     [merge_ready] is true. [merge_ready] is the component-derived predicate, so
     this is independent of GitHub's [mergeStateStatus] — a stale BLOCKED on a
     ready PR may fire, an UNKNOWN/DIRTY (which forces not-ready) never does. *)
  let prop_never_fire_unless_clean =
    Test.make ~name:"automerge AM-SAFE: never fire unless merge_ready"
      ~count:2000
      Gen.(
        let* now = float_range 0.0 1_000_000.0 in
        let* agent = gen_direct_agent ~now in
        return (now, agent))
      (fun (now, agent) ->
        let orch = make_orch [ (pid, agent) ] in
        let orch', decisions = Patch_controller.reconcile_automerge orch ~now in
        if fired_for decisions pid then
          (Orchestrator.agent orch' pid).Patch_agent.merge_ready
        else true)
  in

  (* AM-HOLD: when [automerge_transient_hold] holds and a deadline is armed,
     reconcile preserves the deadline byte-for-byte and issues no decision. This
     is the core of the fix — a sibling-merge-induced UNKNOWN does not reset the
     counter. *)
  let prop_transient_hold_preserves_deadline =
    Test.make
      ~name:"automerge AM-HOLD: transient UNKNOWN preserves armed deadline"
      ~count:2000
      Gen.(
        let* now = float_range 0.0 1_000_000.0 in
        let* agent = gen_direct_agent ~now in
        return (now, agent))
      (fun (now, agent) ->
        assume
          (Patch_controller.automerge_transient_hold agent ~main_branch:main
          && Option.is_some agent.Patch_agent.automerge_deadline);
        let orch = make_orch [ (pid, agent) ] in
        let orch', decisions = Patch_controller.reconcile_automerge orch ~now in
        Option.equal Float.equal (deadline_of orch' pid)
          agent.Patch_agent.automerge_deadline
        && not (fired_for decisions pid))
  in

  (* AM-NOARM: a not-[merge_ready] patch never *creates* a deadline. The idle
     window is only armed from a ready candidate; a not-ready blip with no
     deadline stays at None (we never start a timer on a patch we can't act on).
     Note this keys on [merge_ready], not the status: a ready patch carrying a
     stale BLOCKED *does* arm (that is AM-ARM / AM-STALE-BLOCK). *)
  let prop_no_arm_while_not_clean =
    Test.make ~name:"automerge AM-NOARM: never arm a deadline while not ready"
      ~count:2000
      Gen.(
        let* now = float_range 0.0 1_000_000.0 in
        let* agent = gen_direct_agent ~now in
        return (now, agent))
      (fun (now, agent) ->
        assume
          ((not agent.Patch_agent.merge_ready)
          && Option.is_none agent.Patch_agent.automerge_deadline
          && not agent.Patch_agent.merged);
        let orch = make_orch [ (pid, agent) ] in
        let orch', _ = Patch_controller.reconcile_automerge orch ~now in
        Option.is_none (deadline_of orch' pid))
  in

  (* AM-CLEAR: a real loss of candidacy that is NOT a transient hold still
     clears an armed deadline (BLOCKED/DIRTY/BEHIND, failing checks, queued
     feedback, draft, automerge disabled, failure cap, lost base, …). Guards the
     hold against being over-broad. *)
  let prop_real_loss_clears_deadline =
    Test.make
      ~name:"automerge AM-CLEAR: real loss of candidacy clears armed deadline"
      ~count:2000
      Gen.(
        let* now = float_range 0.0 1_000_000.0 in
        let* agent = gen_direct_agent ~now in
        return (now, agent))
      (fun (now, agent) ->
        assume
          ((not
              (Patch_controller.is_automerge_candidate agent ~main_branch:main))
          && (not
                (Patch_controller.automerge_transient_hold agent
                   ~main_branch:main))
          && (not agent.Patch_agent.automerge_inflight)
          && (not agent.Patch_agent.merged)
          && Option.is_some agent.Patch_agent.automerge_deadline);
        let orch = make_orch [ (pid, agent) ] in
        let orch', decisions = Patch_controller.reconcile_automerge orch ~now in
        Option.is_none (deadline_of orch' pid) && not (fired_for decisions pid))
  in

  (* AM-ARM: a ready candidate with no deadline arms exactly one idle window at
     [now + timeout]; with an elapsed deadline it fires Direct_merge. Candidacy
     is component-derived, so this now also covers a ready patch whose
     mergeStateStatus is a stale BLOCKED/BEHIND. *)
  let prop_clean_candidate_arms_or_fires =
    Test.make
      ~name:"automerge AM-ARM: ready candidate arms at now+timeout / fires"
      ~count:2000
      Gen.(
        let* now = float_range 0.0 1_000_000.0 in
        let* agent = gen_direct_agent ~now in
        return (now, agent))
      (fun (now, agent) ->
        assume (Patch_controller.is_automerge_candidate agent ~main_branch:main);
        let orch = make_orch [ (pid, agent) ] in
        let orch', decisions = Patch_controller.reconcile_automerge orch ~now in
        match agent.Patch_agent.automerge_deadline with
        | None ->
            (* armed at now + timeout, no decision *)
            Option.equal Float.equal (deadline_of orch' pid)
              (Some (now +. timeout))
            && not (fired_for decisions pid)
        | Some d ->
            if Float.( >= ) now d then
              (* elapsed → a Direct_merge decision and inflight set *)
              fired_for decisions pid
              && (Orchestrator.agent orch' pid).Patch_agent.automerge_inflight
            else
              (* not yet elapsed → unchanged, no decision *)
              Option.equal Float.equal (deadline_of orch' pid) (Some d)
              && not (fired_for decisions pid))
  in

  (* AM-STALE-BLOCK: the PR #4026 regression, stated directly. A patch that is
     ready by the component facts ([merge_ready = true], mergeability known)
     must arm the window and fire once it elapses — regardless of whatever stale
     verdict GitHub's [mergeStateStatus] rollup might have carried (it is no
     longer on the agent and no longer read). Before the fix [merge_ready] was
     [mergeStateStatus = CLEAN], so a stale BLOCKED forced [merge_ready = false]
     and the patch sat in "awaiting feedback" forever. *)
  let prop_stale_block_still_fires =
    Test.make
      ~name:"automerge AM-STALE-BLOCK: ready + stale BLOCKED arms and fires"
      ~count:1000
      Gen.(float_range 0.0 1_000_000.0)
      (fun now ->
        let agent =
          make_agent ~patch_id:pid
            ~branch:(Branch.of_string "feat/p")
            ~merge_ready:true ~mergeability_unknown:false ~checks_passing:true
            ~queue:[] ~is_draft:false ~busy:false ~merged:false
            ~branch_blocked:false ~base_branch:(Some main)
            ~merge_queue_required:false ~merge_queue_entry:None
            ~automerge_enabled:true
            ~automerge_deadline:(Some (now -. 1.0))
            ~automerge_inflight:false ~automerge_failure_count:0
        in
        let orch = make_orch [ (pid, agent) ] in
        let orch', decisions = Patch_controller.reconcile_automerge orch ~now in
        fired_for decisions pid
        && (Orchestrator.agent orch' pid).Patch_agent.automerge_inflight)
  in

  (* -- Single-patch flap interleaving -- *)

  (* A sequence of polls that keep the patch fully approval-ready while
     [mergeStateStatus] flaps between CLEAN and UNKNOWN (each UNKNOWN modelling a
     sibling merge advancing the base), with wall-clock advancing arbitrarily.
     The first tick is CLEAN so the window arms at a known value [t0 + timeout].

     Invariants:
     - MONO: every armed deadline value observed across the run is identical —
       the counter is armed once and never reset by a flap. (This is exactly
       what regressed.)
     - SAFE: a merge fires only on a CLEAN tick.
     - LIVE: if some CLEAN tick occurs at or after the armed deadline, the patch
       does fire — a continuously-approval-ready patch eventually merges despite
       UNKNOWN flaps. *)
  let gen_flap_run =
    Gen.(
      let* t0 = float_range 0.0 1000.0 in
      (* ticks after the initial CLEAN arm: (is_clean, dt) *)
      let* rest =
        list_size (int_range 0 14)
          (pair bool (map Float.of_int (int_range 0 150)))
      in
      return (t0, rest))
  in
  let run_flap (t0, rest) =
    let agent_at ~is_clean ~deadline =
      let merge_ready, mergeability_unknown = ready_unknown_of_clean is_clean in
      make_agent ~patch_id:pid
        ~branch:(Branch.of_string "feat/p")
        ~merge_ready ~mergeability_unknown ~checks_passing:true ~queue:[]
        ~is_draft:false ~busy:false ~merged:false ~branch_blocked:false
        ~base_branch:(Some main) ~merge_queue_required:false
        ~merge_queue_entry:None ~automerge_enabled:true
        ~automerge_deadline:deadline ~automerge_inflight:false
        ~automerge_failure_count:0
    in
    (* Drive the ticks, threading the current deadline. Returns the list of
       (now, is_clean, deadline_after, fired?). *)
    let ticks =
      (true, t0) :: List.map rest ~f:(fun (is_clean, dt) -> (is_clean, dt))
    in
    let _, _, trace =
      List.fold ticks ~init:(t0, None, [])
        ~f:(fun (now, deadline, acc) (is_clean, dt) ->
          let now = Float.( + ) now dt in
          let orch = make_orch [ (pid, agent_at ~is_clean ~deadline) ] in
          let orch', decisions =
            Patch_controller.reconcile_automerge orch ~now
          in
          let deadline' = deadline_of orch' pid in
          ( now,
            deadline',
            (now, is_clean, deadline', fired_for decisions pid) :: acc ))
    in
    List.rev trace
  in
  let prop_flap_mono =
    Test.make
      ~name:"automerge FLAP-MONO: armed deadline never resets across flaps"
      ~count:1500 gen_flap_run (fun run ->
        let trace = run_flap run in
        let armed =
          List.filter_map trace ~f:(fun (_, _, d, _) -> d)
          |> List.dedup_and_sort ~compare:Float.compare
        in
        List.length armed <= 1)
  in
  let prop_flap_safe =
    Test.make ~name:"automerge FLAP-SAFE: fires only on CLEAN ticks" ~count:1500
      gen_flap_run (fun run ->
        let trace = run_flap run in
        List.for_all trace ~f:(fun (_, is_clean, _, fired) ->
            (not fired) || is_clean))
  in
  let prop_flap_live =
    Test.make
      ~name:"automerge FLAP-LIVE: a CLEAN tick past the deadline does fire"
      ~count:1500 gen_flap_run (fun run ->
        let trace = run_flap run in
        (* The window arms at t0 + timeout on the first tick. *)
        match List.filter_map trace ~f:(fun (_, _, d, _) -> d) with
        | [] -> true
        | armed :: _ ->
            let reaches_deadline_clean =
              List.exists trace ~f:(fun (now, is_clean, _, _) ->
                  is_clean && Float.( >= ) now armed)
            in
            let fired_somewhere =
              List.exists trace ~f:(fun (_, _, _, fired) -> fired)
            in
            (not reaches_deadline_clean) || fired_somewhere)
  in

  (* -- Multi-patch interleaving -- *)

  (* The scenario verbatim: several approval-ready direct-merge patches share a
     base. An interleaving of (time advancing) and (one patch merging) steps;
     each merge flips every *other* still-open patch to UNKNOWN for that tick
     (GitHub recomputing mergeability on the advanced base). Invariant: no
     surviving patch's deadline is ever reset by a sibling merge — across the
     whole run each patch exhibits at most one armed deadline value. *)
  let gen_multi_run =
    Gen.(
      let* k = int_range 2 4 in
      let* t0 = float_range 0.0 1000.0 in
      (* each step: (merge_some?, which index, dt) *)
      let* steps =
        list_size (int_range 1 16)
          (triple bool
             (int_range 0 (k - 1))
             (map Float.of_int (int_range 1 90)))
      in
      return (k, t0, steps))
  in
  let pids k =
    List.init k ~f:(fun i -> Patch_id.of_string (Printf.sprintf "p%d" i))
  in
  let run_multi (k, t0, steps) =
    let ps = pids k in
    let agent_at p ~is_clean ~deadline =
      let merge_ready, mergeability_unknown = ready_unknown_of_clean is_clean in
      make_agent ~patch_id:p
        ~branch:(Branch.of_string (Patch_id.to_string p))
        ~merge_ready ~mergeability_unknown ~checks_passing:true ~queue:[]
        ~is_draft:false ~busy:false ~merged:false ~branch_blocked:false
        ~base_branch:(Some main) ~merge_queue_required:false
        ~merge_queue_entry:None ~automerge_enabled:true
        ~automerge_deadline:deadline ~automerge_inflight:false
        ~automerge_failure_count:0
    in
    (* Build an initial orchestrator: all mergeable, no deadline. One reconcile
       to arm them all. *)
    let orch0 =
      make_orch
        (List.map ps ~f:(fun p -> (p, agent_at p ~is_clean:true ~deadline:None)))
    in
    let orch0, _ = Patch_controller.reconcile_automerge orch0 ~now:t0 in
    (* Per-patch record of every Some-deadline observed. *)
    let observed = Hashtbl.create (module Patch_id) in
    let record orch =
      List.iter ps ~f:(fun p ->
          let a = Orchestrator.agent orch p in
          if not a.Patch_agent.merged then
            match a.Patch_agent.automerge_deadline with
            | Some d ->
                Hashtbl.update observed p ~f:(function
                  | None -> [ d ]
                  | Some ds -> d :: ds)
            | None -> ())
    in
    record orch0;
    let _ =
      List.fold steps ~init:(orch0, t0)
        ~f:(fun (orch, now) (do_merge, idx, dt) ->
          let now = Float.( + ) now dt in
          let merging =
            if do_merge then
              List.nth ps idx
              |> Option.filter ~f:(fun p ->
                  not (Orchestrator.agent orch p).Patch_agent.merged)
            else None
          in
          (* Re-stamp every still-open patch's mergeability for this tick: the
             merging one stays mergeable; its siblings go Unknown (GitHub
             recomputing on the advanced base). *)
          let orch =
            List.fold ps ~init:orch ~f:(fun orch p ->
                let a = Orchestrator.agent orch p in
                if a.Patch_agent.merged then orch
                else
                  let is_clean =
                    match merging with
                    | Some m when not (Patch_id.equal m p) -> false
                    | _ -> true
                  in
                  let merge_ready, mergeability_unknown =
                    ready_unknown_of_clean is_clean
                  in
                  let orch = Orchestrator.set_merge_ready orch p merge_ready in
                  Orchestrator.set_mergeability_unknown orch p
                    mergeability_unknown)
          in
          let orch, _ = Patch_controller.reconcile_automerge orch ~now in
          (* Force the chosen patch to actually merge (the user's premise:
             patches do merge; it is the *others'* counters that regress). *)
          let orch =
            match merging with
            | Some m -> Patch_controller.apply_automerge_success orch m
            | None -> orch
          in
          record orch;
          (orch, now))
    in
    observed
  in
  let prop_multi_no_reset =
    Test.make
      ~name:
        "automerge MULTI: a sibling merge never resets another patch's deadline"
      ~count:1000 gen_multi_run (fun run ->
        let observed = run_multi run in
        Hashtbl.for_all observed ~f:(fun ds ->
            List.length (List.dedup_and_sort ds ~compare:Float.compare) <= 1))
  in

  let suite =
    [
      prop_never_fire_unless_clean;
      prop_transient_hold_preserves_deadline;
      prop_no_arm_while_not_clean;
      prop_real_loss_clears_deadline;
      prop_clean_candidate_arms_or_fires;
      prop_stale_block_still_fires;
      prop_flap_mono;
      prop_flap_safe;
      prop_flap_live;
      prop_multi_no_reset;
    ]
  in
  List.iter suite ~f:(fun t -> QCheck2.Test.check_exn t)
