(* @archlint.module test
   @archlint.domain push-plan *)

open Base
open Onton_core

(** Property tests for {!Push_plan}.

    Properties cover:

    - {b PPP-1 Totality}: [plan] never raises on any combination of inputs.
    - {b PPP-2 Branch-switch refused}: when
      [worktree_head_branch <> Some expected_branch], decision is
      [Refuse (Branch_switched _)] (after [Worktree_missing] pre-emption).
    - {b PPP-2b Matching head branch not branch-switched}: when no earlier
      pre-emption fires and [worktree_head_branch = Some expected_branch], the
      decision is not [Refuse (Branch_switched _)].
    - {b PPP-3 Local-missing-remote refused}: when
      [ancestry = Local_missing_remote] and both refs are present, decision is
      [Refuse (Local_missing_remote_commits _)].
    - {b PPP-3b Local-diverged-from-remote permitted}: when
      [ancestry = Local_diverged_from_remote] and both refs are present, the
      decision is [Push Force_push_if_includes] (post-rebase divergence — the
      git-level [--force-if-includes] flag covers the unsafe edge case; refusing
      here would strand the orchestrator on the [conflict_noop_count >= 2]
      needs-intervention threshold).
    - {b PPP-4 Zero commits → skip}: [commits_ahead_of_base = Some 0] yields
      [Refuse No_commits_ahead_of_base] (after worktree-missing, branch-switch,
      branch-ref-missing pre-emptions).
    - {b PPP-5 Initial push when no remote}: [remote_tracking_sha = None] + no
      refusals → [Push Initial_push].
    - {b PPP-6 Happy path}: all preconditions met + remote present →
      [Push Force_push_if_includes].
    - {b PPP-7 Worktree-missing pre-empts}: [worktree_path_exists = false]
      always yields [Refuse Worktree_missing].
    - {b PPP-8 Determinism}: same inputs → same output.
    - {b PPP-9 Variant reachability}: each [decision] constructor is reachable.
    - {b PPP-10 Refusal-to-rejection permanence}: every refusal that maps to
      [Some rej] yields a [rej] for which
      [Push_reject_classify.is_permanent = true].
    - {b PPP-11 Label bounds}: [short_label] non-empty, ≤ 32 chars, snake_case.
*)

module Gen = QCheck2.Gen
module Test = QCheck2.Test
module PP = Push_plan

(* ---------- Helpers ---------- *)

let is_push_force = function
  | PP.Push PP.Force_push_if_includes -> true
  | PP.Push PP.Initial_push | PP.Refuse _ -> false

let is_push_initial = function
  | PP.Push PP.Initial_push -> true
  | PP.Push PP.Force_push_if_includes | PP.Refuse _ -> false

let is_refuse_no_commits = function
  | PP.Refuse PP.No_commits_ahead_of_base -> true
  | PP.Refuse
      ( PP.Worktree_missing | PP.Branch_ref_missing _ | PP.Branch_switched _
      | PP.Local_missing_remote_commits _ )
  | PP.Push _ ->
      false

let is_refuse_wt_missing = function
  | PP.Refuse PP.Worktree_missing -> true
  | PP.Refuse
      ( PP.No_commits_ahead_of_base | PP.Branch_ref_missing _
      | PP.Branch_switched _ | PP.Local_missing_remote_commits _ )
  | PP.Push _ ->
      false

let is_refuse_ref_missing = function
  | PP.Refuse (PP.Branch_ref_missing _) -> true
  | PP.Refuse
      ( PP.No_commits_ahead_of_base | PP.Worktree_missing | PP.Branch_switched _
      | PP.Local_missing_remote_commits _ )
  | PP.Push _ ->
      false

let is_refuse_branch_switched = function
  | PP.Refuse (PP.Branch_switched _) -> true
  | PP.Refuse
      ( PP.No_commits_ahead_of_base | PP.Worktree_missing
      | PP.Branch_ref_missing _ | PP.Local_missing_remote_commits _ )
  | PP.Push _ ->
      false

let is_refuse_local_behind = function
  | PP.Refuse (PP.Local_missing_remote_commits _) -> true
  | PP.Refuse
      ( PP.No_commits_ahead_of_base | PP.Worktree_missing
      | PP.Branch_ref_missing _ | PP.Branch_switched _ )
  | PP.Push _ ->
      false

(* ---------- Generators ---------- *)

let gen_sha = Gen.string_size ~gen:Gen.printable (Gen.int_range 0 40)
let gen_branch_name = Gen.string_size ~gen:Gen.printable (Gen.int_range 1 30)

let gen_ancestry =
  Gen.oneof_array
    [|
      PP.Local_includes_remote;
      PP.Local_missing_remote;
      PP.Local_diverged_from_remote;
      PP.No_remote_yet;
      PP.Unknown;
    |]

let gen_int_option =
  Gen.oneof [ Gen.return None; Gen.map (fun n -> Some n) (Gen.int_range 0 100) ]

type plan_inputs = {
  expected_branch : string;
  worktree_path_exists : bool;
  worktree_head_branch : string option;
  branch_ref_sha : string option;
  remote_tracking_sha : string option;
  ancestry : PP.ancestry;
  commits_ahead_of_base : int option;
}

let gen_inputs : plan_inputs Gen.t =
  let open Gen in
  let* expected_branch = gen_branch_name in
  let* worktree_path_exists = bool in
  let* worktree_head_branch = option gen_branch_name in
  let* branch_ref_sha = option gen_sha in
  let* remote_tracking_sha = option gen_sha in
  let* ancestry = gen_ancestry in
  let* commits_ahead_of_base = gen_int_option in
  return
    {
      expected_branch;
      worktree_path_exists;
      worktree_head_branch;
      branch_ref_sha;
      remote_tracking_sha;
      ancestry;
      commits_ahead_of_base;
    }

let call i =
  PP.plan ~expected_branch:i.expected_branch
    ~worktree_path_exists:i.worktree_path_exists
    ~worktree_head_branch:i.worktree_head_branch
    ~branch_ref_sha:i.branch_ref_sha ~remote_tracking_sha:i.remote_tracking_sha
    ~ancestry:i.ancestry ~commits_ahead_of_base:i.commits_ahead_of_base

(* ---------- Properties ---------- *)

let prop_totality =
  Test.make ~count:500 ~name:"PPP-1: plan is total" gen_inputs (fun i ->
      try
        let _ : PP.decision = call i in
        true
      with _ -> false)

let prop_branch_switch_refused =
  Test.make ~count:500
    ~name:"PPP-2: head_branch != expected (path exists) → Branch_switched"
    Gen.(
      let* i = gen_inputs in
      let* mismatched =
        oneof
          [
            return None;
            map
              (fun s -> Some (i.expected_branch ^ s ^ "-x"))
              (string_size ~gen:printable (int_range 0 5));
          ]
      in
      return
        {
          i with
          worktree_path_exists = true;
          worktree_head_branch = mismatched;
        })
    (fun i -> is_refuse_branch_switched (call i))

let prop_matching_head_branch_not_switched =
  Test.make ~count:500
    ~name:"PPP-2b: head_branch = expected → not Branch_switched"
    Gen.(
      let* branch = gen_branch_name in
      let* local_sha = gen_sha in
      let* remote_sha = option gen_sha in
      let* anc = gen_ancestry in
      let* commits = gen_int_option in
      return
        {
          expected_branch = branch;
          worktree_path_exists = true;
          worktree_head_branch = Some branch;
          branch_ref_sha = Some local_sha;
          remote_tracking_sha = remote_sha;
          ancestry = anc;
          commits_ahead_of_base = commits;
        })
    (fun i -> not (is_refuse_branch_switched (call i)))

let prop_local_missing_remote_refused =
  Test.make ~count:500
    ~name:
      "PPP-3: ancestry=Local_missing_remote + both refs present → \
       Local_missing_remote_commits"
    Gen.(
      let* local_sha = gen_sha in
      let* remote_sha = gen_sha in
      let* branch = gen_branch_name in
      return (local_sha, remote_sha, branch))
    (fun (local_sha, remote_sha, branch) ->
      let d =
        PP.plan ~expected_branch:branch ~worktree_path_exists:true
          ~worktree_head_branch:(Some branch) ~branch_ref_sha:(Some local_sha)
          ~remote_tracking_sha:(Some remote_sha)
          ~ancestry:PP.Local_missing_remote ~commits_ahead_of_base:(Some 3)
      in
      is_refuse_local_behind d)

let prop_local_diverged_from_remote_permitted =
  (* Regression: an earlier revision refused this case with
     [Local_diverged_from_remote_commits], which stranded the orchestrator
     on the [conflict_noop_count >= 2] needs-intervention threshold every
     time a stacked patch was rebased onto a freshly-advanced base. Post-
     rebase divergence is the legitimate state where local IS the rebased
     version of remote; the git-level [--force-if-includes] flag handles
     the actually-unsafe edge case (remote has commits local doesn't reach
     via its reflog). *)
  Test.make ~count:500
    ~name:
      "PPP-3b: ancestry=Local_diverged_from_remote + both refs present → Push \
       Force_push_if_includes"
    Gen.(
      let* local_sha = gen_sha in
      let* remote_sha = gen_sha in
      let* branch = gen_branch_name in
      let* commits = int_range 1 50 in
      return (local_sha, remote_sha, branch, commits))
    (fun (local_sha, remote_sha, branch, commits) ->
      let d =
        PP.plan ~expected_branch:branch ~worktree_path_exists:true
          ~worktree_head_branch:(Some branch) ~branch_ref_sha:(Some local_sha)
          ~remote_tracking_sha:(Some remote_sha)
          ~ancestry:PP.Local_diverged_from_remote
          ~commits_ahead_of_base:(Some commits)
      in
      is_push_force d)

let prop_zero_commits_skip =
  Test.make ~count:500
    ~name:"PPP-4: commits_ahead_of_base=Some 0 → No_commits_ahead_of_base"
    Gen.(
      let* branch = gen_branch_name in
      let* local_sha = gen_sha in
      let* remote = option gen_sha in
      let* anc = gen_ancestry in
      return (branch, local_sha, remote, anc))
    (fun (branch, local_sha, remote, anc) ->
      let d =
        PP.plan ~expected_branch:branch ~worktree_path_exists:true
          ~worktree_head_branch:(Some branch) ~branch_ref_sha:(Some local_sha)
          ~remote_tracking_sha:remote ~ancestry:anc
          ~commits_ahead_of_base:(Some 0)
      in
      is_refuse_no_commits d)

let prop_initial_push =
  Test.make ~count:500
    ~name:"PPP-5: no remote, no other refusal → Push Initial_push"
    Gen.(
      let* branch = gen_branch_name in
      let* local_sha = gen_sha in
      let* commits =
        oneof [ return None; map (fun n -> Some n) (int_range 1 50) ]
      in
      let* anc = oneof_array [| PP.No_remote_yet; PP.Unknown |] in
      return (branch, local_sha, commits, anc))
    (fun (branch, local_sha, commits, anc) ->
      let d =
        PP.plan ~expected_branch:branch ~worktree_path_exists:true
          ~worktree_head_branch:(Some branch) ~branch_ref_sha:(Some local_sha)
          ~remote_tracking_sha:None ~ancestry:anc ~commits_ahead_of_base:commits
      in
      is_push_initial d)

let prop_happy_path =
  Test.make ~count:500 ~name:"PPP-6: happy path → Push Force_push_if_includes"
    Gen.(
      let* branch = gen_branch_name in
      let* local_sha = gen_sha in
      let* remote_sha = gen_sha in
      let* anc =
        oneof_array
          [|
            PP.Local_includes_remote;
            PP.Local_diverged_from_remote;
            PP.No_remote_yet;
            PP.Unknown;
          |]
      in
      let* commits = int_range 1 50 in
      return (branch, local_sha, remote_sha, anc, commits))
    (fun (branch, local_sha, remote_sha, anc, commits) ->
      let d =
        PP.plan ~expected_branch:branch ~worktree_path_exists:true
          ~worktree_head_branch:(Some branch) ~branch_ref_sha:(Some local_sha)
          ~remote_tracking_sha:(Some remote_sha) ~ancestry:anc
          ~commits_ahead_of_base:(Some commits)
      in
      is_push_force d)

let prop_worktree_missing_preempts =
  Test.make ~count:500
    ~name:"PPP-7: worktree_path_exists=false pre-empts everything" gen_inputs
    (fun i ->
      let d = call { i with worktree_path_exists = false } in
      is_refuse_wt_missing d)

let prop_determinism =
  Test.make ~count:500 ~name:"PPP-8: same inputs → same output" gen_inputs
    (fun i -> PP.equal_decision (call i) (call i))

let prop_variants_reachable =
  Test.make ~count:1 ~name:"PPP-9: every decision variant is reachable"
    (Gen.return ()) (fun () ->
      let force =
        PP.plan ~expected_branch:"b" ~worktree_path_exists:true
          ~worktree_head_branch:(Some "b") ~branch_ref_sha:(Some "l")
          ~remote_tracking_sha:(Some "r") ~ancestry:PP.Local_includes_remote
          ~commits_ahead_of_base:(Some 1)
      in
      let initial =
        PP.plan ~expected_branch:"b" ~worktree_path_exists:true
          ~worktree_head_branch:(Some "b") ~branch_ref_sha:(Some "l")
          ~remote_tracking_sha:None ~ancestry:PP.No_remote_yet
          ~commits_ahead_of_base:(Some 1)
      in
      let no_commits =
        PP.plan ~expected_branch:"b" ~worktree_path_exists:true
          ~worktree_head_branch:(Some "b") ~branch_ref_sha:(Some "l")
          ~remote_tracking_sha:(Some "r") ~ancestry:PP.Local_includes_remote
          ~commits_ahead_of_base:(Some 0)
      in
      let wt_missing =
        PP.plan ~expected_branch:"b" ~worktree_path_exists:false
          ~worktree_head_branch:(Some "b") ~branch_ref_sha:(Some "l")
          ~remote_tracking_sha:(Some "r") ~ancestry:PP.Local_includes_remote
          ~commits_ahead_of_base:(Some 1)
      in
      let ref_missing =
        PP.plan ~expected_branch:"b" ~worktree_path_exists:true
          ~worktree_head_branch:(Some "b") ~branch_ref_sha:None
          ~remote_tracking_sha:(Some "r") ~ancestry:PP.Unknown
          ~commits_ahead_of_base:(Some 1)
      in
      let branch_switched =
        PP.plan ~expected_branch:"b" ~worktree_path_exists:true
          ~worktree_head_branch:(Some "other") ~branch_ref_sha:(Some "l")
          ~remote_tracking_sha:(Some "r") ~ancestry:PP.Local_includes_remote
          ~commits_ahead_of_base:(Some 1)
      in
      let local_behind =
        PP.plan ~expected_branch:"b" ~worktree_path_exists:true
          ~worktree_head_branch:(Some "b") ~branch_ref_sha:(Some "l")
          ~remote_tracking_sha:(Some "r") ~ancestry:PP.Local_missing_remote
          ~commits_ahead_of_base:(Some 1)
      in
      let local_diverged_now_pushes =
        PP.plan ~expected_branch:"b" ~worktree_path_exists:true
          ~worktree_head_branch:(Some "b") ~branch_ref_sha:(Some "l")
          ~remote_tracking_sha:(Some "r")
          ~ancestry:PP.Local_diverged_from_remote
          ~commits_ahead_of_base:(Some 1)
      in
      is_push_force force && is_push_initial initial
      && is_refuse_no_commits no_commits
      && is_refuse_wt_missing wt_missing
      && is_refuse_ref_missing ref_missing
      && is_refuse_branch_switched branch_switched
      && is_refuse_local_behind local_behind
      && is_push_force local_diverged_now_pushes)

let prop_refusal_to_rejection_permanent =
  Test.make ~count:1
    ~name:
      "PPP-10: every refusal that maps to Some rej yields a permanent \
       Push_reject_classify rejection" (Gen.return ()) (fun () ->
      let refusals =
        [
          PP.No_commits_ahead_of_base;
          PP.Worktree_missing;
          PP.Branch_ref_missing { branch = "b" };
          PP.Branch_switched { expected = "b"; got = Some "other" };
          PP.Local_missing_remote_commits { local_sha = "l"; remote_sha = "r" };
        ]
      in
      List.for_all refusals ~f:(fun r ->
          match PP.to_push_reject_classify_rejection r with
          | None -> true
          | Some rej -> Push_reject_classify.is_permanent rej))

let label_re = Re.Pcre.re {|^[a-z][a-z0-9_]*$|} |> Re.compile

let prop_label_bounds =
  Test.make ~count:500 ~name:"PPP-11: short_label non-empty, ≤32, snake_case"
    gen_inputs (fun i ->
      let lbl = PP.short_label (call i) in
      (not (String.is_empty lbl))
      && String.length lbl <= 32
      && Re.execp label_re lbl)

let () =
  let runner = QCheck_base_runner.run_tests_main in
  ignore
    (runner
       [
         prop_totality;
         prop_branch_switch_refused;
         prop_matching_head_branch_not_switched;
         prop_local_missing_remote_refused;
         prop_local_diverged_from_remote_permitted;
         prop_zero_commits_skip;
         prop_initial_push;
         prop_happy_path;
         prop_worktree_missing_preempts;
         prop_determinism;
         prop_variants_reachable;
         prop_refusal_to_rejection_permanent;
         prop_label_bounds;
       ])
