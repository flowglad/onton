(* @archlint.module test
   @archlint.domain start-point-plan *)

open Base
open Onton_core

(** Property tests for {!Start_point_plan}.

    Properties cover:

    - {b SPP-1 Totality}: [plan] never raises on any combination of inputs.
    - {b SPP-2 No silent clobber}: if both refs are present and ancestry is
      [Local_ahead], [Diverged], or [Unknown], the decision is [Refuse _] — we
      never silently overwrite local commits the supervisor doesn't know about.
    - {b SPP-3 Remote authoritative}: when both refs are present and ancestry is
      [Equal] or [Remote_ahead], the action is [Reset_and_use_remote_tracking].
    - {b SPP-4 Missing both → create from base}: both refs absent ⇒
      [Create_new_branch_from_base], unconditionally (base freshness vs main is
      not gated here — see {!Start_point_plan.plan}).
    - {b SPP-5 Determinism}: same inputs yield identical output.
    - {b SPP-6 Pre-emption}: [branch_checked_out_in_main_root = true] always
      yields [Refuse Branch_checked_out_in_main_root] regardless of every other
      input; [existing_worktree_path = Some _] yields
      [Refuse Worktree_already_registered _] regardless of refs/ancestry.
    - {b SPP-7 Label bounds}: [short_label] is non-empty, ≤ 32 chars, lowercase
      \+ snake_case (matches [^[a-z][a-z0-9_]*$]).
    - {b SPP-8 Variant reachability}: for each [decision] constructor, exhibit
      an input that produces it.
    - {b SPP-9 Remote authority over local}: when [remote_ref = Some _] and
      ancestry is non-divergent, the action is never
      [Use_local_branch_unchanged] (remote always wins). *)

module Gen = QCheck2.Gen
module Test = QCheck2.Test
module SP = Start_point_plan

(* ---------- Helpers (exhaustive predicates over action / refusal) ---------- *)

let is_reset_to (remote : string) = function
  | SP.Plan (SP.Reset_and_use_remote_tracking { remote_sha }) ->
      String.equal remote_sha remote
  | SP.Plan (SP.Use_local_branch_unchanged _)
  | SP.Plan (SP.Create_new_branch_from_base _)
  | SP.Refuse _ ->
      false

let is_use_local = function
  | SP.Plan (SP.Use_local_branch_unchanged _) -> true
  | SP.Plan (SP.Reset_and_use_remote_tracking _)
  | SP.Plan (SP.Create_new_branch_from_base _)
  | SP.Refuse _ ->
      false

let is_create_from_base (b : string) = function
  | SP.Plan (SP.Create_new_branch_from_base { base_branch }) ->
      String.equal base_branch b
  | SP.Plan (SP.Reset_and_use_remote_tracking _)
  | SP.Plan (SP.Use_local_branch_unchanged _)
  | SP.Refuse _ ->
      false

let is_refuse = function SP.Refuse _ -> true | SP.Plan _ -> false

let is_refuse_diverged = function
  | SP.Refuse (SP.Local_diverged_from_remote _) -> true
  | SP.Refuse
      ( SP.Local_has_unpushed_commits _ | SP.Branch_checked_out_in_main_root
      | SP.Worktree_already_registered _ )
  | SP.Plan _ ->
      false

let is_refuse_local_ahead = function
  | SP.Refuse (SP.Local_has_unpushed_commits _) -> true
  | SP.Refuse
      ( SP.Local_diverged_from_remote _ | SP.Branch_checked_out_in_main_root
      | SP.Worktree_already_registered _ )
  | SP.Plan _ ->
      false

let is_refuse_checked_out = function
  | SP.Refuse SP.Branch_checked_out_in_main_root -> true
  | SP.Refuse
      ( SP.Local_diverged_from_remote _ | SP.Local_has_unpushed_commits _
      | SP.Worktree_already_registered _ )
  | SP.Plan _ ->
      false

let is_refuse_registered = function
  | SP.Refuse (SP.Worktree_already_registered _) -> true
  | SP.Refuse
      ( SP.Local_diverged_from_remote _ | SP.Local_has_unpushed_commits _
      | SP.Branch_checked_out_in_main_root ) ->
      false
  | SP.Plan _ -> false

(* ---------- Generators ---------- *)

let gen_sha = Gen.string_size ~gen:Gen.printable (Gen.int_range 0 40)
let gen_branch_name = Gen.string_size ~gen:Gen.printable (Gen.int_range 0 30)

let gen_ancestry =
  Gen.oneof_array
    [| SP.Local_ahead; SP.Remote_ahead; SP.Equal; SP.Diverged; SP.Unknown |]

let gen_sha_option = Gen.option gen_sha
let gen_path_option = Gen.option gen_branch_name

(* Random inputs to [plan]; tuple-packed so generators stay small. *)
type plan_inputs = {
  local_ref : SP.sha option;
  remote_ref : SP.sha option;
  ancestry : SP.ancestry;
  base_branch : string;
  branch_checked_out_in_main_root : bool;
  existing_worktree_path : string option;
}

let gen_inputs : plan_inputs Gen.t =
  let open Gen in
  let* local_ref = gen_sha_option in
  let* remote_ref = gen_sha_option in
  let* ancestry = gen_ancestry in
  let* base_branch = gen_branch_name in
  let* branch_checked_out_in_main_root = bool in
  let* existing_worktree_path = gen_path_option in
  return
    {
      local_ref;
      remote_ref;
      ancestry;
      base_branch;
      branch_checked_out_in_main_root;
      existing_worktree_path;
    }

let call i =
  SP.plan ~local_ref:i.local_ref ~remote_ref:i.remote_ref ~ancestry:i.ancestry
    ~base_branch:i.base_branch
    ~branch_checked_out_in_main_root:i.branch_checked_out_in_main_root
    ~existing_worktree_path:i.existing_worktree_path

(* ---------- Properties ---------- *)

let prop_totality =
  Test.make ~count:500 ~name:"SPP-1: plan is total" gen_inputs (fun i ->
      try
        let _ : SP.decision = call i in
        true
      with _ -> false)

let prop_no_silent_clobber =
  Test.make ~count:500
    ~name:"SPP-2: both refs present + non-Remote-ahead/Equal ancestry → Refuse"
    Gen.(
      let* local = gen_sha in
      let* remote = gen_sha in
      let* anc = oneof_array [| SP.Local_ahead; SP.Diverged; SP.Unknown |] in
      let* base_branch = gen_branch_name in
      return (local, remote, anc, base_branch))
    (fun (local, remote, anc, base_branch) ->
      let d =
        SP.plan ~local_ref:(Some local) ~remote_ref:(Some remote) ~ancestry:anc
          ~base_branch ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:None
      in
      is_refuse d)

let prop_remote_authoritative =
  Test.make ~count:500
    ~name:
      "SPP-3: both refs + Equal/Remote_ahead → Reset_and_use_remote_tracking"
    Gen.(
      let* local = gen_sha in
      let* remote = gen_sha in
      let* anc = oneof_array [| SP.Equal; SP.Remote_ahead |] in
      let* base_branch = gen_branch_name in
      return (local, remote, anc, base_branch))
    (fun (local, remote, anc, base_branch) ->
      let d =
        SP.plan ~local_ref:(Some local) ~remote_ref:(Some remote) ~ancestry:anc
          ~base_branch ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:None
      in
      is_reset_to remote d)

let prop_missing_both =
  Test.make ~count:200
    ~name:"SPP-4: local=None, remote=None → Create_new_branch_from_base"
    Gen.(
      let* base_branch = gen_branch_name in
      let* anc = gen_ancestry in
      return (base_branch, anc))
    (fun (base_branch, anc) ->
      let d =
        SP.plan ~local_ref:None ~remote_ref:None ~ancestry:anc ~base_branch
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      is_create_from_base base_branch d)

let prop_determinism =
  Test.make ~count:500 ~name:"SPP-5: same inputs → same output" gen_inputs
    (fun i -> SP.equal_decision (call i) (call i))

let prop_preempt_checked_out =
  Test.make ~count:500
    ~name:"SPP-6a: branch_checked_out_in_main_root pre-empts everything"
    gen_inputs (fun i ->
      let d = call { i with branch_checked_out_in_main_root = true } in
      SP.equal_decision d (SP.Refuse SP.Branch_checked_out_in_main_root))

let prop_preempt_worktree =
  Test.make ~count:500
    ~name:"SPP-6b: existing_worktree_path pre-empts refs/ancestry"
    Gen.(
      let* i = gen_inputs in
      let* p = gen_branch_name in
      return (i, p))
    (fun (i, p) ->
      let d =
        call
          {
            i with
            branch_checked_out_in_main_root = false;
            existing_worktree_path = Some p;
          }
      in
      SP.equal_decision d
        (SP.Refuse (SP.Worktree_already_registered { existing_path = p })))

let label_re = Re.Pcre.re {|^[a-z][a-z0-9_]*$|} |> Re.compile

let prop_label_bounds =
  Test.make ~count:500 ~name:"SPP-7: short_label non-empty, ≤32, snake_case"
    gen_inputs (fun i ->
      let lbl = SP.short_label (call i) in
      (not (String.is_empty lbl))
      && String.length lbl <= 32
      && Re.execp label_re lbl)

let prop_variants_reachable =
  Test.make ~count:1 ~name:"SPP-8: every decision variant is reachable"
    (Gen.return ()) (fun () ->
      let reset =
        SP.plan ~local_ref:None ~remote_ref:(Some "r") ~ancestry:SP.Unknown
          ~base_branch:"main" ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:None
      in
      let use_local =
        SP.plan ~local_ref:(Some "l") ~remote_ref:None ~ancestry:SP.Unknown
          ~base_branch:"main" ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:None
      in
      let create =
        SP.plan ~local_ref:None ~remote_ref:None ~ancestry:SP.Unknown
          ~base_branch:"main" ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:None
      in
      let refuse_diverged =
        SP.plan ~local_ref:(Some "l") ~remote_ref:(Some "r")
          ~ancestry:SP.Diverged ~base_branch:"main"
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      let refuse_ahead =
        SP.plan ~local_ref:(Some "l") ~remote_ref:(Some "r")
          ~ancestry:SP.Local_ahead ~base_branch:"main"
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      let refuse_checked_out =
        SP.plan ~local_ref:None ~remote_ref:None ~ancestry:SP.Unknown
          ~base_branch:"main" ~branch_checked_out_in_main_root:true
          ~existing_worktree_path:None
      in
      let refuse_registered =
        SP.plan ~local_ref:None ~remote_ref:None ~ancestry:SP.Unknown
          ~base_branch:"main" ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:(Some "/tmp/wt")
      in
      is_reset_to "r" reset && is_use_local use_local
      && is_create_from_base "main" create
      && is_refuse_diverged refuse_diverged
      && is_refuse_local_ahead refuse_ahead
      && is_refuse_checked_out refuse_checked_out
      && is_refuse_registered refuse_registered)

let prop_remote_wins_over_local =
  Test.make ~count:500
    ~name:
      "SPP-9: remote present + non-divergent ancestry → never \
       Use_local_branch_unchanged"
    Gen.(
      let* local = gen_sha in
      let* remote = gen_sha in
      let* anc = oneof_array [| SP.Equal; SP.Remote_ahead |] in
      let* base_branch = gen_branch_name in
      return (local, remote, anc, base_branch))
    (fun (local, remote, anc, base_branch) ->
      let d =
        SP.plan ~local_ref:(Some local) ~remote_ref:(Some remote) ~ancestry:anc
          ~base_branch ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:None
      in
      not (is_use_local d))

(* ---------- base_start_point (BSP) ----------

   The brand-new-branch cut's base ref. Invariants, derived from the authority
   rules (remote canonical for main — the orchestrator never advances local
   main; local canonical for dependency branches — the dep's worktree writes
   locally first and origin lags until push):

   - BSP-1 totality/determinism over all inputs.
   - BSP-2 main base + fetched SHA → cut pinned to that SHA.
   - BSP-3 a non-main base NEVER cuts from a remote SHA, even if supplied.
   - BSP-4 main base without a fetched SHA falls back (fail-open) to the local
     ref — pre-fetch behavior, freshen-rebase detectors as backstop.
   - BSP-5 the rendered ref is exactly the SHA / the base branch name.
   - BSP-6 label bounds. *)

type bsp_inputs = {
  bsp_base_branch : string;
  bsp_base_is_main : bool;
  bsp_fetched_remote_sha : string option;
}

let gen_bsp_inputs : bsp_inputs Gen.t =
  let open Gen in
  let* bsp_base_branch = gen_branch_name in
  let* bsp_base_is_main = bool in
  let* bsp_fetched_remote_sha = gen_sha_option in
  return { bsp_base_branch; bsp_base_is_main; bsp_fetched_remote_sha }

let call_bsp i =
  SP.base_start_point ~base_branch:i.bsp_base_branch
    ~base_is_main:i.bsp_base_is_main
    ~fetched_remote_sha:i.bsp_fetched_remote_sha

let prop_bsp_total_deterministic =
  Test.make ~count:500 ~name:"BSP-1: base_start_point total + deterministic"
    gen_bsp_inputs (fun i ->
      try SP.equal_base_start_point (call_bsp i) (call_bsp i) with _ -> false)

let prop_bsp_main_uses_fetched_sha =
  Test.make ~count:500
    ~name:"BSP-2: main base + fetched SHA → Base_at_fetched_remote_sha"
    Gen.(
      let* base_branch = gen_branch_name in
      let* sha = gen_sha in
      return (base_branch, sha))
    (fun (base_branch, sha) ->
      SP.equal_base_start_point
        (SP.base_start_point ~base_branch ~base_is_main:true
           ~fetched_remote_sha:(Some sha))
        (SP.Base_at_fetched_remote_sha { sha }))

let prop_bsp_non_main_never_remote =
  Test.make ~count:500 ~name:"BSP-3: non-main base never cuts from a remote SHA"
    gen_bsp_inputs (fun i ->
      SP.equal_base_start_point
        (call_bsp { i with bsp_base_is_main = false })
        (SP.Base_at_local_ref { base_branch = i.bsp_base_branch }))

let prop_bsp_main_no_sha_falls_back =
  Test.make ~count:500
    ~name:"BSP-4: main base without fetched SHA falls back to the local ref"
    gen_branch_name (fun base_branch ->
      SP.equal_base_start_point
        (SP.base_start_point ~base_branch ~base_is_main:true
           ~fetched_remote_sha:None)
        (SP.Base_at_local_ref { base_branch }))

let prop_bsp_ref_rendering =
  Test.make ~count:500
    ~name:"BSP-5: base_start_point_ref renders the SHA / the base branch"
    gen_bsp_inputs (fun i ->
      let sp = call_bsp i in
      let rendered = SP.base_start_point_ref sp in
      match sp with
      | SP.Base_at_fetched_remote_sha { sha } -> String.equal rendered sha
      | SP.Base_at_local_ref { base_branch } ->
          String.equal rendered base_branch)

let prop_bsp_label_bounds =
  Test.make ~count:500
    ~name:"BSP-6: base_start_point_short_label non-empty, ≤32, snake_case"
    gen_bsp_inputs (fun i ->
      let lbl = SP.base_start_point_short_label (call_bsp i) in
      (not (String.is_empty lbl))
      && String.length lbl <= 32
      && Re.execp label_re lbl)

let () =
  let runner = QCheck_base_runner.run_tests_main in
  ignore
    (runner
       [
         prop_totality;
         prop_no_silent_clobber;
         prop_remote_authoritative;
         prop_missing_both;
         prop_determinism;
         prop_preempt_checked_out;
         prop_preempt_worktree;
         prop_label_bounds;
         prop_variants_reachable;
         prop_remote_wins_over_local;
         prop_bsp_total_deterministic;
         prop_bsp_main_uses_fetched_sha;
         prop_bsp_non_main_never_remote;
         prop_bsp_main_no_sha_falls_back;
         prop_bsp_ref_rendering;
         prop_bsp_label_bounds;
       ])
