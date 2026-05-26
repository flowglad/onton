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
      [Create_new_branch_from_base].
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
      | SP.Worktree_already_registered _ | SP.Base_branch_stale_vs_main _ )
  | SP.Plan _ ->
      false

let is_refuse_local_ahead = function
  | SP.Refuse (SP.Local_has_unpushed_commits _) -> true
  | SP.Refuse
      ( SP.Local_diverged_from_remote _ | SP.Branch_checked_out_in_main_root
      | SP.Worktree_already_registered _ | SP.Base_branch_stale_vs_main _ )
  | SP.Plan _ ->
      false

let is_refuse_checked_out = function
  | SP.Refuse SP.Branch_checked_out_in_main_root -> true
  | SP.Refuse
      ( SP.Local_diverged_from_remote _ | SP.Local_has_unpushed_commits _
      | SP.Worktree_already_registered _ | SP.Base_branch_stale_vs_main _ )
  | SP.Plan _ ->
      false

let is_refuse_registered = function
  | SP.Refuse (SP.Worktree_already_registered _) -> true
  | SP.Refuse
      ( SP.Local_diverged_from_remote _ | SP.Local_has_unpushed_commits _
      | SP.Branch_checked_out_in_main_root | SP.Base_branch_stale_vs_main _ ) ->
      false
  | SP.Plan _ -> false

let is_refuse_base_stale = function
  | SP.Refuse (SP.Base_branch_stale_vs_main _) -> true
  | SP.Refuse
      ( SP.Local_diverged_from_remote _ | SP.Local_has_unpushed_commits _
      | SP.Branch_checked_out_in_main_root | SP.Worktree_already_registered _ )
  | SP.Plan _ ->
      false

(* ---------- Generators ---------- *)

let gen_sha = Gen.string_size ~gen:Gen.printable (Gen.int_range 0 40)
let gen_branch_name = Gen.string_size ~gen:Gen.printable (Gen.int_range 0 30)

let gen_ancestry =
  Gen.oneof_array
    [| SP.Local_ahead; SP.Remote_ahead; SP.Equal; SP.Diverged; SP.Unknown |]

let gen_base_freshness =
  Gen.oneof_array [| SP.Fresh; SP.Stale; SP.Unknown_freshness |]

let gen_sha_option = Gen.option gen_sha
let gen_path_option = Gen.option gen_branch_name

(* Random inputs to [plan]; tuple-packed so generators stay small. *)
type plan_inputs = {
  local_ref : SP.sha option;
  remote_ref : SP.sha option;
  ancestry : SP.ancestry;
  base_branch : string;
  base_freshness : SP.base_freshness;
  branch_checked_out_in_main_root : bool;
  existing_worktree_path : string option;
}

let gen_inputs : plan_inputs Gen.t =
  let open Gen in
  let* local_ref = gen_sha_option in
  let* remote_ref = gen_sha_option in
  let* ancestry = gen_ancestry in
  let* base_branch = gen_branch_name in
  let* base_freshness = gen_base_freshness in
  let* branch_checked_out_in_main_root = bool in
  let* existing_worktree_path = gen_path_option in
  return
    {
      local_ref;
      remote_ref;
      ancestry;
      base_branch;
      base_freshness;
      branch_checked_out_in_main_root;
      existing_worktree_path;
    }

let call i =
  SP.plan ~local_ref:i.local_ref ~remote_ref:i.remote_ref ~ancestry:i.ancestry
    ~base_branch:i.base_branch ~base_freshness:i.base_freshness
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
          ~base_branch ~base_freshness:SP.Unknown_freshness
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
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
          ~base_branch ~base_freshness:SP.Unknown_freshness
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      is_reset_to remote d)

let prop_missing_both =
  Test.make ~count:200
    ~name:
      "SPP-4: local=None, remote=None, base not Stale → \
       Create_new_branch_from_base"
    Gen.(
      let* base_branch = gen_branch_name in
      let* anc = gen_ancestry in
      let* fresh = oneof_array [| SP.Fresh; SP.Unknown_freshness |] in
      return (base_branch, anc, fresh))
    (fun (base_branch, anc, fresh) ->
      let d =
        SP.plan ~local_ref:None ~remote_ref:None ~ancestry:anc ~base_branch
          ~base_freshness:fresh ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:None
      in
      is_create_from_base base_branch d)

let prop_missing_both_stale =
  Test.make ~count:200
    ~name:
      "SPP-4b: local=None, remote=None, base Stale → Refuse \
       Base_branch_stale_vs_main"
    Gen.(
      let* base_branch = gen_branch_name in
      let* anc = gen_ancestry in
      return (base_branch, anc))
    (fun (base_branch, anc) ->
      let d =
        SP.plan ~local_ref:None ~remote_ref:None ~ancestry:anc ~base_branch
          ~base_freshness:SP.Stale ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:None
      in
      is_refuse_base_stale d)

(* A Stale base must NOT block when the patch's own branch already exists —
   freshness is only consulted on the brand-new-branch arm. *)
let prop_stale_irrelevant_when_branch_exists =
  Test.make ~count:300
    ~name:"SPP-4c: base Stale is ignored when a local/remote ref exists"
    Gen.(
      let* local = gen_sha_option in
      let* remote = gen_sha_option in
      (* exclude (None, None) — that's the brand-new arm where Stale matters *)
      let local, remote =
        match (local, remote) with None, None -> (Some "l", None) | p -> p
      in
      let* anc = gen_ancestry in
      let* base_branch = gen_branch_name in
      return (local, remote, anc, base_branch))
    (fun (local, remote, anc, base_branch) ->
      let d =
        SP.plan ~local_ref:local ~remote_ref:remote ~ancestry:anc ~base_branch
          ~base_freshness:SP.Stale ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:None
      in
      not (is_refuse_base_stale d))

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
          ~base_branch:"main" ~base_freshness:SP.Unknown_freshness
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      let use_local =
        SP.plan ~local_ref:(Some "l") ~remote_ref:None ~ancestry:SP.Unknown
          ~base_branch:"main" ~base_freshness:SP.Unknown_freshness
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      let create =
        SP.plan ~local_ref:None ~remote_ref:None ~ancestry:SP.Unknown
          ~base_branch:"main" ~base_freshness:SP.Fresh
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      let refuse_diverged =
        SP.plan ~local_ref:(Some "l") ~remote_ref:(Some "r")
          ~ancestry:SP.Diverged ~base_branch:"main"
          ~base_freshness:SP.Unknown_freshness
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      let refuse_ahead =
        SP.plan ~local_ref:(Some "l") ~remote_ref:(Some "r")
          ~ancestry:SP.Local_ahead ~base_branch:"main"
          ~base_freshness:SP.Unknown_freshness
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      let refuse_checked_out =
        SP.plan ~local_ref:None ~remote_ref:None ~ancestry:SP.Unknown
          ~base_branch:"main" ~base_freshness:SP.Fresh
          ~branch_checked_out_in_main_root:true ~existing_worktree_path:None
      in
      let refuse_registered =
        SP.plan ~local_ref:None ~remote_ref:None ~ancestry:SP.Unknown
          ~base_branch:"main" ~base_freshness:SP.Fresh
          ~branch_checked_out_in_main_root:false
          ~existing_worktree_path:(Some "/tmp/wt")
      in
      let refuse_base_stale =
        SP.plan ~local_ref:None ~remote_ref:None ~ancestry:SP.Unknown
          ~base_branch:"dep" ~base_freshness:SP.Stale
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      is_reset_to "r" reset && is_use_local use_local
      && is_create_from_base "main" create
      && is_refuse_diverged refuse_diverged
      && is_refuse_local_ahead refuse_ahead
      && is_refuse_checked_out refuse_checked_out
      && is_refuse_registered refuse_registered
      && is_refuse_base_stale refuse_base_stale)

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
          ~base_branch ~base_freshness:SP.Unknown_freshness
          ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
      in
      not (is_use_local d))

let () =
  let runner = QCheck_base_runner.run_tests_main in
  ignore
    (runner
       [
         prop_totality;
         prop_no_silent_clobber;
         prop_remote_authoritative;
         prop_missing_both;
         prop_missing_both_stale;
         prop_stale_irrelevant_when_branch_exists;
         prop_determinism;
         prop_preempt_checked_out;
         prop_preempt_worktree;
         prop_label_bounds;
         prop_variants_reachable;
         prop_remote_wins_over_local;
       ])
