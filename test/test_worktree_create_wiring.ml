(* @archlint.module test
   @archlint.domain start-point-plan *)

(* Wiring test for [Worktree.create_with_io].

   Drives the [create] control flow — the [Sys.file_exists] short-circuit, the
   ref reads, the ancestry probe, the {!Start_point_plan.plan} dispatch, and the
   action execution / refusal mapping — with in-memory fakes instead of a live
   git repository. The pure planner arms are already covered exhaustively in
   {!test_start_point_plan_properties}; this test verifies that [create] feeds
   the planner the right inputs and faithfully executes (or refuses) the result,
   deterministically and without spawning git. The live-git contract that backs
   the fakes ([read_repo_ref_sha], [compute_repo_ancestry]) is verified
   separately in {!test_worktree_start_point_integration}. *)

open Base
module Worktree = Onton.Worktree
module SP = Onton_core.Start_point_plan
module Types = Onton_core.Types

let branch = Types.Branch.of_string "feat"
let patch_id = Types.Patch_id.of_string "1"
let project_name = "proj"
let local_sha = "1111111111111111111111111111111111111111"
let remote_sha = "2222222222222222222222222222222222222222"
let heads = "refs/heads/feat"
let remotes = "refs/remotes/origin/feat"

(* Build a scripted [create_io]. [refs] maps ref names to SHAs; absent refs read
   as [None]. [exists] toggles the worktree short-circuit. [ancestry] is the
   relationship returned for any (local, remote) pair. The returned trackers
   record what the wiring actually did: which action it executed, and whether it
   consulted the ancestry probe / the collision guard. *)
let make_io ?(exists = false) ?(refs = []) ?(ancestry = SP.Unknown) () =
  let executed = ref None in
  let ancestry_called = ref false in
  let collision_called = ref false in
  let io : Worktree.create_io =
    {
      Worktree.worktree_exists = (fun ~path:_ -> exists);
      check_ref_collision = (fun ~branch_str:_ -> collision_called := true);
      read_ref =
        (fun ~ref_name ->
          Ok (List.Assoc.find refs ref_name ~equal:String.equal));
      ancestry =
        (fun ~local:_ ~remote:_ ->
          ancestry_called := true;
          ancestry);
      execute_action =
        (fun ~path:_ ~branch_str:_ ~expected_local:_ action ->
          executed := Some action;
          true);
    }
  in
  (io, executed, ancestry_called, collision_called)

let run io =
  Worktree.create_with_io ~io ~project_name ~patch_id ~branch
    ~base_ref:"origin/main"

let check name cond =
  if cond then Stdlib.print_endline ("  " ^ name ^ ": OK")
  else failwith (name ^ ": FAILED")

let is_ok = function Ok _ -> true | Error _ -> false
let executed_is executed a = Option.equal SP.equal_action !executed (Some a)
let reset = SP.Reset_and_use_remote_tracking { remote_sha }

(* Exhaustive (no wildcard) classifier for the refusal arm that fired. *)
let refusal_tag (r : SP.refusal) =
  match r with
  | SP.Ancestry_unavailable _ -> "ancestry_unavailable"
  | SP.Branch_checked_out_in_main_root -> "main_checkout"
  | SP.Worktree_already_registered _ -> "wt_registered"

let error_tag res = match res with Ok _ -> "ok" | Error r -> refusal_tag r

(* Reset-to-remote: stale local + remote ahead. This is the PR #315 regression
   guard — a stale local ref must NOT be used as-is; the worktree must be reset
   to the remote tip. *)
let test_reset_when_remote_ahead () =
  let io, executed, ancestry_called, _ =
    make_io
      ~refs:[ (heads, local_sha); (remotes, remote_sha) ]
      ~ancestry:SP.Remote_ahead ()
  in
  let res = run io in
  check "reset_when_remote_ahead: ancestry was probed" !ancestry_called;
  check "reset_when_remote_ahead" (is_ok res && executed_is executed reset)

(* Equal ancestry also resets to remote (remote is authoritative). *)
let test_reset_when_equal () =
  let io, executed, _, _ =
    make_io
      ~refs:[ (heads, local_sha); (remotes, remote_sha) ]
      ~ancestry:SP.Equal ()
  in
  let res = run io in
  check "reset_when_equal" (is_ok res && executed_is executed reset)

(* Remote ref present, no local ref: reset to remote without probing ancestry
   (the planner needs none when one side is absent). *)
let test_remote_only () =
  let io, executed, ancestry_called, _ =
    make_io ~refs:[ (remotes, remote_sha) ] ()
  in
  let res = run io in
  check "remote_only: ancestry not probed (one side absent)"
    (not !ancestry_called);
  check "remote_only" (is_ok res && executed_is executed reset)

(* Local ref present, no remote ref (branch never pushed): use the local branch
   unchanged. Ancestry must NOT be probed. *)
let test_local_only () =
  let io, executed, ancestry_called, _ =
    make_io ~refs:[ (heads, local_sha) ] ()
  in
  let res = run io in
  check "local_only: ancestry not probed (one side absent)"
    (not !ancestry_called);
  check "local_only"
    (is_ok res
    && executed_is executed (SP.Use_local_branch_unchanged { local_sha }))

(* Neither ref exists: create a brand-new branch from the base. *)
let test_brand_new () =
  let io, executed, _, _ = make_io ~refs:[] () in
  let res = run io in
  check "brand_new"
    (is_ok res
    && executed_is executed
         (SP.Create_new_branch_from_base { base_branch = "origin/main" }))

let test_preserve_local ancestry =
  let io, executed, _, _ =
    make_io ~refs:[ (heads, local_sha); (remotes, remote_sha) ] ~ancestry ()
  in
  let res = run io in
  check "unique local history is adopted unchanged"
    (is_ok res
    && executed_is executed (SP.Use_local_branch_unchanged { local_sha }))

let test_unknown_ancestry_defers () =
  let io, executed, _, _ =
    make_io
      ~refs:[ (heads, local_sha); (remotes, remote_sha) ]
      ~ancestry:SP.Unknown ()
  in
  let res = run io in
  check "failed ancestry cannot materialize" (Option.is_none !executed);
  check "failed ancestry is retryable"
    (String.equal (error_tag res) "ancestry_unavailable")

let test_ref_probe_error () =
  let io, executed, ancestry_called, _ = make_io () in
  let io =
    {
      io with
      Worktree.read_ref = (fun ~ref_name:_ -> Error "probe unavailable");
    }
  in
  let failed =
    match run io with Ok _ | Error _ -> false | exception Failure _ -> true
  in
  check "ref probe errors cannot become absent branches"
    (failed && Option.is_none !executed && not !ancestry_called)

(* Short-circuit: when the worktree path already exists, [create] trusts it and
   performs NO git work — no collision check, no ref reads, no ancestry, no
   action — returning the existing path. *)
let test_short_circuit () =
  let io, executed, ancestry_called, collision_called =
    make_io ~exists:true
      ~refs:[ (heads, local_sha); (remotes, remote_sha) ]
      ~ancestry:SP.Remote_ahead ()
  in
  let res = run io in
  check "short_circuit: no action executed" (Option.is_none !executed);
  check "short_circuit: ancestry not probed" (not !ancestry_called);
  check "short_circuit: collision check skipped" (not !collision_called);
  let path_ok =
    match res with
    | Ok wt ->
        String.equal (Worktree.path wt)
          (Worktree.worktree_dir ~project_name ~patch_id)
    | Error _ -> false
  in
  check "short_circuit: returns the worktree_dir path" path_ok

(* On the non-short-circuit path the collision guard always runs before any
   action — its failure must be able to abort creation. *)
let test_collision_check_runs () =
  let io, _, _, collision_called = make_io ~refs:[] () in
  let (_ : (Worktree.t, SP.refusal) Result.t) = run io in
  check "collision_check_runs" !collision_called

let () =
  Stdlib.print_endline "Worktree.create_with_io wiring:";
  test_reset_when_remote_ahead ();
  test_reset_when_equal ();
  test_remote_only ();
  test_local_only ();
  test_brand_new ();
  test_preserve_local SP.Diverged;
  test_preserve_local SP.Local_ahead;
  test_unknown_ancestry_defers ();
  test_ref_probe_error ();
  test_short_circuit ();
  test_collision_check_runs ();
  Stdlib.print_endline "All create_with_io wiring cases passed."
