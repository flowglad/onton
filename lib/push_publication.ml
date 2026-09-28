(* @archlint.module shell
   @archlint.domain orchestrator *)

(* Publish the intended head before yielding to git. A poll can observe the
   pushed head while the push is still running; completion must not re-arm the
   marker after that poll has settled it. *)
let with_pending_push ~update ~patch_id ~local_sha push =
  let published = ref false in
  Fun.protect
    ~finally:(fun () ->
      if not !published then
        Eio.Cancel.protect (fun () ->
            update (fun orch ->
                let agent = Orchestrator.agent orch patch_id in
                if
                  Base.Option.equal String.equal
                    agent.Patch_agent.expected_remote_head_oid local_sha
                then
                  Orchestrator.set_expected_remote_head_oid orch patch_id None
                else orch)))
    (fun () ->
      update (fun orch ->
          Orchestrator.set_expected_remote_head_oid orch patch_id local_sha);
      let result = push () in
      (published :=
         match result with
         | Worktree.Push_ok -> true
         | Worktree.Push_up_to_date | Worktree.Push_no_commits
         | Worktree.Push_rejected _ | Worktree.Push_worktree_missing
         | Worktree.Push_error _ ->
             false);
      result)
