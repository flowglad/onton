(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_test_support.Branch_reconcile_git_fixture

[@@@warning "-4-40-42"]

let () =
  timed ~name:"workspace-recovery-separate-clone" (fun () ->
      Eio_main.run @@ fun env ->
      Git.with_temp_repo @@ fun fetch_remote ->
      let base = commit fetch_remote "base" "base\n" "base" in
      let old_patch = ref None in
      Git.with_temp_repo (fun old ->
          prepare fetch_remote old;
          let patch = commit old "work" "work\n" "work" in
          Git.run_git ~cwd:old [ "push"; "-q"; "origin"; "patch" ];
          old_patch := Some patch);
      (* The original clone and its object database have been removed. The new
         clone has neither a patch ref nor Onton's recovery refs. *)
      Git.with_temp_repo @@ fun fresh ->
      Git.run_git ~cwd:fresh [ "remote"; "add"; "origin"; fetch_remote ];
      let io =
        E.make_io
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~path:fresh
      in
      let requested, _ = B.step B.empty (B.Request intent) in
      let requested =
        match (B.pending requested, !old_patch) with
        | Some command, Some patch ->
            fst
              (B.step requested
                 (B.Result
                    {
                      token = command.token;
                      at = 0.;
                      result =
                        B.Observed
                          {
                            destination =
                              B.Remote_id.of_destination fetch_remote;
                            head = sha patch;
                            source = sha patch;
                            target = sha base;
                            remote = Some (sha patch);
                            boundary = B.Plain;
                            topology = B.Equal;
                            clean = true;
                            sequencer = None;
                            conflicts = 0;
                            target_included = false;
                            base_contains_source = false;
                            completed_integration = false;
                          };
                    }))
        | _ -> failwith "old observation unavailable"
      in
      let lost, effects =
        B.step requested (B.Workspace_lost { request = "fresh-attempt" })
      in
      let command =
        match effects with
        | [ B.Execute command ] -> command
        | _ -> failwith "missing remote observation command"
      in
      let operation =
        match B.operation lost with
        | Some operation -> operation
        | None -> failwith "missing recovery operation"
      in
      let result = execute io ~operation command in
      (match result with
      | B.Workspace_observed
          { push_destination = _; patch_revision; base_revision } ->
          check "observed pushed patch in replacement"
            (patch_revision = Option.map sha !old_patch);
          check "observed base from fetch destination"
            (base_revision = Some (sha base))
      | _ -> failwith "fresh remote observation failed");
      let adopted, _ =
        B.step lost (B.Result { token = command.token; at = 1.; result })
      in
      check "pushed branch adopted"
        (match B.workspace_outcome adopted with
        | Some (B.Workspace_patch (patch, observed_base)) ->
            Some patch = Option.map sha !old_patch
            && B.Commit.equal observed_base (sha base)
        | _ -> false);
      let runtime = make_runtime () in
      Runtime.update_orchestrator runtime (fun orchestrator ->
          fst (Orchestrator.reconcile_branch orchestrator id (B.Request intent)));
      check "runner checkpoints loss and remote observation"
        (run runtime
           (fun _ -> Ok ())
           io
           (B.Workspace_lost { request = "runner-attempt" })
        = R.Idle);
      check "runner reaches owner recovery outcome"
        (match B.workspace_outcome (state runtime) with
        | Some (B.Workspace_patch (patch, observed_base)) ->
            Some patch = Option.map sha !old_patch
            && B.Commit.equal observed_base (sha base)
        | _ -> false);
      let blocked = make_runtime () in
      Runtime.update_orchestrator blocked (fun orchestrator ->
          fst (Orchestrator.reconcile_branch orchestrator id (B.Request intent)));
      let reads = ref 0 in
      let counting =
        E.
          {
            git =
              (fun args ->
                incr reads;
                io.git args);
          }
      in
      check "rejected owner checkpoint blocks the remote probe"
        (match
           run blocked
             (fun _ -> Error "rejected")
             counting
             (B.Workspace_lost { request = "rejected-attempt" })
         with
        | R.Checkpoint_failed _ -> !reads = 0
        | _ -> false);
      List.iter
        (fun (label, override) ->
          let faulty = E.{ git = (fun args -> override args io.git) } in
          let failed = execute faulty ~operation command in
          check label (match failed with B.Retryable _ -> true | _ -> false))
        [
          ( "failed remote probe waits",
            fun args git ->
              match args with
              | "ls-remote" :: _ -> (128, "", "unavailable")
              | _ -> git args );
          ( "malformed remote probe waits",
            fun args git ->
              match args with
              | "ls-remote" :: _ -> (0, "not-a-sha refs/heads/patch\n", "")
              | _ -> git args );
          ( "missing fetched object waits",
            fun args git ->
              match args with
              | "fetch" :: _ -> (128, "", "object unavailable")
              | _ -> git args );
        ];
      Git.run_git ~cwd:fresh [ "push"; "-q"; "origin"; "--delete"; "patch" ];
      let never_published, _ = B.step B.empty (B.Request intent) in
      let never_published, commands =
        B.step never_published (B.Workspace_lost { request = "new-patch" })
      in
      let absent_command =
        match commands with
        | [ B.Execute command ] -> command
        | _ -> failwith "missing absent-branch probe"
      in
      let absent_operation =
        match B.operation never_published with
        | Some operation -> operation
        | None -> failwith "missing absent-branch operation"
      in
      let absent_result =
        execute io ~operation:absent_operation absent_command
      in
      let ready, _ =
        B.step never_published
          (B.Result
             { token = absent_command.token; at = 2.; result = absent_result })
      in
      check "never-published absence uses confirmed current base"
        (match B.workspace_outcome ready with
        | Some (B.Workspace_base observed_base) ->
            B.Commit.equal observed_base (sha base)
        | _ -> false);
      (* The owner binding rejects a changed push destination before probing
         or treating a missing branch as authority to recreate work. *)
      Git.with_temp_repo (fun push ->
          Git.run_git ~cwd:fresh
            [ "remote"; "set-url"; "--push"; "origin"; push ];
          let changed = execute io ~operation command in
          check "bound push destination rejects replacement"
            (match changed with B.Needs_diagnosis _ -> true | _ -> false)))
