(* @archlint.module test
   @archlint.domain session-driver *)

open Onton
open Onton_core
module Git = Onton_test_support.Git_env
module B = Branch_reconcile

[@@@warning "-4-42"]

let check label condition = if not condition then failwith label
let contains text part = Base.String.is_substring text ~substring:part

let () =
  Eio_main.run @@ fun env ->
  let sandbox = Stdlib.Filename.temp_dir "onton-recovery-prompt-" "" in
  let old_home = Stdlib.Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" sandbox;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      Unix.putenv "HOME"
        (Option.value old_home ~default:(Stdlib.Filename.get_temp_dir_name ()));
      Git.sh ~dir:"/" ("rm -rf " ^ Stdlib.Filename.quote sandbox))
    (fun () ->
      let seed = Stdlib.Filename.concat sandbox "seed" in
      Unix.mkdir seed 0o755;
      Git.init_repo seed;
      ignore
        (Onton_test_support.Branch_reconcile_git_fixture.commit seed "base"
           "base\n" "base");
      let origin = Stdlib.Filename.concat sandbox "origin.git" in
      Git.sh ~dir:"/"
        (Printf.sprintf "git clone --bare -q %s %s"
           (Stdlib.Filename.quote seed)
           (Stdlib.Filename.quote origin));
      let fresh = Stdlib.Filename.concat sandbox "fresh" in
      Git.sh ~dir:"/"
        (Printf.sprintf "git clone --no-local -q %s %s"
           (Stdlib.Filename.quote origin)
           (Stdlib.Filename.quote fresh));
      let runtime =
        Onton_test_support.Branch_reconcile_git_fixture.make_runtime ()
      in
      let patch_id = Onton_test_support.Branch_reconcile_git_fixture.id in
      let module Real =
        (val Worktree.make ~fs:(Eio.Stdenv.fs env)
               ~config:Worktree_lifecycle.git ~clock:(Eio.Stdenv.clock env)
               ~process_mgr:(Eio.Stdenv.process_mgr env)
               ~repo_root:fresh)
      in
      let module Env = struct
        let runtime = runtime
        let fs = Eio.Stdenv.fs env
        let clock = Eio.Stdenv.clock env
        let worktree_mutex = Eio.Mutex.create ()
        let hook_mutex = Eio.Mutex.create ()
        let fetch_mutex = Eio.Mutex.create ()
        let project_name = "worker-recovery-prompt"
        let user_config = { User_config.on_worktree_create = None }
        let owner = "owner"
        let repo = "repo"
        let transcripts = Stdlib.Hashtbl.create 1
        let transcript_updates = Stdlib.Hashtbl.create 1

        let event_log =
          Event_log.create ~path:(Stdlib.Filename.concat sandbox "events.jsonl")
      end in
      let module C = struct
        let persist _ = Ok ()
        let record_session_id ~patch_id:_ ~session_id:_ = Ok ()
      end in
      let module SD = Session_driver.Make_with_checkpoint (C) (Real) (Env) in
      let agent =
        Runtime.read runtime (fun snap ->
            Orchestrator.agent snap.Runtime.orchestrator patch_id)
      in
      let emit event =
        Runtime.update_orchestrator runtime (fun orch ->
            fst (Orchestrator.reconcile_branch orch patch_id event))
      in
      let pending () =
        Runtime.read runtime (fun snap ->
            B.pending
              (Orchestrator.agent snap.Runtime.orchestrator patch_id)
                .Patch_agent.branch_reconcile)
      in
      let reply result =
        match pending () with
        | Some command ->
            emit (B.Result { token = command.B.token; at = 1.; result })
        | None -> failwith "missing publication setup command"
      in
      let base = Git.git_capture ~cwd:fresh [ "rev-parse"; "HEAD" ] in
      let sha =
        match B.Commit.make base with
        | Some sha -> sha
        | None -> failwith "invalid base revision"
      in
      Runtime.update_orchestrator runtime (fun orch ->
          let orch =
            Orchestrator.set_llm_session_id orch patch_id (Some "old-thread")
          in
          fst
            (Orchestrator.reconcile_branch orch patch_id
               (B.Request
                  B.
                    {
                      base = "main";
                      policy = Rewrite;
                      purpose = Reconcile_base;
                    })));
      reply
        (B.Observed
           B.
             {
               destination = Remote_id.of_destination origin;
               head = sha;
               source = sha;
               target = sha;
               remote = Some sha;
               boundary = Plain;
               topology = Equal;
               clean = true;
               sequencer = None;
               conflicts = 0;
               target_included = false;
               base_contains_source = false;
               completed_integration = false;
             });
      reply B.Pinned;
      reply (B.Integrated sha);
      let calls = ref [] in
      let backend =
        Llm_backend.
          {
            name = "capture";
            run_streaming =
              (fun ~project_name:_
                ~cwd:_
                ~patch_id:_
                ~prompt
                ~resume_session
                ~session_uuid:_
                ~complexity:_
                ~on_event:_
              ->
                calls := (prompt, resume_session) :: !calls;
                {
                  exit_code = 1;
                  stdout = "";
                  stderr = "simulated backend failure";
                  got_events = false;
                  saw_final_result = false;
                  timed_out = false;
                });
          }
      in
      let recovery =
        Session_prompt.create_recovery ~requirements:"REQ unique"
          ~operation:"OP unique" ~guidance:[ "GUIDE unique" ]
          ~patch_revision:Session_prompt.Absent ~base_revision:"BASE unique"
      in
      let prompt =
        SD.create_prompt
          ~context:(fun ~worktree_path:_ -> "old context")
          ~turn:"TURN unique"
      in
      let recover ?observe_forge ~request ~renewal_id () =
        Runtime.with_patch_ownership runtime ~patch_id (fun write_owner ->
            SD.run_recovery_owned ~write_owner ~request ~renewal_id
              ?observe_forge ~recovery ~kind:None
              ~delivery_mode:Patch_decision.Start ~patch_id ~prompt
              ~on_pr_detected:(fun _ -> ())
              ~backend ~complexity:None ())
      in
      (match
         recover ~request:"prompt-loss" ~renewal_id:None
           ~observe_forge:(fun () -> Error "forge unavailable")
           ()
       with
      | Session_driver.Recovery_wait "forge unavailable" -> ()
      | _ -> failwith "failed forge read did not wait");
      check "wait did not invoke backend" (!calls = []);
      let observe_forge () = Ok (B.Forge_not_merged, None) in
      let attention =
        recover ~request:"plain-retry" ~renewal_id:None ~observe_forge ()
      in
      let renewal_id =
        match attention with
        | Session_driver.Recovery_attention { renewal_id; _ } -> renewal_id
        | _ -> failwith "published disappearance did not require renewal"
      in
      check "attention did not invoke backend" (!calls = []);
      (match
         recover ~request:"wrong-renewal" ~renewal_id:(Some "wrong")
           ~observe_forge ()
       with
      | Session_driver.Recovery_attention _ -> ()
      | _ -> failwith "wrong renewal granted execution");
      check "wrong renewal did not invoke backend" (!calls = []);
      let result =
        recover ~request:"renewed-operation" ~renewal_id:(Some renewal_id)
          ~observe_forge ()
      in
      (match result with
      | Session_driver.Recovery_session _ -> ()
      | Session_driver.Recovery_wait reason ->
          failwith ("fresh backend not called: " ^ reason)
      | Session_driver.Recovery_attention _ -> failwith "unexpected attention"
      | Session_driver.Recovery_merged _ -> failwith "unexpected merge");
      (match !calls with
      | [ (delivered, None) ] ->
          List.iter
            (fun part -> check ("missing " ^ part) (contains delivered part))
            [
              "REQ unique";
              "OP unique";
              "GUIDE unique";
              "BASE unique";
              "TURN unique";
              "previous workspace and conversation are unavailable";
              "Inspect the current checkout";
              "Validate the resulting work";
            ];
          check "stale context excluded"
            (not (contains delivered "old context"))
      | _ -> failwith "backend did not receive one fresh invocation");
      let calls_before = List.length !calls in
      (match
         recover ~request:"same-renewal-reused" ~renewal_id:(Some renewal_id)
           ~observe_forge ()
       with
      | Session_driver.Recovery_session _ ->
          failwith "consumed renewal authorized another backend turn"
      | Session_driver.Recovery_wait _ | Session_driver.Recovery_attention _
      | Session_driver.Recovery_merged _ ->
          ());
      check "consumed renewal did not invoke backend"
        (List.length !calls = calls_before);
      Git.run_git ~cwd:fresh
        [
          "push";
          "-q";
          "origin";
          "HEAD:refs/heads/" ^ Types.Branch.to_string agent.Patch_agent.branch;
        ];
      Git.run_git ~cwd:fresh
        [
          "push";
          "-q";
          "origin";
          "--delete";
          Types.Branch.to_string agent.Patch_agent.branch;
        ];
      let runtime2 =
        Onton_test_support.Branch_reconcile_git_fixture.make_runtime ()
      in
      let module Env2 = struct
        include Env

        let runtime = runtime2
        let project_name = "worker-recovery-merged"
      end in
      let module SD2 = Session_driver.Make_with_checkpoint (C) (Real) (Env2) in
      let emit2 event =
        Runtime.update_orchestrator runtime2 (fun orch ->
            fst (Orchestrator.reconcile_branch orch patch_id event))
      in
      let reply2 result =
        let command =
          Runtime.read runtime2 (fun snap ->
              B.pending
                (Orchestrator.agent snap.Runtime.orchestrator patch_id)
                  .Patch_agent.branch_reconcile)
        in
        match command with
        | Some command ->
            emit2 (B.Result { token = command.B.token; at = 1.; result })
        | None -> failwith "missing merged setup command"
      in
      emit2
        (B.Request
           B.{ base = "main"; policy = Rewrite; purpose = Reconcile_base });
      reply2
        (B.Observed
           B.
             {
               destination = Remote_id.of_destination origin;
               head = sha;
               source = sha;
               target = sha;
               remote = Some sha;
               boundary = Plain;
               topology = Equal;
               clean = true;
               sequencer = None;
               conflicts = 0;
               target_included = false;
               base_contains_source = false;
               completed_integration = false;
             });
      reply2 B.Pinned;
      reply2 (B.Integrated sha);
      let merged =
        Runtime.with_patch_ownership runtime2 ~patch_id (fun write_owner ->
            SD2.run_recovery_owned ~write_owner ~request:"merged-loss"
              ~renewal_id:None
              ~observe_forge:(fun () -> Ok (B.Forge_merged, None))
              ~recovery ~kind:None ~delivery_mode:Patch_decision.Start ~patch_id
              ~prompt
              ~on_pr_detected:(fun _ -> ())
              ~backend ~complexity:None ())
      in
      (match merged with
      | Session_driver.Recovery_merged None -> ()
      | Session_driver.Recovery_merged (Some _) ->
          failwith "unexpected PR number"
      | Session_driver.Recovery_session _ ->
          failwith "confirmed merge started backend"
      | Session_driver.Recovery_wait reason ->
          failwith ("confirmed merge waited: " ^ reason)
      | Session_driver.Recovery_attention _ ->
          failwith "confirmed merge requested renewal");
      check "merge did not invoke backend" (List.length !calls = calls_before);
      Runtime.update_orchestrator runtime (fun orch ->
          Orchestrator.set_llm_session_id
            (Orchestrator.clear_session_fallback orch patch_id)
            patch_id (Some "warm-thread"));
      let warm_agent =
        Runtime.read runtime (fun snap ->
            Orchestrator.agent snap.Runtime.orchestrator patch_id)
      in
      check "warm agent can resume"
        (SD.session_mode warm_agent = `Resume "warm-thread");
      ignore
        (Runtime.with_patch_ownership runtime ~patch_id (fun write_owner ->
             SD.run_owned ~write_owner ~kind:None
               ~delivery_mode:Patch_decision.Start ~patch_id ~prompt
               ~agent:warm_agent
               ~on_pr_detected:(fun _ -> ())
               ~backend ~complexity:None));
      match !calls with
      | (_, Some "warm-thread") :: _ -> ()
      | _ -> failwith "warm session did not resume existing conversation")
