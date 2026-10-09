(* @archlint.module test
   @archlint.domain session-driver *)
open Onton
open Onton_core
module B = Branch_reconcile
module F = Onton_core_test_support.Replay_fixture
module P = Onton_core_test_support.Publication_fixture

let get = function Ok x -> x | Error e -> failwith e
let check label ok = if not ok then failwith label
let id = Types.Patch_id.of_string "1"
let source = F.commit 10

let observation =
  F.observation ~source ~target:(F.commit 20)
    ~boundary:(B.Recorded (F.commit 1))
    ~noop:false

let repair_turn mode =
  let initial =
    F.step B.empty
      (B.Request { base = "main"; policy = Rewrite; purpose = Reconcile_base })
  in
  let state, result =
    match mode with
    | 0 -> (initial, B.Needs_diagnosis "environment")
    | 1 ->
        let state =
          F.reply initial (B.Observed observation) |> fun s ->
          F.reply s B.Pinned
        in
        (state, B.Conflict { head = source; sequencer = "step"; conflicts = 1 })
    | 2 | 3 ->
        let dirty = mode = 2 in
        let state =
          F.reply initial (B.Observed { observation with clean = not dirty })
        in
        let state =
          if dirty then state
          else
            F.reply state B.Pinned |> fun s ->
            F.reply s (B.Recovery_required "history")
        in
        ( state,
          B.Recovery_verified
            {
              observation = { observation with clean = not dirty };
              local_extension = None;
              source_preserved = false;
              remote_preserved = true;
            } )
    | _ ->
        let state =
          P.publishing
            ~candidate:(B.Commit.to_string source)
            ~step:F.step ~state:Fun.id B.empty
        in
        let state =
          F.reply state
            (B.Publication_rejected (Push_reject_classify.Hook_failure "gate"))
        in
        ( state,
          B.Recovery_verified
            {
              observation;
              local_extension = None;
              source_preserved = true;
              remote_preserved = true;
            } )
  in
  let command = Option.get (B.pending state) in
  let state, effects =
    B.step state (B.Result { token = command.token; at = 100.; result })
  in
  let token =
    List.find_map
      (function
        | B.Repair t -> Some t
        | B.Execute _ | B.Start_repair _ | B.Completed _ -> None)
      effects
    |> Option.get
  in
  let state = F.step state (B.Repair_started token) in
  Option.get (B.repair_turn state ~branch:"patch" token)

let run env root ~existing mode =
  let project_name = Printf.sprintf "repair-%b-%d" existing mode in
  let gameplan =
    (get
       (Gameplan_parser.parse_string
          ("projectName: " ^ project_name
         ^ "\n\
            owner: test\n\
            repo: test\n\
            problemStatement: test\n\
            solutionSummary: test\n\
            patches:\n\
           \  - number: 1\n\
           \    title: patch\n\
           \    description: patch\n\
           \    dependsOn: []\n\
            dependencyGraph:\n\
           \  - patch: 1\n\
           \    dependsOn: []\n")))
      .gameplan
  in
  let runtime =
    Runtime.create ~gameplan ~main_branch:(Types.Branch.of_string "main") ()
  in
  Runtime.update_orchestrator runtime (fun orch ->
      let orch =
        Orchestrator.fire orch
          (Orchestrator.Start (id, Types.Branch.of_string "main"))
      in
      let orch =
        if existing then Orchestrator.set_session_failed orch id else orch
      in
      Orchestrator.set_llm_session_id orch id
        (if existing then Some "existing-patch-thread" else None));
  let module Env = struct
    let runtime = runtime
    let clock = Eio.Stdenv.clock env
    let fs = Eio.Stdenv.fs env
    let project_name = project_name
    let owner = "test"
    let repo = "test"
    let transcripts = Hashtbl.create 1
    let transcript_updates = Hashtbl.create 1
    let event_log = Event_log.create ~path:"/dev/null"
    let user_config = { User_config.on_worktree_create = None }
    let worktree_mutex = Eio.Mutex.create ()
    let hook_mutex = Eio.Mutex.create ()
    let fetch_mutex = Eio.Mutex.create ()
  end in
  let module W =
    (val Worktree.make ~config:Worktree_lifecycle.git ~fs:Env.fs
           ~clock:Env.clock
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~repo_root:root)
  in
  let module SD = Session_driver.Make (W) (Env) in
  Hashtbl.replace Env.transcripts id "Existing implementation transcript";
  Project_store.ensure_dir (Project_store.project_dir project_name);
  let snapshot_path = Project_store.snapshot_path project_name in
  get (Persistence.save ~path:snapshot_path (Runtime.read runtime Fun.id));
  let conversation =
    ref (if existing then Some "existing-patch-thread" else None)
  in
  let turn = repair_turn mode in
  let actual =
    match turn.B.mode with
    | B.Diagnosis _ -> 0
    | B.Content_repair -> 1
    | B.History_recovery { task = Finish_local_work; _ } -> 2
    | B.History_recovery { task = Reconstruct_history; _ } -> 3
    | B.History_recovery { task = Repair_publication; _ } -> 4
  in
  check "fixture covers the intended fallback mode" (actual = mode);
  let calls = ref 0 in
  let backend =
    Llm_backend.
      {
        name = "fixture";
        run_streaming =
          (fun ~project_name:_
            ~cwd:_
            ~patch_id:_
            ~prompt
            ~resume_session
            ~session_uuid
            ~complexity:_
            ~on_event
          ->
            incr calls;
            check "repair resumes the current patch conversation"
              (resume_session = !conversation);
            (match !conversation with
            | Some id ->
                check "repair reuses the conversation UUID" (session_uuid = id)
            | None -> conversation := Some session_uuid);
            check "repair context is delivered"
              (Base.String.is_substring prompt ~substring:"repair context");
            on_event
              (Types.Stream_event.Session_init
                 {
                   session_id =
                     (if !calls = 4 then "foreign-thread"
                      else Option.get !conversation);
                   api_key_source = None;
                   model = None;
                   claude_code_version = None;
                   permission_mode = None;
                 });
            on_event Turn_started;
            on_event (Text_delta (Printf.sprintf "repair text %d" !calls));
            on_event
              (Tool_use
                 {
                   name = "Bash";
                   input = "repair command";
                   status = Some "completed";
                 });
            on_event (Error "visible warning");
            if !calls = 2 then failwith "backend transport failed";
            if !calls = 5 then
              raise (Eio.Cancel.Cancelled (Failure "cancelled"));
            on_event
              (Final_result { text = "repair done"; stop_reason = End_turn });
            {
              exit_code = 0;
              stdout = "";
              stderr = "";
              got_events = true;
              saw_final_result = true;
              timed_out = false;
            });
      }
  in
  for attempt = 1 to 5 do
    let agent =
      Runtime.read runtime (fun snap -> Orchestrator.agent snap.orchestrator id)
    in
    let before = agent.branch_reconcile in
    (try
       ignore
         (SD.run_repair ~patch_id:id ~agent ~backend ~complexity:None
            ~cwd:Env.fs ~context:"repair context" ~guidance:[ "human guidance" ]
            ~turn ~read_head:(fun () -> Some (B.Commit.to_string source))
           : B.event);
       check "cancellation must propagate" (attempt <> 5)
     with Eio.Cancel.Cancelled _ ->
       check "only cancellation propagates" (attempt = 5));
    let after =
      Runtime.read runtime (fun snap -> Orchestrator.agent snap.orchestrator id)
    in
    check "repair never completes or publishes the implementation session"
      (B.equal before after.branch_reconcile);
    check "patch session identity survives repair"
      (after.llm_session_id = !conversation);
    check "successful repair leaves ordinary turns resumable"
      (SD.session_mode after = `Resume (Option.get !conversation));
    let text = Hashtbl.find Env.transcripts id in
    List.iter
      (fun expected ->
        check
          ("transcript retains " ^ expected)
          (Base.String.is_substring text ~substring:expected))
      [
        "Existing implementation transcript";
        "human guidance";
        "repair text 1";
        "repair command";
        "visible warning";
        Printf.sprintf "repair text %d" (if attempt = 4 then 3 else attempt);
      ];
    if attempt >= 2 then
      check "backend exceptions remain visible"
        (Base.String.is_substring text ~substring:"backend transport failed");
    check "repair publishes the normal transcript update"
      (Hashtbl.find Env.transcript_updates id = text);
    let saved = get (Persistence.load ~path:snapshot_path) in
    let saved_text = Base.Hashtbl.find saved.transcripts id |> Option.get in
    check "completed repair transcript is checkpointed"
      (Base.String.is_substring saved_text ~substring:"repair text 1");
    check "committed session identity survives restart"
      ((Orchestrator.agent saved.orchestrator id).llm_session_id = !conversation);
    Runtime.update_orchestrator runtime (fun _ -> saved.orchestrator)
  done;
  check "one backend invocation per repair, no fresh retry" (!calls = 5)

let () =
  let root = Filename.temp_dir "repair-session-test" "" in
  check "fixture repository initializes"
    (Sys.command ("git init -q " ^ Filename.quote root) = 0);
  let previous = Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "ONTON_DATA_DIR" root;
  Fun.protect
    ~finally:(fun () ->
      (match previous with
      | Some v -> Unix.putenv "ONTON_DATA_DIR" v
      | None -> Unix.unsetenv "ONTON_DATA_DIR");
      ignore (Sys.command ("rm -rf " ^ Filename.quote root)))
    (fun () ->
      Eio_main.run (fun env ->
          List.iter
            (fun existing ->
              List.iter (run env root ~existing) [ 0; 1; 2; 3; 4 ])
            [ false; true ]));
  print_endline
    "repair turns resume patch conversations and retain transcripts: OK"
