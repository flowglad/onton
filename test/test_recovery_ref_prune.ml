(* @archlint.module test
   @archlint.domain prune-decision *)

open Onton
open Onton_core
module B = Branch_reconcile
module E = Branch_reconcile_executor
module P = Recovery_ref_prune
module Git = Onton_test_support.Git_env

let check message condition =
  if not condition then QCheck2.Test.fail_report message

let get = function Ok value -> value | Error reason -> failwith reason

let sha text =
  match B.Commit.make text with
  | Some revision -> revision
  | None -> failwith "invalid fixture SHA"

let ref_name project suffix =
  B.recovery_prefix ~project ~branch:"patch" ^ "/" ^ suffix

let write path text =
  let channel = open_out_bin path in
  output_string channel text;
  close_out channel

let commit dir text =
  write (Filename.concat dir "file") text;
  Git.run_git ~cwd:dir [ "add"; "file" ];
  Git.run_git ~cwd:dir [ "commit"; "-qm"; text ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let exists dir name =
  Git.git_exit_code ~cwd:dir [ "show-ref"; "--verify"; "--quiet"; name ] = 0

exception Interrupted

let () =
  Eio_main.run @@ fun env ->
  let make_io path =
    E.make_io
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~clock:(Eio.Stdenv.clock env) ~path
  in
  List.iter
    (fun after ->
      List.iter
        (fun stop_at ->
          Git.with_temp_repo (fun dir ->
              let old = commit dir "retained dependency" in
              let newer = commit dir "newer source" in
              let io = make_io dir in
              let owned =
                List.init 3 (fun index ->
                    ref_name "retiring" (string_of_int index))
              in
              let protected = ref_name "dependent" "boundary" in
              List.iter
                (fun name -> Git.run_git ~cwd:dir [ "update-ref"; name; newer ])
                owned;
              check
                "dependent without an independent anchor retains all owned refs"
                (match
                   get
                     (P.reclaim ~io ~project:"retiring"
                        ~protected_projects:[ "dependent" ]
                        ~required:[ sha old ])
                 with
                | P.Retained _ -> List.for_all (exists dir) owned
                | P.Pruned -> false);
              Git.run_git ~cwd:dir [ "update-ref"; protected; old ];
              let attempted = ref 0 and deleted = ref 0 in
              let interrupted =
                E.
                  {
                    git =
                      (fun args ->
                        match args with
                        | [ "update-ref"; "--no-deref"; "-d"; name; expected ]
                          ->
                            incr attempted;
                            check "pruning deletes only captured owned tips"
                              (List.mem name owned && expected = newer);
                            if !attempted = stop_at && not after then
                              raise Interrupted;
                            let result = io.git args in
                            incr deleted;
                            if !attempted = stop_at && after then
                              raise Interrupted;
                            result
                        | _ -> io.git args);
                  }
              in
              (try
                 ignore
                   (P.reclaim ~io:interrupted ~project:"retiring"
                      ~protected_projects:[ "dependent" ]
                      ~required:[ sha old ]);
                 failwith "expected pruning boundary interruption"
               with Interrupted -> ());
              check "requested deletion boundary was reached"
                (!attempted = stop_at);
              check "only acknowledged Git deletions changed the namespace"
                (List.length (List.filter (exists dir) owned) = 3 - !deleted);
              check "dependent anchor survives every interruption"
                (Git.git_capture ~cwd:dir [ "rev-parse"; protected ] = old);
              check "restart completes eligible namespace cleanup"
                (get
                   (P.reclaim ~io ~project:"retiring"
                      ~protected_projects:[ "dependent" ]
                      ~required:[ sha old ])
                = P.Pruned);
              check "completed cleanup preserves dependent reachability"
                (exists dir protected
                && List.for_all (fun name -> not (exists dir name)) owned);
              let settled =
                E.
                  {
                    git =
                      (fun args ->
                        match args with
                        | "update-ref" :: _ ->
                            failwith "settled prune attempted another deletion"
                        | _ -> io.git args);
                  }
              in
              check "repeated cleanup is mutation-free"
                (get
                   (P.reclaim ~io:settled ~project:"retiring"
                      ~protected_projects:[ "dependent" ]
                      ~required:[ sha old ])
                = P.Pruned)))
        [ 1; 2; 3 ])
    [ false; true ];
  Git.with_temp_repo (fun dir ->
      let old = commit dir "old" in
      let newer = commit dir "new" in
      let io = make_io dir in
      let own = ref_name "a" "source" and foreign = ref_name "ab" "boundary" in
      Git.run_git ~cwd:dir [ "update-ref"; own; newer ];
      let legacy_owner =
        B.import_legacy_anchors ~revision:(`String old) B.empty `Null
      in
      let restored = get (B.decode (B.yojson_of_t legacy_owner)) in
      check
        "a migrated scalar legacy ancestor retains the project after restart"
        (match
           get
             (P.reclaim ~protected_projects:[ "ab" ] ~io ~project:"a"
                ~required:(B.required_revisions restored))
         with
        | P.Retained [ revision ] -> B.Commit.equal revision (sha old)
        | P.Pruned | P.Retained _ -> false);
      check "required ref remains" (exists dir own);
      Git.run_git ~cwd:dir [ "update-ref"; foreign; old ];
      check "unlocked or orphaned foreign refs do not authorize reclamation"
        (match
           get
             (P.reclaim ~protected_projects:[] ~io ~project:"a"
                ~required:[ sha old ])
         with
        | P.Retained _ -> exists dir own
        | P.Pruned -> false);
      check "independent recovery anchor permits reclamation"
        (get
           (P.reclaim ~protected_projects:[ "ab" ] ~io ~project:"a"
              ~required:[ sha old ])
        = P.Pruned);
      check "prefix-sharing foreign project remains"
        (exists dir foreign && not (exists dir own));
      Git.run_git ~cwd:dir [ "update-ref"; own; old ];
      let failed =
        E.
          {
            git =
              (fun args ->
                if
                  List.exists
                    (fun arg -> String.starts_with ~prefix:"--contains=" arg)
                    args
                then (1, "", "injected probe failure")
                else io.git args);
          }
      in
      check "failed reachability probe cannot authorize deletion"
        (Result.is_error
           (P.reclaim ~protected_projects:[ "ab" ] ~io:failed ~project:"a"
              ~required:[ sha old ])
        && exists dir own);
      let raced =
        E.
          {
            git =
              (fun args ->
                (match args with
                | "update-ref" :: "--no-deref" :: "-d" :: name :: _ ->
                    Git.run_git ~cwd:dir [ "update-ref"; name; newer ]
                | _ -> ());
                io.git args);
          }
      in
      check "captured tip change rejects deletion"
        (Result.is_error
           (P.reclaim ~protected_projects:[ "ab" ] ~io:raced ~project:"a"
              ~required:[])
        && exists dir own);
      check "raced revision remains"
        (Git.git_capture ~cwd:dir [ "rev-parse"; own ] = newer);
      let alias = ref_name "a" "alias" in
      Git.run_git ~cwd:dir [ "symbolic-ref"; alias; own ];
      check "symbolic inventory fails closed"
        (Result.is_error
           (P.reclaim ~protected_projects:[ "ab" ] ~io ~project:"a" ~required:[])
        && exists dir own);
      Git.run_git ~cwd:dir [ "symbolic-ref"; "--delete"; alias ];
      let blob = Git.git_capture ~cwd:dir [ "hash-object"; "-w"; "file" ] in
      Git.run_git ~cwd:dir [ "update-ref"; alias; blob ];
      check "unexpected non-commit recovery objects remain intact"
        (Result.is_error
           (P.reclaim ~protected_projects:[ "ab" ] ~io ~project:"a" ~required:[])
        && exists dir alias);
      Git.run_git ~cwd:dir [ "update-ref"; "-d"; alias; blob ];
      let second = ref_name "a" "second" in
      Git.run_git ~cwd:dir [ "update-ref"; second; old ];
      let deletes = ref 0 in
      let interrupted =
        E.
          {
            git =
              (fun args ->
                let result = io.git args in
                (match args with
                | "update-ref" :: "--no-deref" :: "-d" :: _ ->
                    incr deletes;
                    if !deletes = 1 then raise Interrupted
                | _ -> ());
                result);
          }
      in
      (try
         ignore
           (P.reclaim ~protected_projects:[ "ab" ] ~io:interrupted ~project:"a"
              ~required:[])
       with Interrupted -> ());
      check "interruption occurs after one guarded deletion" (!deletes = 1);
      check "restart reclaims remaining refs"
        (get
           (P.reclaim ~protected_projects:[ "ab" ] ~io ~project:"a" ~required:[])
        = P.Pruned);
      check "restart preserves unrelated namespace" (exists dir foreign);
      check "repeated reclamation settles"
        (get
           (P.reclaim ~protected_projects:[ "ab" ] ~io ~project:"a" ~required:[])
        = P.Pruned);
      Git.run_git ~cwd:dir [ "update-ref"; own; old ];
      let late = ref_name "a" "late" in
      let late_writer =
        E.
          {
            git =
              (fun args ->
                let result = io.git args in
                (match args with
                | "update-ref" :: "--no-deref" :: "-d" :: _ ->
                    Git.run_git ~cwd:dir [ "update-ref"; late; newer ]
                | _ -> ());
                result);
          }
      in
      check "late refs prevent reporting the namespace as reclaimed"
        (Result.is_error
           (P.reclaim ~protected_projects:[ "ab" ] ~io:late_writer ~project:"a"
              ~required:[])
        && exists dir late);
      check "late ref is reclaimed on a fresh observation"
        (get
           (P.reclaim ~protected_projects:[ "ab" ] ~io ~project:"a" ~required:[])
        = P.Pruned);
      let linked = Filename.concat dir "linked" in
      Git.run_git ~cwd:dir [ "worktree"; "add"; "--detach"; linked; "HEAD" ];
      check "linked worktrees share repository identity"
        (get (P.repository_id io) = get (P.repository_id (make_io linked))));
  (* Drive the actual project command with a persisted dependent checkpoint. *)
  Git.with_temp_repo (fun dir ->
      let old = commit dir "original" in
      let data = Filename.concat dir "projects" in
      Unix.mkdir data 0o700;
      let previous = Sys.getenv_opt "ONTON_DATA_DIR" in
      Unix.putenv "ONTON_DATA_DIR" data;
      Fun.protect
        ~finally:(fun () ->
          Unix.putenv "ONTON_DATA_DIR" (Option.value previous ~default:""))
        (fun () ->
          let save ?(repo_root = dir) project merged dependency =
            Project_store.save_config ~project_name:project
              ~github_owner:"owner" ~github_repo:"repo" ~backend:"codex"
              ~model:"test" ~main_branch:"main" ~poll_interval:5. ~repo_root
              ~max_concurrency:1 ~max_ci_failures:2 ~automerge_timeout:0. ();
            let gameplan =
              (get
                 (Gameplan_parser.parse_string
                    ("projectName: " ^ project
                   ^ "\n\
                      owner: owner\n\
                      repo: repo\n\
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
                .Gameplan_parser.gameplan
            in
            let runtime =
              Runtime.create ~gameplan
                ~main_branch:(Types.Branch.of_string "main")
                ()
            in
            let id = Types.Patch_id.of_string "1" in
            Runtime.update runtime (fun state ->
                let orchestrator =
                  if dependency then
                    fst
                      (Orchestrator.reconcile_branch state.Runtime.orchestrator
                         id
                         (B.Materialized (B.New_branch (sha old))))
                  else state.orchestrator
                in
                {
                  state with
                  orchestrator =
                    (if merged then Orchestrator.mark_merged orchestrator id
                     else orchestrator);
                });
            get
              (Persistence.save_snapshot
                 ~path:(Project_store.snapshot_path project)
                 (Runtime.read runtime Fun.id))
          in
          let run () =
            Prune_runner.run_prune ~net:(Eio.Stdenv.net env)
              ~clock:(Eio.Stdenv.clock env)
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~github_token:"" ~refresh:false ()
          in
          save "a-owner" true false;
          let registration =
            match Project_lifecycle.acquire_registration () with
            | Ok value -> value
            | Error error -> failwith (Project_lifecycle.error_message error)
          in
          let live_use =
            match
              Project_lifecycle.acquire_use registration ~project_name:"a-owner"
            with
            | Ok value -> value
            | Error error -> failwith (Project_lifecycle.error_message error)
          in
          Project_lifecycle.release_registration registration;
          check "live use without an exclusive supervisor lock prevents pruning"
            (run () = 0
            && Sys.file_exists (Project_store.snapshot_path "a-owner"));
          Project_lifecycle.release_use live_use;
          save "z-dependent" false true;
          let owned = ref_name "a-owner" "boundary" in
          Git.run_git ~cwd:dir [ "update-ref"; owned; old ];
          check "prune retains dependency without error" (run () = 0);
          check "dependent keeps project and ref"
            (Sys.file_exists (Project_store.snapshot_path "a-owner")
            && exists dir owned);
          save "z-dependent" true true;
          check "finished dependent releases project" (run () = 0);
          check "eligible project and private refs reclaimed"
            ((not (Sys.file_exists (Project_store.project_dir "a-owner")))
            && not (exists dir owned));
          save "a-owner" true false;
          save "z-dependent" false true;
          let independent = ref_name "z-dependent" "anchor" in
          Git.run_git ~cwd:dir [ "update-ref"; owned; old ];
          Git.run_git ~cwd:dir [ "update-ref"; independent; old ];
          let lifecycle = function
            | Ok value -> value
            | Error error -> failwith (Project_lifecycle.error_message error)
          in
          let registration =
            lifecycle (Project_lifecycle.acquire_registration ())
          in
          let peer =
            lifecycle
              (Project_lifecycle.acquire_use registration
                 ~project_name:"z-dependent")
          in
          Project_lifecycle.release_registration registration;
          check
            "active shared peer prevents relying on its mutable recovery \
             namespace"
            (run () = 0 && exists dir owned);
          Project_lifecycle.release_use peer;
          check "idle shared peer can supply a locked independent anchor"
            (run () = 0 && (not (exists dir owned)) && exists dir independent);
          save "z-dependent" true true;
          check "finished peer releases its namespace"
            (run () = 0 && not (exists dir independent));
          let managed = Project_store.managed_repo_dir "a-owner" in
          save ~repo_root:managed "a-owner" true false;
          Git.run_git ~cwd:dir
            [ "clone"; "--quiet"; "--no-hardlinks"; dir; managed ];
          save ~repo_root:managed "z-dependent" false true;
          check "managed repository sharing is retained"
            (run () = 0 && Sys.file_exists managed);
          save ~repo_root:managed "z-dependent" true true;
          check "completed aliases can be pruned" (run () = 0);
          check "managed repository is eventually reclaimed after aliases leave"
            (run () = 0 && not (Sys.file_exists managed))));
  print_endline "recovery ref pruning and dependent lifetimes: OK"
