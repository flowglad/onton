(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
module B = Branch_reconcile
module G = Git_observation
module E = Branch_reconcile_executor
module Git = Onton_test_support.Git_env

let check message value = if not value then failwith message

let revision text =
  match B.Commit.make text with
  | Some revision -> revision
  | None -> failwith "invalid fixture revision"

let commit dir file =
  let oc = open_out (Filename.concat dir file) in
  output_string oc (file ^ "\n");
  close_out oc;
  Git.run_git ~cwd:dir [ "add"; file ];
  Git.run_git ~cwd:dir [ "commit"; "-qm"; file ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let run ?(mask = 0) env ~recorded ~side_merge =
  Git.with_temp_repo (fun remote ->
      let base = commit remote "base" in
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          Git.run_git ~cwd:dir [ "checkout"; "-qb"; "patch"; "origin/main" ];
          if side_merge then (
            Git.run_git ~cwd:dir [ "checkout"; "-qb"; "unrelated" ];
            ignore (commit dir "unrelated");
            Git.run_git ~cwd:dir [ "checkout"; "-q"; "patch" ]);
          let patch = commit dir "patch" in
          if side_merge then
            Git.run_git ~cwd:dir
              [ "merge"; "--no-ff"; "--no-commit"; "unrelated" ];
          if side_merge then ignore (commit dir "merge-only");
          let source = Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ] in
          if mask = 1 then
            let replacement =
              Git.git_capture ~cwd:dir
                [
                  "commit-tree";
                  patch ^ "^{tree}";
                  "-p";
                  base;
                  "-m";
                  "masked side merge";
                ]
            in
            Git.run_git ~cwd:dir [ "replace"; source; replacement ]
          else if mask = 2 then (
            let path = Filename.concat dir ".git/info/grafts" in
            let channel = open_out path in
            output_string channel (source ^ " " ^ patch ^ "\n");
            close_out channel);
          ignore (commit remote "upstream");
          let io =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let mutations = ref [] in
          let io =
            E.
              {
                git =
                  (fun args ->
                    (match args with
                    | ("rebase" | "merge" | "reset" | "push") :: _ ->
                        mutations := args :: !mutations
                    | _ -> ());
                    io.git args);
              }
          in
          let state =
            if recorded then
              fst
                (B.step B.empty (B.Materialized (B.New_branch (revision base))))
            else B.empty
          in
          let state, _ =
            B.step state
              (B.Request
                 { base = "main"; policy = Rewrite; purpose = Reconcile_base })
          in
          let rec drive remaining state effects =
            check "scope fixture converges" (remaining > 0);
            let recovering =
              Option.is_none (B.pending state)
              &&
              match B.operation state with
              | Some { repair = Some { mode = History_recovery _; _ }; _ } ->
                  true
              | Some
                  {
                    repair =
                      None | Some { mode = Diagnosis _ | Content_repair; _ };
                    _;
                  }
              | None ->
                  false
            in
            if recovering then (
              let token =
                List.find_map
                  (function
                    | B.Repair token -> Some token
                    | B.Execute _ | B.Start_repair _ | B.Completed _ -> None)
                  effects
                |> Option.get
              in
              let state = fst (B.step state (B.Repair_started token)) in
              let code, _, err =
                io.git [ "merge"; "--no-edit"; "origin/main" ]
              in
              check ("agent merge: " ^ err) (code = 0);
              let state, effects =
                B.step state (B.Repair_completed { token; at = 100. })
              in
              drive (remaining - 1) state effects)
            else
              match (B.operation state, B.pending state) with
              | Some operation, Some command ->
                  let result =
                    E.execute ~io ~prefix:"refs/onton/reconcile/scope"
                      ~branch:"patch" ~operation command
                  in
                  let next, effects =
                    B.step state
                      (B.Result { token = command.token; at = 100.; result })
                  in
                  (* Each command boundary is also a process-restart boundary. *)
                  let next =
                    match B.decode (B.yojson_of_t next) with
                    | Ok next -> next
                    | Error reason -> failwith reason
                  in
                  if B.publication_status next = `Published then next
                  else drive (remaining - 1) next effects
              | None, _ | Some _, None ->
                  failwith "scope fixture stopped unexpectedly"
          in
          let state = drive 15 state [] in
          check "Git ancestry allows adopted and merged histories to publish"
            (B.publication_status state = `Published);
          check "rebase publishes all branch contributions"
            (Git.git_capture ~cwd:remote
               [ "diff"; "--name-only"; "main...patch" ]
            = if side_merge then "merge-only\npatch\nunrelated" else "patch");
          check "original work remains recoverable"
            (List.mem (revision source) (B.required_revisions state));
          check "integration used Git"
            (List.exists
               (function "rebase" :: _ | "merge" :: _ -> true | _ -> false)
               !mutations);
          check "publication used Git"
            (List.exists
               (function "push" :: _ -> true | _ -> false)
               !mutations)))

let repair_scope env policy ~side_merge =
  Git.with_temp_repo (fun remote ->
      let base = commit remote "base" in
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          Git.run_git ~cwd:dir [ "checkout"; "-qb"; "patch"; "origin/main" ];
          let own_commit = commit dir "patch" in
          if side_merge then (
            Git.run_git ~cwd:dir [ "checkout"; "-qb"; "foreign"; "origin/main" ];
            ignore (commit dir "foreign");
            Git.run_git ~cwd:dir [ "checkout"; "-q"; "patch" ];
            Git.run_git ~cwd:dir
              [ "merge"; "--no-ff"; "--no-commit"; "foreign" ];
            ignore (commit remote "merge-only"));
          let target = commit remote "upstream" in
          let raw =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let pushes = ref 0 in
          let io =
            E.
              {
                git =
                  (fun args ->
                    (match args with "push" :: _ -> incr pushes | _ -> ());
                    raw.git args);
              }
          in
          let execute state =
            match (B.operation state, B.pending state) with
            | Some operation, Some command ->
                let result =
                  E.execute ~io ~prefix:"refs/onton/reconcile/repair-scope"
                    ~branch:"patch" ~operation command
                in
                B.step state
                  (B.Result { token = command.token; at = 100.; result })
            | None, _ | _, None -> failwith "missing repair-scope command"
          in
          let rec repair_turn count state effects =
            check "repair scope reaches bounded agent assistance" (count > 0);
            match
              List.find_map
                (function
                  | B.Repair token -> Some token
                  | B.Execute _ | B.Start_repair _ | B.Completed _ -> None)
                effects
            with
            | Some token -> (fst (B.step state (B.Repair_started token)), token)
            | None ->
                let next, effects = execute state in
                repair_turn (count - 1) next effects
          in
          let initial =
            fst (B.step B.empty (B.Materialized (B.New_branch (revision base))))
          in
          let initial, _ =
            B.step initial
              (B.Request { base = "main"; policy; purpose = Reconcile_base })
          in
          let initial, _ = execute initial in
          let failed, effects =
            match B.pending initial with
            | Some command ->
                B.step initial
                  (B.Result
                     {
                       token = command.token;
                       at = 100.;
                       result =
                         B.Recovery_required "fixture deterministic failure";
                     })
            | None -> failwith "missing pinned failure boundary"
          in
          let claimed, token = repair_turn 10 failed effects in
          (match (E.observe_checkout io).sequencer with
          | G.Rebase _ -> Git.run_git ~cwd:dir [ "rebase"; "--abort" ]
          | G.Merge _ -> Git.run_git ~cwd:dir [ "merge"; "--abort" ]
          | G.Cherry_pick _ -> Git.run_git ~cwd:dir [ "cherry-pick"; "--abort" ]
          | G.None_active -> ());
          (if side_merge then (
             Git.run_git ~cwd:dir [ "reset"; "--keep"; target ];
             Git.run_git ~cwd:dir [ "cherry-pick"; own_commit ])
           else
             match policy with
             | B.Rewrite ->
                 Git.run_git ~cwd:dir [ "rebase"; "--onto"; target; base ]
             | B.Preserve_ancestry ->
                 Git.run_git ~cwd:dir [ "merge"; "--no-edit"; target ]);
          let valid_head = Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ] in
          let rejected = commit dir "unrelated" in
          let state, effects =
            B.step claimed (B.Repair_completed { token; at = 100. })
          in
          ignore (effects : B.effect_command list);
          check "agent repair may change paths beyond deterministic prediction"
            (rejected <> valid_head);
          let rec settle count state =
            if B.publication_status state = `Published then state
            else (
              check "successful repair converges through exact-SHA publication"
                (count > 0);
              let next, _ = execute state in
              settle (count - 1) next)
          in
          let state = settle 15 state in
          check "successful agent candidate publishes once" (!pushes = 1);
          check "published repair includes repository-wide agent edits"
            (Git.git_capture ~cwd:remote
               [ "diff"; "--name-only"; "main...patch" ]
            = "patch\nunrelated");
          check
            "accepted commit remains recoverable after successful publication"
            (List.mem (revision rejected) (B.required_revisions state))))

let unfinished_work env ~advance =
  Git.with_temp_repo (fun remote ->
      let base = commit remote "base" in
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          Git.run_git ~cwd:dir [ "checkout"; "-qb"; "patch"; "origin/main" ];
          ignore (commit dir "patch");
          if advance then ignore (commit remote "upstream");
          let oc = open_out (Filename.concat dir "unfinished") in
          output_string oc "authorized interrupted work\n";
          close_out oc;
          let io =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let initial =
            fst (B.step B.empty (B.Materialized (B.New_branch (revision base))))
          in
          let initial, _ =
            B.step initial
              (B.Request
                 { base = "main"; policy = Rewrite; purpose = Reconcile_base })
          in
          let turns = ref 0 in
          let rec drive remaining state effects =
            check "unfinished work converges after scoped local extension"
              (remaining > 0);
            if B.publication_status state = `Published then state
            else
              match
                List.find_map
                  (function
                    | B.Repair token -> Some token
                    | B.Execute _ | B.Start_repair _ | B.Completed _ -> None)
                  effects
              with
              | Some token ->
                  incr turns;
                  check "only unfinished work needs an agent" (!turns = 1);
                  let state = fst (B.step state (B.Repair_started token)) in
                  Git.run_git ~cwd:dir [ "add"; "unfinished" ];
                  Git.run_git ~cwd:dir
                    [ "commit"; "-qm"; "finish interrupted work" ];
                  Git.run_git ~cwd:dir [ "merge"; "--no-edit"; "origin/main" ];
                  let state, effects =
                    B.step state (B.Repair_completed { token; at = 100. })
                  in
                  drive (remaining - 1) state effects
              | None -> (
                  match (B.operation state, B.pending state) with
                  | Some operation, Some command ->
                      let result =
                        E.execute ~io ~prefix:"refs/onton/reconcile/unfinished"
                          ~branch:"patch" ~operation command
                      in
                      let state, effects =
                        B.step state
                          (B.Result { token = command.token; at = 100.; result })
                      in
                      let state =
                        match B.decode (B.yojson_of_t state) with
                        | Ok s -> s
                        | Error reason -> failwith reason
                      in
                      drive (remaining - 1) state effects
                  | None, _ | _, None -> failwith "unfinished work stopped")
          in
          ignore (drive 35 initial []);
          check "interrupted work was committed before reconciliation"
            (!turns = if advance then 1 else 0);
          check "unfinished work and patch are the only published changes"
            (Git.git_capture ~cwd:remote
               [ "diff"; "--name-only"; "main...patch" ]
            = if advance then "patch\nunfinished" else "patch")))

let remote_scope env policy ~side_merge =
  Git.with_temp_repo (fun remote ->
      let base = commit remote "base" in
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          Git.run_git ~cwd:dir [ "checkout"; "-qb"; "patch"; "origin/main" ];
          ignore (commit dir "patch");
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "patch" ];
          ignore (commit dir "local");
          Git.run_git ~cwd:remote [ "checkout"; "-q"; "patch" ];
          ignore (commit remote "remote");
          if side_merge then (
            Git.run_git ~cwd:remote [ "checkout"; "-qb"; "foreign"; base ];
            ignore (commit remote "foreign");
            Git.run_git ~cwd:remote [ "checkout"; "-q"; "patch" ];
            Git.run_git ~cwd:remote
              [ "merge"; "--no-ff"; "--no-commit"; "foreign" ];
            ignore (commit remote "merge-only"));
          let incoming = Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ] in
          Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
          let raw =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let integrations = ref 0 and resets = ref 0 and repairs = ref 0 in
          let io =
            E.
              {
                git =
                  (fun args ->
                    (match args with
                    | ("merge" | "rebase" | "cherry-pick") :: _ ->
                        incr integrations
                    | "reset" :: _ -> incr resets
                    | _ -> ());
                    raw.git args);
              }
          in
          let initial =
            fst (B.step B.empty (B.Materialized (B.New_branch (revision base))))
          in
          let initial, _ =
            B.step initial
              (B.Request { base = "main"; policy; purpose = Reconcile_base })
          in
          let rec drive fuel state effects =
            check "remote scope terminates in publication or owned repair"
              (fuel > 0);
            if B.publication_status state = `Published then state
            else
              match
                List.find_map
                  (function
                    | B.Repair token -> Some token
                    | B.Execute _ | B.Start_repair _ | B.Completed _ -> None)
                  effects
              with
              | Some _ ->
                  incr repairs;
                  failwith "disjoint remote work unexpectedly requires recovery"
              | None -> (
                  match (B.operation state, B.pending state) with
                  | Some operation, Some command ->
                      let result =
                        E.execute ~io
                          ~prefix:"refs/onton/reconcile/remote-scope"
                          ~branch:"patch" ~operation command
                      in
                      let state, effects =
                        B.step state
                          (B.Result { token = command.token; at = 100.; result })
                      in
                      let state =
                        match B.decode (B.yojson_of_t state) with
                        | Ok state -> state
                        | Error reason -> failwith reason
                      in
                      drive (fuel - 1) state effects
                  | None, _ | _, None ->
                      failwith "remote scope lost its next action")
          in
          let state = drive 40 initial [] in
          check "remote replay never resets the managed checkout" (!resets = 0);
          check "incoming remote remains recoverable"
            (List.mem (revision incoming) (B.required_revisions state));
          check "captured merge inputs are preserved by ancestry policy"
            (if policy = B.Preserve_ancestry then
               Git.git_exit_code ~cwd:dir
                 [ "merge-base"; "--is-ancestor"; incoming; "HEAD" ]
               = 0
             else true);
          check
            "only the policy-authorized input contributions enter the PR diff"
            (Git.git_capture ~cwd:remote
               [ "diff"; "--name-only"; "main...patch" ]
            =
            if side_merge then "foreign\nlocal\nmerge-only\npatch\nremote"
            else "local\npatch\nremote");
          check "remote scope preserves normal deterministic progress"
            (!repairs = 0)))

let () =
  Eio_main.run (fun env ->
      run env ~recorded:false ~side_merge:false;
      run env ~recorded:true ~side_merge:true;
      run ~mask:1 env ~recorded:true ~side_merge:true;
      run ~mask:2 env ~recorded:true ~side_merge:true;
      run env ~recorded:true ~side_merge:false;
      repair_scope env B.Rewrite ~side_merge:false;
      repair_scope env B.Preserve_ancestry ~side_merge:false;
      repair_scope env B.Rewrite ~side_merge:true;
      unfinished_work env ~advance:false;
      unfinished_work env ~advance:true;
      remote_scope env B.Rewrite ~side_merge:false;
      remote_scope env B.Preserve_ancestry ~side_merge:false;
      remote_scope env B.Rewrite ~side_merge:true;
      remote_scope env B.Preserve_ancestry ~side_merge:true);
  print_endline "Git ancestry and agent recovery acceptance: OK"
