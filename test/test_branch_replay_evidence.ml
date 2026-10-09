(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
module B = Branch_reconcile
module E = Branch_reconcile_executor
module Git = Onton_test_support.Git_env

let check label condition = if not condition then failwith label

let sha text =
  match B.Commit.make text with Some v -> v | None -> assert false

let commit dir file message =
  let oc = open_out (Filename.concat dir file) in
  output_string oc (file ^ "\n");
  close_out oc;
  Git.run_git ~cwd:dir [ "add"; file ];
  Git.run_git ~cwd:dir [ "commit"; "-q"; "-m"; message ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let evidence = function
  | B.Observed o -> (Some o, false)
  | B.Retryable _ -> (None, true)
  | B.Checkout_ready | B.Observed_recovery _ | B.Observed_active _ | Pinned
  | Merge_completion_needed _ | Merge_completed _ | Remote_replay_selected _
  | Remote_checked_out | Integrated _ | Conflict _ | Recovery_verified _
  | Inspected _ | Inspected_active _ | Published | Remote _
  | Recovery_required _ | Publication_rejected _ | Attempt_failed _
  | Needs_diagnosis _ ->
      (None, false)

let plain_replay env merged_history =
  Git.with_temp_repo (fun remote ->
      ignore (commit remote "base" "base");
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          Git.run_git ~cwd:dir
            [ "checkout"; "-q"; "-b"; "patch"; "origin/main" ];
          if merged_history then (
            Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "side" ];
            ignore (commit dir "side" "side contribution");
            Git.run_git ~cwd:dir [ "checkout"; "-q"; "patch" ]);
          ignore (commit dir "patch" "patch contribution");
          if merged_history then
            Git.run_git ~cwd:dir [ "merge"; "--no-ff"; "--no-edit"; "side" ];
          let source = Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ] in
          let target = commit remote "base-advance" "advance" in
          let io =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let initial, _ =
            B.step B.empty
              (B.Request
                 { base = "main"; policy = Rewrite; purpose = Reconcile_base })
          in
          let rec settle state remaining =
            if B.publication_status state = `Published then state
            else (
              check "plain replay converges" (remaining > 0);
              match (B.operation state, B.pending state) with
              | Some operation, Some command ->
                  let result =
                    E.execute ~io ~prefix:"refs/onton/reconcile/plain-replay"
                      ~branch:"patch" ~operation command
                  in
                  (match result with
                  | B.Observed observed ->
                      check "unproven replay remains labelled plain"
                        (observed.boundary = B.Plain)
                  | B.Checkout_ready | B.Observed_recovery _
                  | B.Observed_active _ | B.Pinned | B.Merge_completion_needed _
                  | B.Merge_completed _ | B.Remote_replay_selected _
                  | B.Remote_checked_out | B.Integrated _ | B.Conflict _
                  | B.Recovery_verified _ | B.Inspected _ | B.Inspected_active _
                  | B.Published | B.Remote _ | B.Retryable _
                  | B.Recovery_required _ | B.Publication_rejected _
                  | B.Attempt_failed _ | B.Needs_diagnosis _ ->
                      ());
                  let next, _ =
                    B.step state
                      (B.Result { token = command.token; at = 100.; result })
                  in
                  let restored =
                    match B.decode (B.yojson_of_t next) with
                    | Ok restored -> restored
                    | Error reason -> failwith reason
                  in
                  settle restored (remaining - 1)
              | None, _ | _, None ->
                  failwith "plain replay stopped unexpectedly")
          in
          let settled = settle initial 12 in
          check "plain provenance survives checkpoint and publication"
            (match B.operation settled with
            | Some operation -> operation.boundary = B.Plain
            | None -> false);
          List.iter
            (fun file ->
              check
                ("plain replay preserves " ^ file)
                (Git.git_capture ~cwd:remote [ "show"; "patch:" ^ file ] = file))
            ([ "base"; "base-advance"; "patch" ]
            @ if merged_history then [ "side" ] else []);
          check "plain replay includes captured target"
            (Git.git_exit_code ~cwd:remote
               [ "merge-base"; "--is-ancestor"; target; "patch" ]
            = 0);
          check "plain replay rewrites source"
            (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ] <> source)))

let deep_stack env empty_tip =
  Git.with_temp_repo (fun remote ->
      ignore (commit remote "base" "base");
      ignore (commit remote "conflict" "original shared file");
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "--detach"; "origin/main" ];
          let branch index = "stack-" ^ string_of_int (index + 1) in
          let states = Array.make 4 B.empty in
          for index = 0 to 3 do
            let boundary = Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ] in
            Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; branch index ];
            (if not (empty_tip && index = 3) then
               let path = Filename.concat dir "stack" in
               List.iter
                 (fun subject ->
                   let channel =
                     open_out_gen
                       [ Open_creat; Open_append; Open_text ]
                       0o600 path
                   in
                   output_string channel (subject ^ "\n");
                   close_out channel;
                   Git.run_git ~cwd:dir [ "add"; "stack" ];
                   if index = 1 then (
                     let channel = open_out (Filename.concat dir "conflict") in
                     output_string channel (subject ^ "\n");
                     close_out channel;
                     Git.run_git ~cwd:dir [ "add"; "conflict" ]);
                   Git.run_git ~cwd:dir [ "commit"; "-qm"; subject ])
                 [ branch index; branch index ^ " follow-up" ]);
            Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; branch index ];
            states.(index) <-
              fst
                (B.step B.empty (B.Materialized (B.New_branch (sha boundary))))
          done;
          let io =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let restore state =
            match B.decode (B.yojson_of_t state) with
            | Ok value -> value
            | Error reason -> failwith reason
          in
          let repairs = ref 0 in
          let reconcile index base round =
            Git.run_git ~cwd:dir [ "checkout"; "-q"; branch index ];
            let initial, _ =
              B.step states.(index)
                (B.Request
                   B.
                     {
                       base;
                       policy = Rewrite;
                       purpose =
                         Reconcile_request ("stack-round-" ^ string_of_int round);
                     })
            in
            let rec settle state remaining =
              check "deep stack reconciliation converges" (remaining > 0);
              match (B.pending state, B.operation state) with
              | None, _ when B.publication_status state = `Published -> state
              | Some command, Some operation ->
                  let result =
                    E.execute ~io
                      ~prefix:("refs/onton/reconcile/deep-stack/" ^ branch index)
                      ~branch:(branch index) ~operation command
                  in
                  let next, effects =
                    B.step state
                      (B.Result { token = command.token; at = 100.; result })
                  in
                  let restored = restore next in
                  let restored =
                    List.fold_left
                      (fun state emitted ->
                        match emitted with
                        | B.Execute _ | B.Start_repair _ | B.Completed _ ->
                            state
                        | B.Repair token ->
                            check
                              "only the first rewritten dependency needs repair"
                              (index = 1 && round = 0 && !repairs < 2);
                            let claimed =
                              restore
                                (fst (B.step state (B.Repair_started token)))
                            in
                            let turn =
                              match
                                B.repair_turn claimed ~branch:(branch index)
                                  token
                              with
                              | Some turn -> turn
                              | None ->
                                  failwith "missing durable stack repair claim"
                            in
                            let before =
                              sha
                                (Git.git_capture ~cwd:dir
                                   [ "rev-parse"; "HEAD" ])
                            in
                            incr repairs;
                            let channel =
                              open_out (Filename.concat dir "conflict")
                            in
                            output_string channel
                              (if !repairs = 1 then "upstream\nresolved one\n"
                               else "upstream\nresolved one\nresolved two\n");
                            close_out channel;
                            Git.run_git ~cwd:dir [ "add"; "conflict" ];
                            check
                              "stack repair stages without owning continuation"
                              (Git_observation.repair_ready
                                 (E.observe_checkout io));
                            let after =
                              sha
                                (Git.git_capture ~cwd:dir
                                   [ "rev-parse"; "HEAD" ])
                            in
                            check "stack repair does not commit" (before = after);
                            restore
                              (fst
                                 (B.step claimed
                                    (B.repair_result ~turn_accepted:false ~turn
                                       ~at:100. ~before_head:(Some before)
                                       ~after_head:(Some after) ~timed_out:false
                                       ~final_result:true
                                       ~detail:"staged stack conflict"))))
                      restored effects
                  in
                  settle restored (remaining - 1)
              | _ -> failwith "deep stack reconciliation stopped unexpectedly"
            in
            states.(index) <- settle initial 20;
            let expected =
              List.init (if empty_tip && index = 3 then 3 else index + 1) branch
              |> List.concat_map (fun name -> [ name; name ^ " follow-up" ])
              |> String.concat "\n"
            in
            check "composed replay preserves each dependency exactly once"
              (Git.git_capture ~cwd:remote [ "show"; branch index ^ ":stack" ]
              = expected);
            check
              "later stack transformations preserve both repairs and upstream \
               work"
              (Git.git_capture ~cwd:remote
                 [ "show"; branch index ^ ":conflict" ]
              = "upstream\nresolved one\nresolved two");
            for advance = 0 to round do
              let file = "advance-" ^ string_of_int advance in
              check "composed replay retains every mainline advancement"
                (Git.git_capture ~cwd:remote
                   [ "show"; branch index ^ ":" ^ file ]
                = file)
            done;
            let target = Git.git_capture ~cwd:remote [ "rev-parse"; base ] in
            check "no dependency commits are replayed above the new target"
              (Git.git_capture ~cwd:remote
                 [ "log"; "--format=%s"; target ^ ".." ^ branch index ]
              =
              if empty_tip && index = 3 then ""
              else branch index ^ " follow-up\n" ^ branch index)
          in
          for merged = 0 to 2 do
            ignore
              (commit remote ("advance-" ^ string_of_int merged) "advance main");
            if merged = 1 then
              Git.run_git ~cwd:remote
                [ "cherry-pick"; "main.." ^ branch merged ]
            else (
              Git.run_git ~cwd:remote [ "merge"; "--squash"; branch merged ];
              Git.run_git ~cwd:remote
                [ "commit"; "-qm"; "squashed " ^ branch merged ]);
            let equivalence =
              Git.git_capture ~cwd:remote [ "cherry"; "main"; branch merged ]
              |> String.split_on_char '\n'
            in
            check
              "fixture exercises changed and equivalent dependency patch IDs"
              (List.length equivalence = 2
              && List.for_all
                   (fun line ->
                     String.starts_with
                       ~prefix:(if merged = 1 then "- " else "+ ")
                       line)
                   equivalence);
            if merged = 0 then (
              let channel = open_out (Filename.concat remote "conflict") in
              output_string channel "upstream\n";
              close_out channel;
              Git.run_git ~cwd:remote [ "add"; "conflict" ];
              Git.run_git ~cwd:remote
                [ "commit"; "-qm"; "concurrent upstream edit" ]);
            for index = merged + 1 to 3 do
              reconcile index
                (if index = merged + 1 then "main" else branch (index - 1))
                merged
            done
          done;
          check "deep stack executes exactly two distinct staged repairs"
            (!repairs = 2)))

let () =
  Eio_main.run @@ fun env ->
  List.iter (deep_stack env) [ false; true ];
  List.iter (plain_replay env) [ false; true ];
  List.iter
    (fun (subject, empty) ->
      Git.with_temp_repo (fun remote ->
          let original_base = commit remote "base" "base" in
          Git.with_temp_repo (fun dir ->
              Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
              Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
              Git.run_git ~cwd:dir
                [ "checkout"; "-q"; "-b"; "patch"; "origin/main" ];
              let first =
                commit dir "dependency" "[demo] Patch 1: dependency"
              in
              let dependency =
                if subject then
                  commit dir "dependency-extra" "[demo] Patch 1: follow-up"
                else first
              in
              let source =
                if empty then dependency else commit dir "patch" "patch"
              in
              ignore (commit remote "base-advance" "advance");
              Git.run_git ~cwd:remote [ "fetch"; "-q"; dir; dependency ];
              if subject then (
                Git.run_git ~cwd:remote [ "merge"; "--squash"; dependency ];
                Git.run_git ~cwd:remote
                  [ "commit"; "-q"; "-m"; "squash dependency" ])
              else Git.run_git ~cwd:remote [ "cherry-pick"; dependency ];
              let target =
                Git.git_capture ~cwd:remote [ "rev-parse"; "HEAD" ]
              in
              let io =
                E.make_io
                  ~process_mgr:(Eio.Stdenv.process_mgr env)
                  ~clock:(Eio.Stdenv.clock env) ~path:dir
              in
              let intent =
                B.
                  {
                    base = "main";
                    policy = Rewrite;
                    purpose =
                      Reconcile_scoped
                        {
                          request = "fixture";
                          project = "demo";
                          ancestors = [ Types.Patch_id.of_string "1" ];
                        };
                  }
              in
              let request state = fst (B.step state (B.Request intent)) in
              let execute io state =
                match (B.operation state, B.pending state) with
                | Some operation, Some command ->
                    ( command,
                      E.execute ~io ~prefix:"refs/onton/reconcile/replay-test"
                        ~branch:"patch" ~operation command )
                | None, _ | _, None -> failwith "expected pending command"
              in
              let expected_boundary =
                if subject then B.Subject_inferred (sha dependency)
                else B.Patch_equivalent (sha dependency)
              in
              let initial = request B.empty in
              let _, result = execute io initial in
              check "patch equivalence records the dependency boundary"
                (match fst (evidence result) with
                | Some o -> o.boundary = expected_boundary
                | None -> false);
              if subject then (
                let unscoped =
                  fst
                    (B.step B.empty
                       (B.Request { intent with purpose = Reconcile_base }))
                in
                let _, result = execute io unscoped in
                check "unscoped requests cannot infer dependency subjects"
                  (match fst (evidence result) with
                  | Some o -> o.boundary = B.Plain
                  | None -> false);
                let subject_failure =
                  E.
                    {
                      git =
                        (fun args ->
                          if List.mem "--format=%H%x00%P%x00%s" args then
                            (128, "", "injected subject probe failure")
                          else io.git args);
                    }
                in
                let _, result = execute subject_failure initial in
                check "subject probe failures remain retryable"
                  (snd (evidence result)));
              let failing =
                E.
                  {
                    git =
                      (fun args ->
                        match args with
                        | "log" :: _ -> (128, "", "injected probe failure")
                        | _ -> io.git args);
                  }
              in
              let _, failed = execute failing initial in
              check "failed patch-id probe remains retryable"
                (snd (evidence failed));
              let recorded, _ =
                B.step B.empty
                  (B.Materialized (B.New_branch (sha original_base)))
              in
              let _, preferred = execute failing (request recorded) in
              check "recorded provenance precedes patch-equivalence probe"
                (match fst (evidence preferred) with
                | Some o -> o.boundary = B.Recorded (sha original_base)
                | None -> false);
              let rec settle state remaining =
                if B.publication_status state = `Published then state
                else (
                  check "replay converges" (remaining > 0);
                  let command, result = execute io state in
                  let state, _ =
                    B.step state
                      (B.Result { token = command.token; at = 100.; result })
                  in
                  let state =
                    match B.decode (B.yojson_of_t state) with
                    | Ok state -> state
                    | Error reason -> failwith reason
                  in
                  settle state (remaining - 1))
              in
              let state = settle initial 12 in
              check "strategy survives serialization and settlement"
                (match B.operation state with
                | Some op -> op.boundary = expected_boundary
                | None -> false);
              check "only patch work replayed"
                (Git.git_capture ~cwd:dir
                   [ "log"; "--format=%s"; target ^ "..patch" ]
                = if empty then "" else "patch");
              List.iter
                (fun file ->
                  check
                    ("published tree retains " ^ file)
                    (Git.git_capture ~cwd:remote [ "show"; "patch:" ^ file ]
                    = file))
                ([ "base"; "base-advance"; "dependency" ]
                @ (if subject then [ "dependency-extra" ] else [])
                @ if empty then [] else [ "patch" ]);
              check "rewritten patch differs from captured source"
                (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ] <> source))))
    [ (false, false); (false, true); (true, false); (true, true) ];
  Git.with_temp_repo (fun remote ->
      let base = commit remote "base" "base" in
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          Git.run_git ~cwd:dir
            [ "checkout"; "-q"; "-b"; "patch"; "origin/main" ];
          let original = commit dir "dependency" "original dependency" in
          ignore (commit dir "patch" "patch");
          let reconstructed =
            Git.git_capture ~cwd:dir
              [
                "commit-tree";
                original ^ "^{tree}";
                "-p";
                base;
                "-m";
                "rewritten dependency";
              ]
          in
          Git.run_git ~cwd:dir
            [ "rebase"; "--onto"; reconstructed; original; "patch" ];
          ignore (commit remote "base-advance" "advance");
          Git.run_git ~cwd:remote [ "fetch"; "-q"; dir; original ];
          Git.run_git ~cwd:remote [ "cherry-pick"; original ];
          let io =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let state, _ =
            B.step B.empty (B.Materialized (B.New_branch (sha original)))
          in
          let state, _ =
            B.step state
              (B.Request
                 { base = "main"; policy = Rewrite; purpose = Reconcile_base })
          in
          let execute io state =
            match (B.operation state, B.pending state) with
            | Some operation, Some command ->
                ( command,
                  E.execute ~io ~prefix:"refs/onton/reconcile/reconstruction"
                    ~branch:"patch" ~operation command )
            | None, _ | _, None -> failwith "missing reconstruction command"
          in
          let no_equivalence =
            E.
              {
                git =
                  (fun args ->
                    if List.mem "--cherry-mark" args then
                      (128, "", "must not probe equivalence")
                    else io.git args);
              }
          in
          let _, result = execute no_equivalence state in
          let boundary =
            B.Reconstructed
              { original = sha original; upstream = sha reconstructed }
          in
          check "exact tree reconstruction precedes patch equivalence"
            (match fst (evidence result) with
            | Some o -> o.boundary = boundary
            | None -> false);
          let failed =
            E.
              {
                git =
                  (fun args ->
                    if List.mem "--first-parent" args then
                      (128, "", "injected topology failure")
                    else io.git args);
              }
          in
          let _, result = execute failed state in
          check "reconstruction probe failures retry" (snd (evidence result));
          let rec settle state remaining =
            if B.publication_status state = `Published then state
            else (
              check "reconstructed replay converges" (remaining > 0);
              let command, result = execute io state in
              let state, _ =
                B.step state
                  (B.Result { token = command.token; at = 100.; result })
              in
              let state =
                match B.decode (B.yojson_of_t state) with
                | Ok state -> state
                | Error reason -> failwith reason
              in
              settle state (remaining - 1))
          in
          let settled = settle state 12 in
          let retained =
            Git.git_capture ~cwd:dir
              [
                "for-each-ref";
                "--format=%(objectname)";
                "refs/onton/reconcile/reconstruction";
              ]
            |> String.split_on_char '\n'
          in
          check "reconstruction retains original and new boundary refs"
            (List.mem original retained && List.mem reconstructed retained);

          check "both reconstruction revisions survive checkpoints"
            (match B.operation settled with
            | Some op -> op.boundary = boundary
            | None -> false);
          List.iter
            (fun file ->
              check
                ("reconstruction preserves " ^ file)
                (Git.git_capture ~cwd:remote [ "show"; "patch:" ^ file ] = file))
            [ "base"; "base-advance"; "dependency"; "patch" ]));
  Git.with_temp_repo (fun remote ->
      let target = commit remote "upstream" "independent upstream root" in
      Git.with_temp_repo (fun dir ->
          let source = commit dir "local" "independent local root" in
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "patch" ];
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          let io =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let state, _ =
            B.step B.empty
              (B.Request
                 {
                   base = "main";
                   policy = Preserve_ancestry;
                   purpose = Reconcile_base;
                 })
          in
          let rec advance state remaining effects =
            check "unrelated recovery converges" (remaining > 0);
            match (B.operation state, B.pending state) with
            | Some operation, Some command ->
                let result =
                  E.execute ~io ~prefix:"refs/onton/reconcile/unrelated"
                    ~branch:"patch" ~operation command
                in
                let state, effects =
                  B.step state
                    (B.Result { token = command.token; at = 100.; result })
                in
                advance state (remaining - 1) effects
            | None, _ | _, None -> (state, effects)
          in
          let repair, effects = advance state 12 [] in
          let token =
            match effects with
            | [ B.Repair token ] -> token
            | []
            | B.Execute _ :: _
            | B.Start_repair _ :: _
            | B.Completed _ :: _
            | B.Repair _ :: _ :: _ ->
                failwith "expected history repair"
          in
          let claimed, effects = B.step repair (B.Repair_started token) in
          check "agent dispatch is claimed once"
            (effects = [ B.Start_repair token ]);
          let turn =
            match B.repair_turn claimed ~branch:"patch" token with
            | Some turn -> turn
            | None -> failwith "missing claimed turn"
          in
          check "topology failure reaches history recovery"
            (match turn.mode with
            | B.History_recovery { reason; _ } -> reason = "unrelated_histories"
            | B.Content_repair | B.Diagnosis _ -> false);
          (* Simulate the claimed history-repair agent's preserving merge. *)
          Git.run_git ~cwd:dir
            [ "merge"; "--allow-unrelated-histories"; "--no-edit"; target ];
          let completed, _ =
            B.step claimed (B.Repair_completed { token; at = 100. })
          in
          let settled, _ = advance completed 12 [] in
          check "verified agent history repair publishes"
            (B.publication_status settled = `Published);
          List.iter
            (fun ancestor ->
              check "both unrelated histories preserved"
                (Git.git_exit_code ~cwd:remote
                   [ "merge-base"; "--is-ancestor"; ancestor; "patch" ]
                = 0))
            [ source; target ]));
  print_endline "deterministic replay and agent fallback evidence: OK"
