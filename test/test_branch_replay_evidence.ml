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
  | Remote_checked_out | Integrated _ | Conflict _ | Recovery_observed _
  | Inspected _ | Inspected_active _ | Published | Remote _
  | Recovery_required _ | Publication_rejected _ | Attempt_failed _
  | Needs_diagnosis _ ->
      (None, false)

let plain_replay env merged_history =
  Git.with_temp_repo (fun remote ->
      let base = commit remote "base" "base" in
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
          if merged_history then (
            Git.run_git ~cwd:dir [ "merge"; "--no-ff"; "--no-commit"; "side" ];
            ignore (commit dir "merge-only" "merge-only edit"));
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
          let rec settle state remaining effects =
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
                      check "adopted branch observes a shared ancestor"
                        (observed.boundary = B.Inferred (sha base))
                  | B.Checkout_ready | B.Observed_recovery _
                  | B.Observed_active _ | B.Pinned | B.Merge_completion_needed _
                  | B.Merge_completed _ | B.Remote_replay_selected _
                  | B.Remote_checked_out | B.Integrated _ | B.Conflict _
                  | B.Recovery_observed _ | B.Inspected _ | B.Inspected_active _
                  | B.Published | B.Remote _ | B.Retryable _
                  | B.Recovery_required _ | B.Publication_rejected _
                  | B.Attempt_failed _ | B.Needs_diagnosis _ ->
                      ());
                  let next, effects =
                    B.step state
                      (B.Result { token = command.token; at = 100.; result })
                  in
                  let restored =
                    match B.decode (B.yojson_of_t next) with
                    | Ok restored -> restored
                    | Error reason -> failwith reason
                  in
                  settle restored (remaining - 1) effects
              | ( Some { repair = Some { mode = B.History_recovery _; _ }; _ },
                  None ) ->
                  let token =
                    List.find_map
                      (function
                        | B.Repair token -> Some token
                        | B.Execute _ | B.Start_repair _ | B.Completed _ -> None)
                      effects
                    |> Option.get
                  in
                  let state = fst (B.step state (B.Repair_started token)) in
                  Git.run_git ~cwd:dir [ "merge"; "--no-edit"; "origin/main" ];
                  let state, effects =
                    B.step state (B.Repair_completed { token; at = 100. })
                  in
                  settle state (remaining - 1) effects
              | None, _
              | ( Some
                    {
                      repair =
                        ( None
                        | Some { mode = B.Content_repair | B.Diagnosis _; _ } );
                      _;
                    },
                  None ) ->
                  failwith "unproven replay stopped without agent recovery")
          in
          let settled = settle initial 18 [] in
          check
            "adopted branch rebases and publishes without materialization \
             evidence"
            (B.publication_status settled = `Published);
          check "upstream is included in the published branch"
            (Git.git_exit_code ~cwd:remote
               [ "merge-base"; "--is-ancestor"; target; "patch" ]
            = 0);
          List.iter
            (fun file ->
              check "rebase preserves patch content"
                (Git.git_capture ~cwd:remote [ "show"; "patch:" ^ file ] = file))
            (if merged_history then [ "merge-only"; "patch"; "side" ]
             else [ "patch" ]);
          check "captured source and target remain retained for recovery"
            (List.for_all
               (fun revision ->
                 List.mem (sha revision) (B.required_revisions settled))
               [ source; target ])))

let criss_cross env =
  Git.with_temp_repo (fun remote ->
      let root = commit remote "root" "root" in
      Git.run_git ~cwd:remote [ "checkout"; "-qb"; "left" ];
      let left = commit remote "left" "left" in
      Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
      let right = commit remote "right" "right" in
      Git.run_git ~cwd:remote [ "merge"; "--no-ff"; "--no-edit"; left ];
      let opposite =
        Git.git_capture ~cwd:remote
          [
            "commit-tree";
            "HEAD^{tree}";
            "-p";
            left;
            "-p";
            right;
            "-m";
            "opposite merge";
          ]
      in
      Git.run_git ~cwd:remote [ "branch"; "patch"; opposite ];
      let target = commit remote "upstream" "upstream" in
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          Git.run_git ~cwd:dir [ "checkout"; "-qb"; "patch"; "origin/patch" ];
          let source = commit dir "patch" "patch" in
          let bases =
            Git.git_capture ~cwd:dir [ "merge-base"; "--all"; source; target ]
            |> String.split_on_char '\n'
          in
          check "fixture has two best shared ancestors"
            (List.length bases = 2
            && List.mem left bases && List.mem right bases);
          let raw =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let mutations = ref 0 and turns = ref 0 in
          let io =
            E.
              {
                git =
                  (fun args ->
                    (match args with
                    | "rebase" :: _ | "merge" :: _ | "cherry-pick" :: _ ->
                        incr mutations
                    | _ -> ());
                    raw.git args);
              }
          in
          let initial =
            fst (B.step B.empty (B.Materialized (B.New_branch (sha root))))
          in
          let initial =
            fst
              (B.step initial
                 (B.Request
                    {
                      base = "main";
                      policy = Rewrite;
                      purpose = Reconcile_base;
                    }))
          in
          let rec drive state fuel effects =
            check "criss-cross recovery converges" (fuel > 0);
            if B.publication_status state = `Published then state
            else
              match (B.operation state, B.pending state) with
              | Some operation, Some command ->
                  let result =
                    E.execute ~io ~prefix:"refs/onton/reconcile/criss-cross"
                      ~branch:"patch" ~operation command
                  in
                  let state, effects =
                    B.step state
                      (B.Result { token = command.token; at = 100.; result })
                  in
                  drive state (fuel - 1) effects
              | Some { repair = Some _; _ }, None ->
                  incr turns;
                  check "ambiguous ancestry delegates before mutation"
                    (!turns = 1 && !mutations = 0);
                  let token =
                    List.find_map
                      (function
                        | B.Repair token -> Some token
                        | B.Execute _ | B.Start_repair _ | B.Completed _ -> None)
                      effects
                    |> Option.get
                  in
                  let state = fst (B.step state (B.Repair_started token)) in
                  Git.run_git ~cwd:dir [ "merge"; "--no-edit"; target ];
                  let state, effects =
                    B.step state (B.Repair_completed { token; at = 100. })
                  in
                  drive state (fuel - 1) effects
              | Some { repair = None; _ }, None | None, _ ->
                  failwith "criss-cross recovery stopped"
          in
          let state = drive initial 30 [] in
          check "ambiguous ancestry used exactly one recovery turn" (!turns = 1);
          List.iter
            (fun revision ->
              check "agent recovery preserves the criss-cross inputs"
                (Git.git_exit_code ~cwd:remote
                   [ "merge-base"; "--is-ancestor"; revision; "patch" ]
                = 0))
            [ source; target ];
          check "criss-cross recovery survives checkpoint import"
            (match B.decode (B.yojson_of_t state) with
            | Ok restored -> B.equal restored state
            | Error _ -> false)))

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
                            ignore
                              (io.git
                                 [
                                   "-c";
                                   "core.editor=true";
                                   "rebase";
                                   "--continue";
                                 ]);
                            if
                              not
                                (Git_observation.equal_sequencer
                                   (E.observe_checkout io).sequencer
                                   Git_observation.None_active)
                            then (
                              incr repairs;
                              let channel =
                                open_out (Filename.concat dir "conflict")
                              in
                              output_string channel
                                "upstream\nresolved one\nresolved two\n";
                              close_out channel;
                              Git.run_git ~cwd:dir [ "add"; "conflict" ];
                              Git.run_git ~cwd:dir
                                [
                                  "-c";
                                  "core.editor=true";
                                  "rebase";
                                  "--continue";
                                ]);
                            let after =
                              sha
                                (Git.git_capture ~cwd:dir
                                   [ "rev-parse"; "HEAD" ])
                            in
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
  criss_cross env;
  (* Failed ancestry reads are transport failures, not absent provenance. *)
  Git.with_temp_repo (fun remote ->
      ignore (commit remote "base" "base");
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          Git.run_git ~cwd:dir
            [ "checkout"; "-q"; "-b"; "patch"; "origin/main" ];
          ignore (commit dir "patch" "patch");
          ignore (commit remote "upstream" "upstream");
          let raw =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let io =
            E.
              {
                git =
                  (fun args ->
                    match args with
                    | "merge-base" :: _ -> (128, "", "injected ancestry outage")
                    | _ -> raw.git args);
              }
          in
          let state, _ =
            B.step B.empty
              (B.Request
                 { base = "main"; policy = Rewrite; purpose = Reconcile_base })
          in
          let operation = Option.get (B.operation state) in
          let command = Option.get (B.pending state) in
          check "shared ancestor failures remain retryable"
            (snd
               (evidence
                  (E.execute ~io ~prefix:"refs/onton/reconcile/probe"
                     ~branch:"patch" ~operation command)))));
  List.iter
    (fun policy ->
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
              let created =
                fst
                  (B.step B.empty (B.Materialized (B.New_branch (sha source))))
              in
              let state, _ =
                B.step created
                  (B.Request { base = "main"; policy; purpose = Reconcile_base })
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
                | B.History_recovery { reason; _ } ->
                    reason
                    =
                    if policy = B.Rewrite then "no_shared_ancestor"
                    else "unrelated_histories"
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
                [ source; target ])))
    [ B.Rewrite; B.Preserve_ancestry ];
  print_endline "deterministic replay and agent fallback evidence: OK"
