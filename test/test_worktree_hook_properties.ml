(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton_core
module H = Worktree_hook
module G = QCheck2.Gen

let request id =
  H.{ id; path = "/checkout"; branch = "patch"; script = "prepare" }

let decision state = H.next state ~path:"/checkout" ~branch:"patch"
let roundtrip state = H.decode (H.yojson_of_t state) = Ok state

let finish (run : H.run) outcome =
  H.Completed { id = run.request.id; attempt = run.attempt; outcome }

let tests =
  [
    QCheck2.Test.make ~count:100
      ~name:
        "legacy owners default to no hook and read-only verification cannot \
         schedule one" G.bool (fun verify ->
        try
          let module B = Branch_reconcile in
          let state =
            if verify then
              fst
                (B.step B.empty
                   (B.Request
                      {
                        base = "main";
                        policy = B.Preserve_ancestry;
                        purpose = B.Verify_publication;
                      }))
            else B.empty
          in
          let old =
            match B.yojson_of_t state with
            | `Assoc fields -> `Assoc (List.remove_assoc "worktree_hook" fields)
            | _ -> failwith "owner checkpoint"
          in
          B.decode old = Ok state
          && ((not verify)
             || B.step state (B.Worktree_hook (H.Plan (request "hook")))
                = (state, []))
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:
        "hook attempt counters reject malformed state and stop before overflow"
      G.(oneof_list [ -1; 0; 1; max_int ])
      (fun attempts ->
        try
          let state = H.step H.empty (H.Plan (request "hook")) in
          let json =
            match H.yojson_of_t state with
            | `Assoc fields ->
                `Assoc
                  (("attempts", `Int attempts)
                  :: List.remove_assoc "attempts" fields)
            | _ -> failwith "hook checkpoint"
          in
          match H.decode json with
          | Error _ -> attempts < 0
          | Ok state -> (
              attempts >= 0
              &&
              match decision state with
              | H.Run run -> attempts < max_int && run.attempt = attempts + 1
              | H.Stop "worktree_create_hook_attempt_exhausted" ->
                  attempts = max_int
              | H.Ready | H.Stop _ -> false)
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "owner hook uncertainty requires explicit resume before checkout \
         readiness"
      G.bool (fun failed ->
        try
          let module B = Branch_reconcile in
          let step state event = fst (B.step state event) in
          let reply state result =
            match B.pending state with
            | Some command ->
                step state
                  (B.Result { token = command.token; at = 10.; result })
            | None -> failwith "pending owner command"
          in
          let initial =
            step B.empty
              (B.Request
                 {
                   base = "main";
                   policy = B.Rewrite;
                   purpose = B.Provision_checkout "checkout";
                 })
          in
          let planned =
            step initial (B.Worktree_hook (H.Plan (request "hook")))
          in
          let rejected = reply planned B.Checkout_ready in
          let rejected =
            Onton_core_test_support.Publication_fixture.exhausted_diagnosis
              rejected
          in
          B.phase rejected
          = Some (B.Intervention "worktree_create_hook_incomplete")
          &&
          match decision (B.worktree_hook planned) with
          | H.Run first -> (
              let running = step planned (B.Worktree_hook (H.Started first)) in
              let running = Result.get_ok (B.decode (B.yojson_of_t running)) in
              let stopped =
                reply running
                  (B.Needs_diagnosis "worktree_create_hook_outcome_unknown")
              in
              let stopped =
                Onton_core_test_support.Publication_fixture.exhausted_diagnosis
                  stopped
              in
              let resumed = step stopped B.Resume in
              B.is_unsettled stopped
              && B.decode (B.yojson_of_t resumed) = Ok resumed
              &&
              match decision (B.worktree_hook resumed) with
              | H.Run second ->
                  let running =
                    step resumed (B.Worktree_hook (H.Started second))
                  in
                  let stale =
                    step running (B.Worktree_hook (finish first H.Succeeded))
                  in
                  let completed =
                    step stale
                      (B.Worktree_hook
                         (finish second
                            (if failed then H.Failed "hook failed"
                             else H.Succeeded)))
                  in
                  let ready = reply completed B.Checkout_ready in
                  second.attempt > first.attempt
                  && B.equal stale running
                  && B.phase ready = Some B.Settled
                  && (not (B.is_unsettled ready))
                  && B.decode (B.yojson_of_t ready) = Ok ready
              | H.Ready | H.Stop _ -> false)
          | H.Ready | H.Stop _ -> false
        with _ -> false);
    QCheck2.Test.make ~count:500
      ~name:"hook admission is total and preserves invalid input"
      G.(quad string string string string)
      (fun (id, path, branch, script) ->
        try
          let request = H.{ id; path; branch; script } in
          let state = H.step H.empty (H.Plan request) in
          let valid =
            List.for_all
              (fun s -> s <> "" && not (String.contains s '\000'))
              [ id; path; branch; script ]
          in
          H.valid state && roundtrip state
          && H.is_pending state = valid
          && H.equal state (H.step state (H.Plan request))
        with _ -> false);
    QCheck2.Test.make ~count:300
      ~name:
        "hook claims and completion acknowledgements survive restart without \
         repetition" G.bool (fun failed ->
        try
          let planned = H.step H.empty (H.Plan (request "hook")) in
          match decision planned with
          | H.Run run ->
              let running = H.step planned (H.Started run) in
              let restored = Result.get_ok (H.decode (H.yojson_of_t running)) in
              let outcome = if failed then H.Failed "failed" else H.Succeeded in
              let completed = H.step restored (finish run outcome) in
              H.valid running && H.is_pending running && roundtrip running
              && H.equal running (H.step running (H.Started run))
              && decision restored
                 = H.Stop "worktree_create_hook_outcome_unknown"
              && decision completed = H.Ready
              && (not (H.is_pending completed))
              && roundtrip completed
              && H.equal completed (H.step completed (finish run outcome))
              && H.equal completed (H.resume completed)
          | H.Ready | H.Stop _ -> false
        with _ -> false);
    QCheck2.Test.make ~count:300
      ~name:
        "hook explicit resume invalidates old completions and retains captured \
         script" G.string (fun new_script ->
        try
          let original = request "hook" in
          let planned = H.step H.empty (H.Plan original) in
          match decision planned with
          | H.Run first -> (
              let running = H.step planned (H.Started first) in
              let resumed = H.resume running in
              let replaced =
                H.step resumed (H.Plan { original with script = new_script })
              in
              H.equal resumed replaced && roundtrip resumed
              &&
              match decision resumed with
              | H.Run second ->
                  let running = H.step resumed (H.Started second) in
                  second.attempt > first.attempt
                  && H.equal_request first.request second.request
                  && H.equal running (H.step running (finish first H.Succeeded))
                  && decision (H.step running (finish second H.Succeeded))
                     = H.Ready
              | H.Ready | H.Stop _ -> false)
          | H.Ready | H.Stop _ -> false
        with _ -> false);
    QCheck2.Test.make ~count:300
      ~name:"hook context changes never authorize captured scripts elsewhere"
      G.(pair bool bool)
      (fun (started, retarget) ->
        try
          let planned = H.step H.empty (H.Plan (request "hook")) in
          let state =
            match decision planned with
            | H.Run run ->
                if started then H.step planned (H.Started run) else planned
            | H.Ready | H.Stop _ -> planned
          in
          H.next state
            ~path:(if retarget then "/checkout" else "/other")
            ~branch:(if retarget then "other" else "patch")
          = H.Stop "worktree_create_hook_context_changed"
        with _ -> false);
    QCheck2.Test.make ~count:500
      ~name:"hook malformed checkpoint decoding is total" G.string (fun input ->
        try
          List.for_all
            (fun json ->
              match H.decode json with
              | Error _ -> true
              | Ok state -> H.valid state && roundtrip state)
            [
              `String input;
              `Assoc [ ("request", `String input) ];
              `List [ `String input ];
              `Null;
            ]
        with _ -> false);
    QCheck2.Test.make ~count:300
      ~name:
        "hook attempt and restart interleavings retain uncertainty until \
         acknowledged"
      G.(list_size (int_range 0 70) (int_range 0 5))
      (fun actions ->
        try
          let initial = H.step H.empty (H.Plan (request "hook")) in
          let _, _, _, valid =
            List.fold_left
              (fun (state, runs, highest, valid) action ->
                let state, runs, highest =
                  match action with
                  | 0 -> (
                      match decision state with
                      | H.Run run ->
                          ( H.step state (H.Started run),
                            run :: runs,
                            max highest run.attempt )
                      | H.Ready | H.Stop _ -> (state, runs, highest))
                  | 1 -> (H.resume state, runs, highest)
                  | 2 -> (
                      match runs with
                      | run :: _ ->
                          (H.step state (finish run H.Succeeded), runs, highest)
                      | [] -> (state, runs, highest))
                  | 3 -> (
                      match List.rev runs with
                      | run :: _ ->
                          (H.step state (finish run H.Succeeded), runs, highest)
                      | [] -> (state, runs, highest))
                  | 4 ->
                      ( Result.get_ok (H.decode (H.yojson_of_t state)),
                        runs,
                        highest )
                  | _ -> (H.step state (H.Plan (request "hook")), runs, highest)
                in
                let pending_valid =
                  match decision state with
                  | H.Run run -> run.attempt > highest
                  | H.Ready | H.Stop _ -> true
                in
                ( state,
                  runs,
                  highest,
                  valid && H.valid state && roundtrip state && pending_valid ))
              (initial, [], 0, true) actions
          in
          valid
        with _ -> false);
  ]

let () = QCheck_base_runner.run_tests_main tests
