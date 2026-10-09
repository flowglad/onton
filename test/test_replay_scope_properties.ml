(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton_core
module S = Replay_scope
module G = QCheck2.Gen

let oid n = Printf.sprintf "%040x" n
let boundary = oid 1

let records count =
  List.init count (fun index ->
      let n = count + 1 - index in
      Printf.sprintf "%s\000%s\n" (oid n) (oid (n - 1)))

let capture count history =
  S.capture ~boundary:(Some boundary) ~source:(oid (count + 1)) history

let get = function Ok value -> value | Error _ -> failwith "invalid fixture"
let scope = get (capture 1 (String.concat "" (records 1)))
let nul paths = String.concat "" (List.map (fun path -> path ^ "\000") paths)
let target = oid 500
let candidate = oid 501
let expected_tree = oid 900
let candidate_history = Printf.sprintf "%s\000%s\n" candidate target

let plan conflicts =
  S.prepare scope ~target
    ~status:(if conflicts = [] then 0 else 1)
    (nul (expected_tree :: conflicts))
  |> get

let () =
  QCheck_base_runner.run_tests_main
    [
      QCheck2.Test.make ~count:1000
        ~name:"merge continuation binds both exact captured parents"
        G.(triple (int_range 0 3) bool bool)
        (fun (kind, same_head, same_target) ->
          let request =
            match kind with
            | 0 -> S.Merge { source = scope.source; target }
            | 1 -> S.Identity scope.source
            | 2 -> S.Replay { source = scope.source; boundary; target }
            | _ -> S.Unproven
          in
          S.merge_continuation request
            ~head:(if same_head then scope.source else candidate)
            ~target:(if same_target then target else boundary)
          = (kind = 0 && same_head && same_target));
      QCheck2.Test.make ~count:1000
        ~name:
          "private certificates cannot authorize another request or candidate"
        G.(quad bool bool bool bool)
        (fun (same_source, same_boundary, same_target, same_candidate) ->
          try
            let proof =
              get
                (S.verify (plan []) ~candidate ~tree:expected_tree
                   ~history:candidate_history ~changed_paths:"")
            in
            let request =
              S.Replay
                {
                  source = (if same_source then scope.source else oid 700);
                  boundary = (if same_boundary then boundary else oid 701);
                  target = (if same_target then target else oid 702);
                }
            in
            S.matches_request proof ~request
              ~candidate:(if same_candidate then candidate else oid 703)
            = (same_source && same_boundary && same_target && same_candidate)
            && List.for_all
                 (fun request ->
                   not (S.matches_request proof ~request ~candidate))
                 [
                   S.Unproven;
                   S.Identity scope.source;
                   S.Merge { source = scope.source; target };
                 ]
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"local extension preserves target and recorded boundary"
        G.(int_range 0 80)
        (fun count ->
          try
            let extension =
              get (capture count (String.concat "" (records count)))
            in
            let contract =
              S.Replay { source = boundary; boundary = oid 990; target }
            in
            S.extend_local contract extension
            = Some
                (S.Replay
                   { source = extension.source; boundary = oid 990; target })
            && S.extend_local (S.Merge { source = boundary; target }) extension
               = Some (S.Merge { source = extension.source; target })
            && S.extend_local (S.Identity boundary) extension
               = Some (S.Identity extension.source)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"local extension cannot acquire authority from another source"
        G.(int_range 0 80)
        (fun count ->
          try
            let extension =
              get (capture count (String.concat "" (records count)))
            in
            List.for_all
              (fun contract -> S.extend_local contract extension = None)
              [
                S.Unproven;
                S.Identity target;
                S.Merge { source = target; target = boundary };
                S.Replay { source = target; boundary; target };
              ]
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"a projected side merge is not unfinished local work"
        G.(int_range 3 800)
        (fun n ->
          try
            let source = oid n in
            let history =
              Printf.sprintf "%s\000%s %s\n" source boundary (oid 1000)
            in
            let extension =
              get
                (S.capture_first_parent ~boundary:(Some boundary) ~source
                   history)
            in
            S.extend_local (S.Identity boundary) extension = None
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"repair projection excludes every merge and every side parent"
        G.(list_size (int_range 1 80) bool)
        (fun merges ->
          try
            let count = List.length merges in
            let history =
              List.mapi
                (fun i merge ->
                  let n = i + 2 in
                  Printf.sprintf "%s\000%s%s\n" (oid n)
                    (oid (n - 1))
                    (if merge then " " ^ oid (1000 + n) else ""))
                merges
              |> List.rev |> String.concat ""
            in
            let projected =
              get
                (S.capture_first_parent ~boundary:(Some boundary)
                   ~source:(oid (count + 1))
                   history)
            in
            let selected flag =
              List.mapi
                (fun i merge ->
                  if merge = flag then Some (oid (i + 2)) else None)
                merges
              |> List.filter_map Fun.id
            in
            projected.S.commits = selected false
            && projected.S.omitted_merges = selected true
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"linear input has identical deterministic and repair scope"
        G.(int_range 0 100)
        (fun count ->
          try
            let history = String.concat "" (records count) in
            S.capture ~boundary:(Some boundary)
              ~source:(oid (count + 1))
              history
            = S.capture_first_parent ~boundary:(Some boundary)
                ~source:(oid (count + 1))
                history
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"composed predictions cannot substitute another source commit"
        G.(pair (int_range 1 50) bool)
        (fun (count, substituted) ->
          try
            let scope =
              get (capture count (String.concat "" (records count)))
            in
            let predictions =
              List.mapi
                (fun index origin ->
                  ( (if substituted && index = count / 2 then oid 9999
                     else origin),
                    0,
                    nul [ oid (1000 + index) ] ))
                scope.S.commits
            in
            Result.is_ok
              (S.prepare_composed scope ~target ~initial_tree:expected_tree
                 ~predictions)
            = not substituted
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"missing prediction results cannot certify completion"
        G.(int_range 1 50)
        (fun count ->
          try
            let scope =
              get (capture count (String.concat "" (records count)))
            in
            S.prepare_composed scope ~target ~initial_tree:expected_tree
              ~predictions:[]
            = Error "replay_scope_incomplete_prediction"
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"sequencer scope checks are total over arbitrary todo files"
        G.string (fun text ->
          try
            ignore (S.todo_allowed scope text);
            true
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"recorded replay commits may be resumed only in captured order"
        G.(int_range 0 100)
        (fun count ->
          try
            let scope =
              get (capture count (String.concat "" (records count)))
            in
            let todo =
              List.map
                (fun revision -> "pick " ^ revision ^ " message\n")
                scope.S.commits
              |> String.concat ""
            in
            S.todo_allowed scope todo
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "foreign rebase picks and arbitrary sequencer commands are rejected"
        G.(pair (int_range 1000 10000) bool)
        (fun (n, execute) ->
          try
            let todo =
              if execute then "exec echo " ^ string_of_int n
              else "pick " ^ oid n ^ " unrelated"
            in
            not (S.todo_allowed scope todo)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"repeating an owned commit cannot widen a replay"
        G.(int_range 1 100)
        (fun count ->
          try
            let scope =
              get (capture count (String.concat "" (records count)))
            in
            let todo = "pick " ^ oid 2 ^ "\npick " ^ oid 2 ^ "\n" in
            not (S.todo_allowed scope todo)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "a rebase continuation cannot change its original source, target or \
           current commit"
        G.(triple bool bool bool)
        (fun (same_source, same_target, same_current) ->
          try
            let request =
              S.Replay { source = scope.S.source; boundary; target }
            in
            S.rebase_continuation request ~scope
              ~original:(if same_source then scope.S.source else oid 999)
              ~target:(if same_target then target else oid 998)
              ~current:(Some (if same_current then scope.S.source else oid 997))
              ~todo:("pick " ^ scope.S.source ^ "\n")
            = (same_source && same_target && same_current)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"a merge candidate can contain only the two captured input roots"
        G.bool (fun correct ->
          try
            let source = scope.S.source in
            let plan =
              get
                (S.prepare_merge ~source ~target ~status:0
                   (nul [ expected_tree ]))
            in
            let history =
              Printf.sprintf "%s\000%s %s\n" candidate source
                (if correct then target else oid 999)
            in
            Result.is_ok
              (S.verify plan ~candidate ~tree:expected_tree ~history
                 ~changed_paths:"")
            = correct
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "unchanged publication cannot substitute a different commit with the \
           same tree" G.bool (fun same ->
          try
            let plan =
              get
                (S.prepare_identity ~source:scope.S.source ~tree:expected_tree)
            in
            Result.is_ok
              (S.verify plan
                 ~candidate:(if same then scope.S.source else candidate)
                 ~tree:expected_tree ~history:"" ~changed_paths:"")
            = same
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "scope request decoding is total and malformed data grants no \
           authority"
        G.string (fun text ->
          try S.request_of_yojson (`String text) = S.Unproven with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"tree prediction decoding is total for arbitrary process output"
        G.(triple int string string)
        (fun (status, target, output) ->
          try
            ignore (S.prepare scope ~target ~status output);
            true
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"composed prediction decoding is total"
        G.(triple string string (list (triple string int string)))
        (fun (target, initial_tree, predictions) ->
          try
            ignore (S.prepare_composed scope ~target ~initial_tree ~predictions);
            true
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"candidate scope verification is total for arbitrary Git evidence"
        G.(quad string string string string)
        (fun (candidate, tree, history, changed_paths) ->
          try
            ignore (S.verify (plan []) ~candidate ~tree ~history ~changed_paths);
            true
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"a clean prediction permits no additional tree change"
        G.(int_range 0 10000)
        (fun n ->
          try
            S.verify (plan []) ~candidate ~tree:(oid 901)
              ~history:candidate_history
              ~changed_paths:(nul [ "extra-" ^ string_of_int n ])
            = Error "replay_scope_unrelated_changes"
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "conflict authority is exactly the independently captured path set"
        G.(pair (list (int_range 0 20)) (list (int_range 0 40)))
        (fun (allowed, changed) ->
          try
            let allowed = List.sort_uniq compare allowed in
            let changed = List.sort_uniq compare changed in
            let paths = List.map (fun n -> "path-" ^ string_of_int n) in
            let expected = List.for_all (fun n -> List.mem n allowed) changed in
            Result.is_ok
              (S.verify
                 (plan (paths allowed))
                 ~candidate
                 ~tree:(if changed = [] then expected_tree else oid 901)
                 ~history:candidate_history
                 ~changed_paths:(nul (paths changed)))
            = expected
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"a side parent is rejected even when the final tree matches"
        G.(int_range 1000 10000)
        (fun foreign ->
          try
            let history =
              Printf.sprintf "%s\000%s %s\n" candidate target (oid foreign)
            in
            S.verify (plan []) ~candidate ~tree:expected_tree ~history
              ~changed_paths:""
            = Error "replay_scope_foreign_ancestry"
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "contradictory tree identity and path observations grant no authority"
        G.bool (fun same_tree ->
          try
            S.verify (plan [ "conflict" ]) ~candidate
              ~tree:(if same_tree then expected_tree else oid 901)
              ~history:candidate_history
              ~changed_paths:(if same_tree then nul [ "conflict" ] else "")
            = Error "replay_scope_inconsistent_tree_diff"
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"verified candidates cannot be reused across request identities"
        G.(quad bool bool bool bool)
        (fun (same_source, same_boundary, same_target, same_candidate) ->
          try
            let proof =
              S.verify (plan []) ~candidate ~tree:expected_tree
                ~history:candidate_history ~changed_paths:""
              |> get
            in
            S.matches proof
              ~source:(if same_source then scope.S.source else oid 700)
              ~boundary:(if same_boundary then boundary else oid 701)
              ~target:(if same_target then target else oid 702)
              ~candidate:(if same_candidate then candidate else oid 703)
            = (same_source && same_boundary && same_target && same_candidate)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"scope decoder is total over arbitrary external observations"
        G.(triple (option string) string string)
        (fun (boundary, source, history) ->
          try
            try
              ignore (S.capture ~boundary ~source history);
              ignore (S.capture_first_parent ~boundary ~source history);
              true
            with _ -> false
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"a complete linear range authorizes exactly its ordered commits"
        G.(int_range 0 100)
        (fun count ->
          try
            match capture count (String.concat "" (records count)) with
            | Error _ -> false
            | Ok scope ->
                scope.S.boundary = boundary
                && scope.S.source = oid (count + 1)
                && scope.S.commits = List.init count (fun n -> oid (n + 2))
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"unknown provenance never authorizes even a linear range"
        G.(int_range 0 100)
        (fun count ->
          try
            S.capture ~boundary:None
              ~source:(oid (count + 1))
              (String.concat "" (records count))
            = Error S.Missing_boundary
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"every possible side parent invalidates a replay range"
        G.(pair (int_range 1 100) (int_range 0 1000))
        (fun (count, choice) ->
          try
            let index = choice mod count in
            let history =
              records count
              |> List.mapi (fun i line ->
                  if i = index then
                    String.sub line 0 (String.length line - 1)
                    ^ " "
                    ^ oid (count + 200)
                    ^ "\n"
                  else line)
              |> String.concat ""
            in
            capture count history = Error S.Nonlinear_history
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"dropping any observed commit cannot widen the certified range"
        G.(pair (int_range 1 100) (int_range 0 1000))
        (fun (count, choice) ->
          try
            let index = choice mod count in
            let history =
              records count
              |> List.mapi (fun i line -> if i = index then "" else line)
              |> String.concat ""
            in
            Result.is_error (capture count history)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"truncated successful output is not a complete observation"
        G.(int_range 1 100)
        (fun count ->
          try
            let text = String.concat "" (records count) in
            capture count (String.sub text 0 (String.length text - 1))
            = Error S.Invalid_history
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"extra history below the boundary is never patch scope"
        G.(int_range 0 100)
        (fun count ->
          try
            let history =
              String.concat "" (records count)
              ^ Printf.sprintf "%s\000%s\n" boundary (oid 9000)
            in
            Result.is_error (capture count history)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"repeated commits cannot certify a cyclic history"
        G.(int_range 1 100)
        (fun count ->
          try
            let history = String.concat "" (records count @ records count) in
            Result.is_error (capture count history)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"an unrelated linear history cannot satisfy captured endpoints"
        G.(int_range 1 100)
        (fun count ->
          try
            let history =
              List.init count (fun i ->
                  let n = count + 1000 - i in
                  Printf.sprintf "%s\000%s\n" (oid n) (oid (n - 1)))
              |> String.concat ""
            in
            capture count history = Error S.Disconnected_history
          with _ -> false);
    ]
