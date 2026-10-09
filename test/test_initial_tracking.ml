(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
module B = Branch_reconcile
module E = Branch_reconcile_executor
module Git = Onton_test_support.Git_env

type boundary =
  | Push
  | Tracking_ref
  | Remote_config
  | Merge_config
  | Confirmation

type tracking = Absent | Inherited | External

let check message condition = if not condition then failwith message

let restore state =
  match B.decode (B.yojson_of_t state) with
  | Ok state -> state
  | Error reason -> failwith reason

let commit dir file =
  let out = open_out (Filename.concat dir file) in
  output_string out (file ^ "\n");
  close_out out;
  Git.run_git ~cwd:dir [ "add"; file ];
  Git.run_git ~cwd:dir [ "commit"; "-q"; "-m"; file ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let () =
  Eio_main.run @@ fun env ->
  let cases =
    List.concat_map
      (fun boundary -> [ Some (boundary, false); Some (boundary, true) ])
      [ Push; Tracking_ref; Remote_config; Merge_config; Confirmation ]
    @ [ None ]
  in
  let cases =
    List.map (fun fault -> (fault, Absent)) cases
    @ [ (None, Inherited); (None, External) ]
  in
  List.iter
    (fun (fault, tracking) ->
      Git.with_temp_repo (fun remote ->
          ignore (commit remote "base");
          Git.with_temp_repo (fun dir ->
              Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
              Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
              Git.run_git ~cwd:dir
                [ "checkout"; "-q"; "--no-track"; "-b"; "patch"; "origin/main" ];
              let candidate = commit dir "work" in
              (match tracking with
              | Absent -> ()
              | Inherited | External ->
                  Git.run_git ~cwd:dir
                    [
                      "config";
                      "branch.patch.remote";
                      (if tracking = Inherited then "origin" else "elsewhere");
                    ];
                  Git.run_git ~cwd:dir
                    [
                      "config";
                      "branch.patch.merge";
                      (if tracking = Inherited then "refs/heads/main"
                       else "refs/heads/custom");
                    ]);
              let real =
                E.make_io
                  ~process_mgr:(Eio.Stdenv.process_mgr env)
                  ~clock:(Eio.Stdenv.clock env) ~path:dir
              in
              let armed = ref (Option.is_some fault) in
              let successful_pushes = ref 0 in
              let tracking_ref = "refs/remotes/origin/patch" in
              let matches boundary args =
                match (boundary, args) with
                | Push, "push" :: _ -> true
                | Tracking_ref, [ "update-ref"; name; _; "" ] ->
                    name = tracking_ref
                | ( Remote_config,
                    [ "config"; "--local"; "branch.patch.remote"; "origin" ] )
                  ->
                    true
                | ( Merge_config,
                    [
                      "config";
                      "--local";
                      "branch.patch.merge";
                      "refs/heads/patch";
                    ] ) ->
                    true
                | ( ( Push | Tracking_ref | Remote_config | Merge_config
                    | Confirmation ),
                    _ ) ->
                    false
              in
              let execute_git args =
                let ((code, _, _) as result) = real.git args in
                (match args with
                | "push" :: _ when code = 0 ->
                    incr successful_pushes;
                    (* Model a restart with no tracking ref. The public ref and the
                 owner's private recovery refs remain intact. *)
                    Git.run_git ~cwd:dir [ "update-ref"; "-d"; tracking_ref ]
                | _ -> ());
                result
              in
              let io =
                E.
                  {
                    git =
                      (fun args ->
                        match fault with
                        | Some (boundary, after)
                          when !armed && matches boundary args ->
                            armed := false;
                            if after then ignore (execute_git args);
                            (124, "", "injected initial-tracking interruption")
                        | Some _ | None -> execute_git args);
                  }
              in
              let prefix = "refs/onton/reconcile/initial-tracking" in
              let rec drive remaining state =
                if remaining = 0 then
                  failwith "initial publication did not terminate";
                let state = restore state in
                match (B.operation state, B.pending state) with
                | Some operation, Some command -> (
                    let stop =
                      match (fault, command.kind) with
                      | Some (Confirmation, after), B.Confirm _ when !armed ->
                          Some after
                      | ( ( Some
                              ( ( Push | Tracking_ref | Remote_config
                                | Merge_config | Confirmation ),
                                _ )
                          | None ),
                          ( B.Observe | Inspect | Verify_recovery | Pin _
                          | Integrate _ | Continue _ | Commit_merge _
                          | Plan_remote_replay _ | Checkout_remote _ | Publish _
                          | Confirm _ ) ) ->
                          None
                    in
                    match stop with
                    | Some after ->
                        armed := false;
                        if after then
                          ignore
                            (E.execute ~io ~prefix ~branch:"patch" ~operation
                               command);
                        state
                        (* Loss of acknowledgement: retain the preceding checkpoint. *)
                    | None ->
                        let result =
                          E.execute ~io ~prefix ~branch:"patch" ~operation
                            command
                        in
                        drive (remaining - 1)
                          (fst
                             (B.step state
                                (B.Result
                                   { token = command.token; at = 100.; result })))
                    )
                | None, _ | _, None -> state
              in
              let intent =
                B.
                  {
                    base = "main";
                    policy = Rewrite;
                    purpose = Publish_session "initial";
                  }
              in
              let state = drive 50 (fst (B.step B.empty (B.Request intent))) in
              check "injected boundary was reached" (not !armed);
              let state = drive 50 (fst (B.step (restore state) B.Recover)) in
              check "initial publication settles after interruption"
                (B.phase state = Some B.Settled);
              check "initial publication performs one successful push"
                (!successful_pushes = 1);
              check "published commit survives setup recovery"
                (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                = candidate);
              (match tracking with
              | Absent | Inherited ->
                  check "upstream is usable after recovery"
                    (Git.git_capture ~cwd:dir
                       [ "rev-parse"; "--abbrev-ref"; "patch@{upstream}" ]
                    = "origin/patch");
                  check "tracking ref points at confirmed initial publication"
                    (Git.git_capture ~cwd:dir [ "rev-parse"; tracking_ref ]
                    = candidate);
                  Git.run_git ~cwd:dir
                    [ "-c"; "push.default=simple"; "push"; "-q"; "--dry-run" ]
              | External ->
                  check "unrelated remote configuration survives publication"
                    (Git.git_capture ~cwd:dir
                       [ "config"; "--get"; "branch.patch.remote" ]
                    = "elsewhere");
                  check "unrelated merge configuration survives publication"
                    (Git.git_capture ~cwd:dir
                       [ "config"; "--get"; "branch.patch.merge" ]
                    = "refs/heads/custom"));
              let repeated, effects = B.step state (B.Request intent) in
              check "completed initial setup is a fixed point"
                (B.equal repeated state && effects = []))))
    cases;
  print_endline "initial publication tracking interruption/restart: OK"
