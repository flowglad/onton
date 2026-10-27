(* @archlint.module test
   @archlint.domain worker-transport *)
open Onton

[@@@warning "-4"]

module P = Onton_core.Worker_protocol
module V = Onton_core.Worker_protocol_v3

let check label ok = if not ok then failwith label
let field k = function `Assoc xs -> List.assoc_opt k xs | _ -> None

let str k json =
  match field k json with
  | Some (`String x) -> x
  | _ -> failwith ("missing " ^ k)

let replace k value = function
  | `Assoc xs -> `Assoc ((k, value) :: List.remove_assoc k xs)
  | _ -> failwith "not an object"

let wire json = Yojson.Safe.to_string json ^ "\n"

let frame kind fields =
  `Assoc
    ([
       ("version", `Int 3);
       ("type", `String kind);
       ("run_id", `String "run-1");
       ("patch_id", `String "2");
     ]
    @ fields)

let op n =
  [
    ("operation_id", `String ("operation-" ^ string_of_int n));
    ("attempt_id", `String ("attempt-" ^ string_of_int n));
    ("assignment_seq", `String (string_of_int n));
  ]

let context ~base ~generation ~input_version =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("version", `Int 3);
         ("run_id", `String "run-1");
         ("patch_id", `String "2");
         ("config_id", `String "config");
         ("plan_id", `String "plan");
         ("message_id", `String ("msg-" ^ string_of_int generation));
         ("generation", `String (string_of_int generation));
         ( "dependencies",
           `List
             [
               `Assoc
                 [
                   ("patch_id", `String "1");
                   ("admitted", `Bool true);
                   ("revision", `String "abc123");
                 ];
             ] );
         ("selected_base", `String base);
         ("execution_mode", `String "feature");
         ("root_role", `String "patch");
         ("admitted", `Bool true);
         ("evidence_id", `String "evidence");
         ("guidance_ids", `List []);
         ("input_version", `String (string_of_int input_version));
       ])

let decode json =
  match V.decode_host (wire json) with
  | Ok msg -> msg
  | Error error -> failwith ("invalid v3 fixture: " ^ error)

let step host event =
  let state, outputs = V.step !host event in
  host := state;
  outputs

let send output json =
  output_string output (wire json);
  flush output

let recv channel = Yojson.Safe.from_string (input_line channel)

let write path bytes =
  let out = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out out)
    (fun () -> output_string out bytes)

let () =
  let root = Filename.temp_dir "onton-v3-worker-" "" in
  let seed = Filename.concat root "seed" in
  Unix.mkdir seed 0o755;
  Onton_test_support.Git_env.init_repo seed;
  ignore
    (Onton_test_support.Branch_reconcile_git_fixture.commit seed "base" "base\n"
       "base");
  let origin = Filename.concat root "origin.git" in
  Onton_test_support.Git_env.sh ~dir:"/"
    (Printf.sprintf "git clone --bare -q %s %s" (Filename.quote seed)
       (Filename.quote origin));
  Onton_test_support.Git_env.run_git ~cwd:seed
    [ "push"; "-q"; origin; "HEAD:refs/heads/v3-test/patch-1" ];
  let managed = Filename.concat root "managed" in
  Onton_test_support.Git_env.sh ~dir:"/"
    (Printf.sprintf "git clone --no-local -q %s %s" (Filename.quote origin)
       (Filename.quote managed));
  Onton_test_support.Git_env.run_git ~cwd:managed
    [ "config"; "user.name"; "Onton Worker Test" ];
  Onton_test_support.Git_env.run_git ~cwd:managed
    [ "config"; "user.email"; "worker-test@example.invalid" ];
  let gameplan = Filename.concat root "plan.yaml" in
  write gameplan
    "projectName: v3-test\n\
     owner: owner\n\
     repo: repo\n\
     problemStatement: test\n\
     solutionSummary: test\n\
     patches:\n\
    \  - number: 1\n\
    \    title: root\n\
    \    description: root\n\
    \    dependsOn: []\n\
    \  - number: 2\n\
    \    title: child\n\
    \    description: child\n\
    \    dependsOn: [1]\n\
     dependencyGraph:\n\
    \  - patch: 1\n\
    \    dependsOn: []\n\
    \  - patch: 2\n\
    \    dependsOn: [1]\n";
  let fake_bin = Filename.concat root "bin" in
  Unix.mkdir fake_bin 0o755;
  let codex = Filename.concat fake_bin "codex" in
  write codex
    "#!/bin/sh\n\
     printf '%s\\n' \"$@\" > \"$ONTON_FAKE_PROMPT\"\n\
     mkdir -p \"$ONTON_DATA_DIR/v3-test/artifacts/2\"\n\
     printf 'EXACT NOTES\\n' > \
     \"$ONTON_DATA_DIR/v3-test/artifacts/2/pr-body.md\"\n\
     printf 'implementation\\n' > implementation.txt\n\
     git add implementation.txt && git commit -qm 'implementation'\n\
     printf '%s\\n' '{\"type\":\"thread.started\",\"thread_id\":\"fake\"}' \
     '{\"type\":\"turn.started\"}' \
     '{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"text\":\"done\"}}' \
     '{\"type\":\"turn.completed\"}'\n";
  Unix.chmod codex 0o755;
  let binary =
    match Sys.getenv_opt "ONTON_WORKER_BINARY" with
    | Some x -> x
    | None -> Option.get (Sys.getenv_opt "ONTON_WORKER_BINARY_DEFAULT")
  in
  let prompt_log = Filename.concat root "prompt.log" in
  let env =
    Array.to_list (Unix.environment ())
    |> List.filter (fun s ->
        not
          (String.starts_with ~prefix:"HOME=" s
          || String.starts_with ~prefix:"PATH=" s))
  in
  let env =
    Array.of_list
      ([
         "HOME=" ^ root;
         "PATH=" ^ fake_bin ^ ":"
         ^ Option.value (Sys.getenv_opt "PATH") ~default:"";
         "ONTON_FAKE_PROMPT=" ^ prompt_log;
         "ONTON_DATA_DIR=" ^ Filename.concat root "data";
       ]
      @ env)
  in
  let command =
    String.concat " "
      (List.map Filename.quote
         [
           binary;
           "--gameplan";
           gameplan;
           "--repo-root";
           managed;
           "--project-name";
           "v3-test";
           "--protocol-version";
           "3";
           "--ack-timeout";
           "3";
         ])
  in
  let output, input, errors = Unix.open_process_full command env in
  let host = ref (V.initial ()) in
  let capsule = ref "{}" and version = ref 0L in
  let bind_context =
    context ~base:"v3-test/patch-1" ~generation:1 ~input_version:0
  in
  let bind =
    frame "bind"
      [
        ("worker_id", `String "worker-1");
        ("workspace_id", `String "workspace-1");
        ("context", `String "requirements");
        ("capsule", `String !capsule);
        ("state_version", `String "0");
        ("attention_id", `Null);
        ("admission", `Assoc [ ("mode", `String "fresh") ]);
        ("execution_context", `String bind_context);
      ]
  in
  let outputs = step host (V.Host (decode bind)) in
  check "host ready"
    (List.exists (function V.Send (P.Ready _) -> true | _ -> false) outputs);
  send input bind;
  check "real worker v3 ready" (str "type" (recv output) = "ready");
  let input_frame =
    frame "input"
      [
        ("input_id", `String "input-1");
        ("input_version", `String "1");
        ("guidance", `String "new guidance");
        ("selected_base", `String "main");
      ]
  in
  let input_delivered = ref false in
  let execute n ~context ~input_version work =
    let json =
      frame "execute"
        (op n
        @ [
            ("expected_state_version", `String (Int64.to_string !version));
            ("work", work);
            ("guidance", `String "Do assigned work");
            ("recovery_mode", `String "retained_workspace");
            ("renewal_id", `Null);
            ("renewed_intent", `Bool false);
            ("execution_context", `String context);
            ("input_version", `String (string_of_int input_version));
          ])
    in
    let outputs = step host (V.Host (decode json)) in
    check "model admits operation"
      (List.exists (function V.Begin _ -> true | _ -> false) outputs);
    send input json;
    check "worker accepts" (str "type" (recv output) = "accepted");
    let rec run checks =
      let msg = recv output in
      match str "type" msg with
      | "checkpoint" ->
          let bytes = str "capsule" msg in
          let parsed =
            match Worker_checkpoint.decode (Yojson.Safe.from_string bytes) with
            | Ok c -> c
            | Error e -> failwith (e ^ ": " ^ bytes)
          in
          check "capsule carries exact context"
            (Worker_checkpoint.v3_context parsed = Some bind_context);
          (if n = 1 && checks > 0 then
             match Worker_checkpoint.terminal parsed with
             | None ->
                 check "new input waits for safe boundary"
                   (Worker_checkpoint.v3_input parsed = None)
             | Some _ ->
                 check "terminal checkpoint consumes current input"
                   (Option.exists
                      (fun i -> i.V.selected_base = "main")
                      (Worker_checkpoint.v3_input parsed)));
          let next = Int64.of_string (str "state_version" msg) in
          let proposal_id = str "proposal_id" msg in
          ignore
            (step host
               (V.Worker_event
                  (P.Propose_checkpoint { proposal_id; capsule = bytes })));
          let ack =
            frame "checkpoint_ack"
              (op n
              @ [
                  ("proposal_id", `String proposal_id);
                  ("state_version", `String (Int64.to_string next));
                  ("capsule", `String bytes);
                  ("accepted", `Bool true);
                ])
          in
          ignore (step host (V.Host (decode ack)));
          capsule := bytes;
          version := next;
          send input ack;
          if n = 1 && checks = 0 then (
            check
              "captured Start retains original selected base before retarget"
              (Worker_checkpoint.v3_input parsed = None);
            ignore (step host (V.Host (decode input_frame)));
            ignore (step host (V.Input_committed ("input-1", 1L)));
            send input input_frame;
            input_delivered := true);
          run (checks + 1)
      | "observation" -> run checks
      | "result" ->
          check "result follows checkpoint" (checks > 0);
          let status = str "status" msg in
          check "known outcome"
            (List.mem status
               [ "completed_ok"; "completed_no_commits"; "retryable"; "failed" ]);
          let status =
            match status with
            | "completed_ok" -> P.Completed_ok
            | "completed_no_commits" -> P.Completed_no_commits
            | "retryable" -> P.Retryable
            | _ -> P.Failed
          in
          ignore (step host (V.Worker_event P.Cleanup_complete));
          ignore
            (step host
               (V.Worker_event
                  (P.Finish
                     {
                       status;
                       diagnostics = str "diagnostics" msg;
                       renewal_id = None;
                     })));
          let ack =
            frame "result_ack"
              (op n @ [ ("state_version", `String (Int64.to_string !version)) ])
          in
          ignore (step host (V.Host (decode ack)));
          send input ack;
          msg
      | other -> failwith ("unexpected v3 output " ^ other)
    in
    run 0
  in
  let session =
    `Assoc
      [
        ("type", `String "session");
        ("phase", `String "start");
        ("kind", `Null);
        ("turn", `String "DEPENDENCY-UNBLOCKED");
      ]
  in
  let result = execute 1 ~context:bind_context ~input_version:0 session in
  check "dependent session admitted without merged peer defaults"
    (List.mem (str "status" result)
       [ "completed_ok"; "completed_no_commits"; "retryable" ]);
  let prompt = In_channel.with_open_bin prompt_log In_channel.input_all in
  check "actual backend received dependent Start"
    (Base.String.is_substring prompt ~substring:"DEPENDENCY-UNBLOCKED");
  let restored =
    match Worker_checkpoint.decode (Yojson.Safe.from_string !capsule) with
    | Ok c -> c
    | Error e -> failwith e
  in
  let capsule_json = Yojson.Safe.from_string !capsule in
  (match field "note" capsule_json with
  | Some (`Assoc fields) ->
      let too_large =
        replace "note"
          (`Assoc
             (("bytes", `String (String.make 262145 'x'))
             :: List.remove_assoc "bytes" fields))
          capsule_json
      in
      check "oversized artifact rejects without erasing old capsule"
        (Result.is_error (Worker_checkpoint.decode too_large))
  | _ -> failwith "notes missing from capsule");
  (match Worker_checkpoint.note restored with
  | Some (V.Present { assignment; bytes; publication }) ->
      check "exact bounded note bytes" (bytes = "EXACT NOTES\n");
      check "note bound to session assignment"
        (assignment.P.operation_id = "operation-1");
      check "note has captured publication" (publication <> "unpublished")
  | _ -> failwith "missing checkpointed publication-bound notes");
  let parsed_gameplan =
    match Onton_core.Gameplan_parser.parse_file gameplan with
    | Ok p -> p.Onton_core.Gameplan_parser.gameplan
    | Error e -> failwith e
  in
  let restored_agent =
    match
      Worker_checkpoint.validate_binding restored ~run_id:"run-1"
        ~patch_id:(Onton_core.Types.Patch_id.of_string "2")
        ~context:"requirements" ~state_version:!version ~attention_id:None
        ~gameplan:parsed_gameplan
        ~main_branch:(Onton_core.Types.Branch.of_string "main")
    with
    | Ok a -> a
    | Error e -> failwith e
  in
  let projection =
    match
      Worker_checkpoint.project restored ~run_id:"run-1"
        ~patch_id:(Onton_core.Types.Patch_id.of_string "2")
        ~context:"requirements" ~state_version:!version ~attention_id:None
        ~gameplan:parsed_gameplan
        ~main_branch:(Onton_core.Types.Branch.of_string "main")
    with
    | Ok p -> p
    | Error error -> failwith error
  in
  check "validated projection has feature mode and bound exact notes"
    (projection.Worker_checkpoint.execution_mode = "feature"
    && projection.Worker_checkpoint.root_role = "patch"
    && projection.Worker_checkpoint.note_bytes = Some "EXACT NOTES\n"
    && Option.is_some projection.Worker_checkpoint.publication);
  check "current base retargeted during captured Start"
    (!input_delivered
    && Option.exists
         (fun b -> Onton_core.Types.Branch.to_string b = "main")
         restored_agent.Onton_core.Patch_agent.base_branch);
  let current_context = context ~base:"main" ~generation:2 ~input_version:1 in
  ignore
    (execute 2 ~context:current_context ~input_version:1
       (`Assoc [ ("type", `String "reconcile") ]));
  let updated =
    match Worker_checkpoint.decode (Yojson.Safe.from_string !capsule) with
    | Ok c -> c
    | Error e -> failwith e
  in
  check "checkpoint retains current input separately from historical bind"
    (Option.exists
       (fun i -> i.V.selected_base = "main" && i.V.guidance = "new guidance")
       (Worker_checkpoint.v3_input updated));
  let agent =
    match
      Worker_checkpoint.validate_binding updated ~run_id:"run-1"
        ~patch_id:(Onton_core.Types.Patch_id.of_string "2")
        ~context:"requirements" ~state_version:!version ~attention_id:None
        ~gameplan:parsed_gameplan
        ~main_branch:(Onton_core.Types.Branch.of_string "main")
    with
    | Ok a -> a
    | Error e -> failwith e
  in
  let next_projection =
    match
      Worker_checkpoint.project updated ~run_id:"run-1"
        ~patch_id:(Onton_core.Types.Patch_id.of_string "2")
        ~context:"requirements" ~state_version:!version ~attention_id:None
        ~gameplan:parsed_gameplan
        ~main_branch:(Onton_core.Types.Branch.of_string "main")
    with
    | Ok p -> p
    | Error error -> failwith error
  in
  check "old note cannot be credited to a newer assignment"
    (next_projection.Worker_checkpoint.note_bytes = None);
  check "retargeted desired base survives completion"
    (Option.exists
       (fun b -> Onton_core.Types.Branch.to_string b = "main")
       agent.Onton_core.Patch_agent.base_branch);
  send input (frame "shutdown" []);
  check "v3 worker shutdown"
    (Unix.close_process_full (output, input, errors) = Unix.WEXITED 0);
  print_endline
    "v3 real worker: admitted dependent Start, retained mode/base and \
     sequential reuse";
  let fresh = Filename.concat root "replacement-clone" in
  Onton_test_support.Git_env.sh ~dir:"/"
    (Printf.sprintf "git clone --no-local -q %s %s" (Filename.quote origin)
       (Filename.quote fresh));
  Onton_test_support.Git_env.run_git ~cwd:fresh
    [ "config"; "user.name"; "Replacement Worker" ];
  Onton_test_support.Git_env.run_git ~cwd:fresh
    [ "config"; "user.email"; "replacement@example.invalid" ];
  let replacement_command =
    String.concat " "
      (List.map Filename.quote
         [
           binary;
           "--gameplan";
           gameplan;
           "--repo-root";
           fresh;
           "--project-name";
           "v3-test";
           "--protocol-version";
           "3";
           "--ack-timeout";
           "3";
         ])
  in
  let replacement_output, replacement_input, replacement_errors =
    Unix.open_process_full replacement_command env
  in
  let replacement_host = ref (V.initial ()) in
  let replacement_bind =
    frame "bind"
      [
        ("worker_id", `String "worker-2");
        ("workspace_id", `String "workspace-2");
        ("context", `String "requirements");
        ("capsule", `String !capsule);
        ("state_version", `String (Int64.to_string !version));
        ("attention_id", `Null);
        ("admission", Option.get (V.admission_receipt !host));
        ("execution_context", `String bind_context);
      ]
  in
  ignore (step replacement_host (V.Host (decode replacement_bind)));
  send replacement_input replacement_bind;
  check "replacement restored acknowledged idle receipts"
    (str "type" (recv replacement_output) = "ready");
  ignore (step replacement_host (V.Host (decode input_frame)));
  ignore (step replacement_host (V.Input_committed ("input-1", 1L)));
  send replacement_input input_frame;
  let replacement_request =
    frame "execute"
      (op 3
      @ [
          ("expected_state_version", `String (Int64.to_string !version));
          ("work", `Assoc [ ("type", `String "reconcile") ]);
          ("guidance", `String "fresh operation after idle loss");
          ("recovery_mode", `String "lost_workspace");
          ("renewal_id", `Null);
          ("renewed_intent", `Bool false);
          ("execution_context", `String current_context);
          ("input_version", `String "1");
        ])
  in
  check "new logical operation receives distinct assignment"
    (List.exists
       (function V.Begin _ -> true | _ -> false)
       (step replacement_host (V.Host (decode replacement_request))));
  send replacement_input replacement_request;
  check "replacement accepts new operation"
    (str "type" (recv replacement_output) = "accepted");
  let rec replacement_complete checkpoints =
    let msg = recv replacement_output in
    match str "type" msg with
    | "checkpoint" ->
        let bytes = str "capsule" msg in
        let next = Int64.of_string (str "state_version" msg) in
        let proposal_id = str "proposal_id" msg in
        ignore
          (step replacement_host
             (V.Worker_event
                (P.Propose_checkpoint { proposal_id; capsule = bytes })));
        let ack =
          frame "checkpoint_ack"
            (op 3
            @ [
                ("proposal_id", `String proposal_id);
                ("state_version", `String (Int64.to_string next));
                ("capsule", `String bytes);
                ("accepted", `Bool true);
              ])
        in
        ignore (step replacement_host (V.Host (decode ack)));
        version := next;
        capsule := bytes;
        send replacement_input ack;
        replacement_complete (checkpoints + 1)
    | "observation" -> replacement_complete checkpoints
    | "result" ->
        check "replacement checkpoint before result" (checkpoints > 0);
        check "idle replacement did not replay backend turn"
          (List.mem (str "status" msg) [ "completed_ok"; "retryable" ]);
        let status =
          if str "status" msg = "retryable" then P.Retryable else P.Completed_ok
        in
        ignore (step replacement_host (V.Worker_event P.Cleanup_complete));
        ignore
          (step replacement_host
             (V.Worker_event
                (P.Finish
                   {
                     status;
                     diagnostics = str "diagnostics" msg;
                     renewal_id = None;
                   })));
        let ack =
          frame "result_ack"
            (op 3 @ [ ("state_version", `String (Int64.to_string !version)) ])
        in
        ignore (step replacement_host (V.Host (decode ack)));
        send replacement_input ack
    | other -> failwith ("unexpected idle replacement output " ^ other)
  in
  replacement_complete 0;
  send replacement_input (frame "shutdown" []);
  check "replacement shutdown"
    (Unix.close_process_full
       (replacement_output, replacement_input, replacement_errors)
    = Unix.WEXITED 0);
  print_endline "v3 worker: acknowledged idle loss uses a new assignment";
  let child_revision =
    Onton_test_support.Git_env.git_capture ~cwd:origin
      [ "rev-parse"; "refs/heads/v3-test/patch-2" ]
  in
  let root_clone = Filename.concat root "root-clone" in
  Onton_test_support.Git_env.sh ~dir:"/"
    (Printf.sprintf "git clone --no-local -q %s %s" (Filename.quote origin)
       (Filename.quote root_clone));
  Onton_test_support.Git_env.run_git ~cwd:root_clone
    [ "config"; "user.name"; "Onton Root" ];
  Onton_test_support.Git_env.run_git ~cwd:root_clone
    [ "config"; "user.email"; "root@example.invalid" ];
  let root_command =
    String.concat " "
      (List.map Filename.quote
         [
           binary;
           "--gameplan";
           gameplan;
           "--repo-root";
           root_clone;
           "--project-name";
           "v3-test-root";
           "--protocol-version";
           "3";
           "--ack-timeout";
           "3";
         ])
  in
  let root_output, root_input, root_errors =
    Unix.open_process_full root_command env
  in
  let root_host = ref (V.initial ()) in
  let root_context =
    context ~base:"main" ~generation:1 ~input_version:0
    |> Yojson.Safe.from_string
    |> replace "patch_id" (`String "1")
    |> replace "dependencies" (`List [])
    |> replace "root_role" (`String "root")
    |> Yojson.Safe.to_string
  in
  let root_frame kind fields =
    replace "patch_id" (`String "1") (frame kind fields)
  in
  let root_bind =
    root_frame "bind"
      [
        ("worker_id", `String "root-worker");
        ("workspace_id", `String "root-workspace");
        ("context", `String "root requirements");
        ("capsule", `String "{}");
        ("state_version", `String "0");
        ("attention_id", `Null);
        ("admission", `Assoc [ ("mode", `String "fresh") ]);
        ("execution_context", `String root_context);
      ]
  in
  ignore (step root_host (V.Host (decode root_bind)));
  send root_input root_bind;
  check "root worker ready" (str "type" (recv root_output) = "ready");
  let root_execute =
    root_frame "execute"
      (op 1
      @ [
          ("expected_state_version", `String "0");
          ( "work",
            `Assoc
              [
                ("type", `String "integrate_revision");
                ("contributor", `String "2");
                ("revision", `String child_revision);
              ] );
          ("guidance", `String "integrate captured child");
          ("recovery_mode", `String "retained_workspace");
          ("renewal_id", `Null);
          ("renewed_intent", `Bool false);
          ("execution_context", `String root_context);
          ("input_version", `String "0");
        ])
  in
  check "root model admits integration"
    (List.exists
       (function V.Begin _ -> true | _ -> false)
       (step root_host (V.Host (decode root_execute))));
  send root_input root_execute;
  check "root worker accepts captured integration"
    (str "type" (recv root_output) = "accepted");
  let root_version = ref 0L and root_capsule = ref "{}" in
  let rec root_complete checkpoints =
    let message = recv root_output in
    match str "type" message with
    | "checkpoint" ->
        let bytes = str "capsule" message in
        let next = Int64.of_string (str "state_version" message) in
        let proposal_id = str "proposal_id" message in
        ignore
          (step root_host
             (V.Worker_event
                (P.Propose_checkpoint { proposal_id; capsule = bytes })));
        let ack =
          root_frame "checkpoint_ack"
            (op 1
            @ [
                ("proposal_id", `String proposal_id);
                ("state_version", `String (Int64.to_string next));
                ("capsule", `String bytes);
                ("accepted", `Bool true);
              ])
        in
        ignore (step root_host (V.Host (decode ack)));
        root_version := next;
        root_capsule := bytes;
        send root_input ack;
        root_complete (checkpoints + 1)
    | "observation" -> root_complete checkpoints
    | "result" ->
        check "root result follows checkpoint" (checkpoints > 0);
        check
          ("root integration result: " ^ str "diagnostics" message)
          (str "status" message = "completed_ok");
        ignore (step root_host (V.Worker_event P.Cleanup_complete));
        ignore
          (step root_host
             (V.Worker_event
                (P.Finish
                   {
                     status = P.Completed_ok;
                     diagnostics = "";
                     renewal_id = None;
                   })));
        let ack =
          root_frame "result_ack"
            (op 1
            @ [ ("state_version", `String (Int64.to_string !root_version)) ])
        in
        ignore (step root_host (V.Host (decode ack)));
        send root_input ack
    | other -> failwith ("unexpected root worker message " ^ other)
  in
  root_complete 0;
  let root_state =
    match Worker_checkpoint.decode (Yojson.Safe.from_string !root_capsule) with
    | Ok state -> state
    | Error error -> failwith error
  in
  check "root capsule bound to feature/root role"
    (Worker_checkpoint.v3_context root_state = Some root_context);
  check "origin root includes captured contributor"
    (Onton_test_support.Git_env.git_exit_code ~cwd:origin
       [ "merge-base"; "--is-ancestor"; child_revision; "v3-test/patch-1" ]
    = 0);
  send root_input (root_frame "shutdown" []);
  check "root worker shutdown"
    (Unix.close_process_full (root_output, root_input, root_errors)
    = Unix.WEXITED 0);
  print_endline "v3 root worker: captured revision through owner reconciliation"
