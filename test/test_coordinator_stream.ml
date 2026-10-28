(* @archlint.module test
   @archlint.domain coordinator-stream *)
open Onton
open Onton_core

[@@@warning "-4-40-42"]

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
         ("config_id", `String "cfg");
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
  let process_command =
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
  let output, input, errors = Unix.open_process_full process_command env in
  let output = ref output and input = ref input and errors = ref errors in

  let module F = Onton_test_support.Coordinator_store_fixture in
  let parsed =
    match Onton_core.Gameplan_parser.parse_file gameplan with
    | Ok p -> p
    | Error e -> failwith e
  in
  let gameplan = parsed.Onton_core.Gameplan_parser.gameplan in
  let main_branch = Types.Branch.of_string "main" in
  let f = F.fixture () in
  let store, state, next_id = F.setup ~run_id:"run-1" f in
  ignore
    (F.committed
       (Coordinator_store.commit store ~expected_version:0L ~write_id:"initial"
          (Coordinator_store.initial state)));
  let restore store =
    match Coordinator_host.restore ~store ~next_write_id:next_id with
    | Ok h -> h
    | Error _ -> failwith "restore"
  in
  let host = ref (restore store) in
  let apply event =
    match Coordinator_host.submit !host event with
    | Ok (Coordinator_host.Applied _) -> ()
    | _ -> failwith "coordinator intent rejected"
  in
  let patch = Types.Patch_id.of_string "2" in
  let operation n : Onton_core.Coordinator_state.operation =
    {
      id = "operation-" ^ string_of_int n;
      patch;
      used = 0;
      budget = 3;
      retry_after = 0L;
      attention = false;
      unfinished = true;
      maintenance = false;
    }
  in
  apply (Admit (operation 1));
  let launch resource =
    Printf.sprintf
      {|{"version":"resource-v1","op":"launch","run_id":"run-1","request_id":"launch-%s","coordinator_id":"c","owner_epoch":"1","revision":"1","chain_id":"chain-%s","patch_id":"2","resource_id":"%s","launch_digest":"digest","manifest":{"executable":"worker"}}|}
      resource resource resource
  in
  apply (Resource_request ("resource-1", launch "resource-1"));
  apply (Bind_resource ("resource-1", patch));
  let assignment n resource : Onton_core.Coordinator_state.assignment =
    {
      id = "assignment-" ^ string_of_int n;
      operation = "operation-" ^ string_of_int n;
      patch;
      resource;
      request = "attempt-" ^ string_of_int n;
      reserved = true;
      retired = false;
      result_committed = false;
      cleanup = false;
    }
  in
  apply (Assign (assignment 1 "resource-1", 0L));
  let command assignment json () =
    Coordinator_host.command !host ~assignment ~frame:(wire json)
  in
  let receive assignment json () =
    Coordinator_host.receive !host ~assignment ~frame:(wire json) ~gameplan
      ~main_branch ~now:0L
  in
  let send_committed = function
    | Error (Coordinator_host.Invalid e) -> failwith e
    | Error _ -> failwith "unconfirmed barrier"
    | Ok (_, frames) ->
        List.iter
          (fun frame ->
            output_string !input frame;
            flush !input)
          frames
  in
  let barrier ?(deliver = true) label operation =
    let before = Coordinator_store.encode (Coordinator_host.envelope !host) in
    f.reject_write <- true;
    check
      (label ^ ": rejected write released a frame")
      (Result.is_error (operation ()));
    check
      (label ^ ": rejected memory")
      (Coordinator_store.encode (Coordinator_host.envelope !host) = before);
    f.reject_write <- false;
    f.drop_write <- true;
    f.refuse_receipt <- true;
    check
      (label ^ ": lost/unconfirmed write released a frame")
      (Result.is_error (operation ()));
    check
      (label ^ ": uncertain memory")
      (Coordinator_store.encode (Coordinator_host.envelope !host) = before);
    f.drop_write <- false;
    f.refuse_receipt <- false;
    f.refuse_read <- true;
    check
      (label ^ ": receipt without exact readback released a frame")
      (Result.is_error (operation ()));
    check
      (label ^ ": missing readback published memory")
      (Coordinator_store.encode (Coordinator_host.envelope !host) = before);
    f.refuse_read <- false;
    let result = operation () in
    if deliver then send_committed result
    else check (label ^ ": commit failed") (Result.is_ok result);
    let new_store, _, _ = F.setup ~run_id:"run-1" f in
    let replacement = restore new_store in
    check
      (label ^ ": exact restore")
      (Coordinator_store.encode (Coordinator_host.envelope replacement)
      = Coordinator_store.encode (Coordinator_host.envelope !host));
    host := replacement
  in
  let bind_context =
    context ~base:"v3-test/patch-1" ~generation:1 ~input_version:0
  in
  let bind =
    frame "bind"
      [
        ("worker_id", `String "worker-1");
        ("workspace_id", `String "workspace-1");
        ("context", `String "requirements");
        ("capsule", `String "{}");
        ("state_version", `String "0");
        ("attention_id", `Null);
        ("admission", `Assoc [ ("mode", `String "fresh") ]);
        ("execution_context", `String bind_context);
      ]
  in
  barrier "bind" (command "assignment-1" bind);
  send_committed (receive "assignment-1" (recv !output) ());
  let version = ref 0L in
  let work =
    `Assoc
      [
        ("type", `String "session");
        ("phase", `String "start");
        ("kind", `Null);
        ("turn", `String "DEPENDENCY-UNBLOCKED");
      ]
  in
  let input_delivered = ref false in
  let input_frame =
    frame "input"
      [
        ("input_id", `String "input-1");
        ("input_version", `String "1");
        ("guidance", `String "current durable guidance");
        ("selected_base", `String "main");
      ]
  in
  let execute n work =
    frame "execute"
      (op n
      @ [
          ("expected_state_version", `String (Int64.to_string !version));
          ("work", work);
          ("guidance", `String "Do assigned work");
          ( "recovery_mode",
            `String (if n = 3 then "lost_workspace" else "retained_workspace")
          );
          ("renewal_id", `Null);
          ("renewed_intent", `Bool false);
          ( "execution_context",
            `String
              (if !input_delivered then
                 context ~base:"main" ~generation:2 ~input_version:1
               else bind_context) );
          ("input_version", `String (if !input_delivered then "1" else "0"));
        ])
  in
  barrier "admission" (command "assignment-1" (execute 1 work));
  let checkpoints = ref 0 in
  let rec consume assignment =
    let json =
      try recv !output
      with End_of_file ->
        failwith ("worker EOF: " ^ In_channel.input_all !errors)
    in
    match str "type" json with
    | "checkpoint" ->
        incr checkpoints;
        version := Int64.of_string (str "state_version" json);
        let count = List.length f.writes in
        let capsule = Yojson.Safe.from_string (str "capsule" json) in
        let changed =
          replace "capsule"
            (`String
               (Yojson.Safe.to_string
                  (replace "run_id" (`String "another-run") capsule)))
            json
        in
        check "unbound capsule accepted"
          (Result.is_error (receive assignment changed ()));
        check "stale attempt accepted"
          (Result.is_error
             (receive assignment
                (replace "attempt_id" (`String "stale-attempt") json)
                ()));
        check "invalid capsule/frame committed" (List.length f.writes = count);
        barrier "checkpoint" (receive assignment json);
        (* Exact duplicate may resend its already committed ACK, without another
           write or caller-constructed receipt. *)
        let count = List.length f.writes in
        check "duplicate checkpoint" (Result.is_ok (receive assignment json ()));
        check "duplicate checkpoint wrote again" (List.length f.writes = count);
        if not !input_delivered then (
          apply (Input ("input-1", "2", 1L, wire input_frame));
          barrier "durable input" (command assignment input_frame);
          let count = List.length f.writes in
          check "exact input replay is idempotent"
            (Result.is_ok (command assignment input_frame ()));
          check "input replay rewrote storage" (List.length f.writes = count);
          input_delivered := true);
        consume assignment
    | "result" ->
        let count = List.length f.writes in
        check "result diagnostics differ from terminal capsule"
          (Result.is_error
             (receive assignment
                (replace "diagnostics" (`String "fabricated") json)
                ()));
        check "invalid result committed" (List.length f.writes = count);
        (* First terminal adoption must be durable; a lost post-ACK write cannot
           release the Result ACK or publish cleanup in this host's memory. *)
        let snapshot =
          Coordinator_store.encode (Coordinator_host.envelope !host)
        in
        f.reject_write <- true;
        check "terminal rejection released ACK"
          (Result.is_error (receive assignment json ()));
        check "terminal rejection published memory"
          (Coordinator_store.encode (Coordinator_host.envelope !host) = snapshot);
        f.reject_write <- false;
        f.drop_write <- true;
        f.refuse_receipt <- true;
        check "terminal uncertainty released ACK"
          (Result.is_error (receive assignment json ()));
        check "terminal uncertainty published memory"
          (Coordinator_store.encode (Coordinator_host.envelope !host) = snapshot);
        f.drop_write <- false;
        f.refuse_receipt <- false;
        f.refuse_read <- true;
        check "terminal receipt without readback released ACK"
          (Result.is_error (receive assignment json ()));
        check "terminal missing readback published memory"
          (Coordinator_store.encode (Coordinator_host.envelope !host) = snapshot);
        f.refuse_read <- false;
        f.reject_write <- true;
        check "post-ACK rejection released Result ACK"
          (Result.is_error (receive assignment json ()));
        let current =
          Onton_core.Coordinator_state.assignments
            (Coordinator_host.state !host)
        in
        check "terminal committed separately"
          (List.exists
             (fun (a : Coordinator_state.assignment) ->
               a.id = assignment && a.result_committed && not a.cleanup)
             current);
        let terminal_snapshot =
          Coordinator_store.encode (Coordinator_host.envelope !host)
        in
        f.reject_write <- false;
        f.drop_write <- true;
        f.refuse_receipt <- true;
        check "post-ACK lost/unconfirmed write released Result ACK"
          (Result.is_error (receive assignment json ()));
        check "post-ACK uncertainty published memory"
          (Coordinator_store.encode (Coordinator_host.envelope !host)
          = terminal_snapshot);
        f.drop_write <- false;
        f.refuse_receipt <- false;
        f.refuse_read <- true;
        check "post-ACK receipt without readback released Result ACK"
          (Result.is_error (receive assignment json ()));
        check "post-ACK missing readback published memory"
          (Coordinator_store.encode (Coordinator_host.envelope !host)
          = terminal_snapshot);
        f.refuse_read <- false;
        send_committed (receive assignment json ());
        check "cleanup releases exact reservation"
          (Onton_core.Coordinator_state.reserved_count
             (Coordinator_host.state !host)
          = 0);
        let current = Coordinator_host.envelope !host in
        let fresh_store, _, _ = F.setup ~run_id:"run-1" f in
        host := restore fresh_store;
        check "post-ACK receipt restored exactly"
          (Coordinator_store.encode current
          = Coordinator_store.encode (Coordinator_host.envelope !host));
        let stale = replace "run_id" (`String "another-run") json in
        check "cross-run result accepted"
          (Result.is_error (receive assignment stale ()));
        json
    | "ready" | "accepted" | "observation" ->
        send_committed (receive assignment json ());
        consume assignment
    | other -> failwith ("unexpected real worker frame " ^ other)
  in
  ignore (consume "assignment-1");
  check "real worker checkpointed" (!checkpoints > 1);
  apply (Admit (operation 2));
  apply (Assign (assignment 2 "resource-1", 0L));
  barrier "sequential admission"
    (command "assignment-2"
       (execute 2 (`Assoc [ ("type", `String "reconcile") ])));
  ignore (consume "assignment-2");
  send_committed (command "assignment-2" (frame "shutdown" []) ());
  ignore (Unix.close_process_full (!output, !input, !errors));
  apply (Loss ("resource-1", 0L));
  let stop =
    match
      Resource_protocol.make_request
        {|{"version":"resource-v1","op":"stop","run_id":"run-1","request_id":"stop-old","coordinator_id":"c","owner_epoch":"1","revision":"1","resource_id":"resource-1"}|}
    with
    | Ok r -> r
    | Error e -> failwith e
  in
  apply (Resource_request (stop.request_id, stop.bytes));
  let response =
    match Resource_protocol.decode_response (F.reply "ok") with
    | Ok r -> r
    | Error e -> failwith e
  in
  (match
     Coordinator_host.retired !host ~resource_id:"resource-1" ~stop_request:stop
       ~response
   with
  | Ok (Coordinator_host.Applied _) -> ()
  | _ -> failwith "retirement");
  apply (Admit (operation 3));
  apply (Resource_request ("resource-2", launch "resource-2"));
  apply (Bind_resource ("resource-2", patch));
  apply (Assign (assignment 3 "resource-2", 0L));
  let next_output, next_input, next_errors =
    Unix.open_process_full process_command env
  in
  output := next_output;
  input := next_input;
  errors := next_errors;
  send_committed
    (Coordinator_host.recover !host ~predecessor:"assignment-2"
       ~assignment:"assignment-3" ~worker_id:"worker-2"
       ~workspace_id:"workspace-2");
  send_committed (receive "assignment-3" (recv !output) ());
  barrier "replacement admission"
    (command "assignment-3"
       (execute 3 (`Assoc [ ("type", `String "reconcile") ])));
  ignore (consume "assignment-3");
  apply (Admit (operation 4));
  apply (Assign (assignment 4 "resource-2", 0L));
  barrier "interrupted admission"
    (command "assignment-4"
       (execute 4 (`Assoc [ ("type", `String "reconcile") ])));
  send_committed (receive "assignment-4" (recv !output) ());
  let interrupted = recv !output in
  check "interruption at actual worker checkpoint"
    (str "type" interrupted = "checkpoint");
  barrier ~deliver:false "commit before lost checkpoint ACK"
    (receive "assignment-4" interrupted);
  version := Int64.of_string (str "state_version" interrupted);
  close_out !input;
  ignore (Unix.close_process_full (!output, !input, !errors));
  apply (Loss ("resource-2", 0L));
  let stop =
    match
      Resource_protocol.make_request
        {|{"version":"resource-v1","op":"stop","run_id":"run-1","request_id":"stop-interrupted","coordinator_id":"c","owner_epoch":"1","revision":"1","resource_id":"resource-2"}|}
    with
    | Ok r -> r
    | Error e -> failwith e
  in
  apply (Resource_request (stop.request_id, stop.bytes));
  (match
     Coordinator_host.retired !host ~resource_id:"resource-2" ~stop_request:stop
       ~response
   with
  | Ok (Coordinator_host.Applied _) -> ()
  | _ -> failwith "interrupted retirement");
  apply (Resource_request ("resource-3", launch "resource-3"));
  apply (Bind_resource ("resource-3", patch));
  let recovery =
    {
      (assignment 5 "resource-3") with
      operation = "operation-4";
      request = "attempt-4";
    }
  in
  apply (Assign (recovery, 1L));
  let next_output, next_input, next_errors =
    Unix.open_process_full process_command env
  in
  output := next_output;
  input := next_input;
  errors := next_errors;
  send_committed
    (Coordinator_host.recover !host ~predecessor:"assignment-4"
       ~assignment:"assignment-5" ~worker_id:"worker-3"
       ~workspace_id:"workspace-3");
  check "retired checkpoint cannot discharge replacement"
    (Result.is_error (receive "assignment-4" interrupted ()));
  ignore (consume "assignment-5");
  send_committed (command "assignment-5" (frame "shutdown" []) ());
  ignore (Unix.close_process_full (!output, !input, !errors));
  print_endline
    "coordinator real v3 stream: all commit barriers, sequential reuse, idle \
     replacement and interrupted recovery from committed state";
  (* Exercise the actual coordinator attachment/command/read loop, not only the
     host entrypoints. The public helper fixture supplies a single-use raw stream
     whose process is the real worker. *)
  let helper = Filename.concat root "stream-helper" in
  let path =
    fake_bin ^ ":" ^ Option.value (Sys.getenv_opt "PATH") ~default:""
  in
  write helper
    ("#!/bin/sh\nexec env "
    ^ String.concat " "
        (List.map Filename.quote
           [
             "HOME=" ^ root;
             "PATH=" ^ path;
             "ONTON_FAKE_PROMPT=" ^ prompt_log;
             "ONTON_DATA_DIR=" ^ Filename.concat root "loop-data";
           ])
    ^ " " ^ process_command ^ "\n");
  Unix.chmod helper 0o755;
  let loop_fixture = F.fixture () in
  let _, initial, loop_id = F.setup ~run_id:"run-1" loop_fixture in
  let client =
    Resource_client.create ~helper ~transport:(F.transport loop_fixture)
  in
  let store =
    Coordinator_store.create ~client ~run_id:"run-1" ~coordinator_id:"c"
      ~owner_epoch:1L ~revision:1L ~next_id:loop_id
  in
  ignore
    (F.committed
       (Coordinator_store.commit store ~expected_version:0L ~write_id:"initial"
          (Coordinator_store.initial initial)));
  let loop_bind =
    replace "worker_id" (`String "loop-worker") bind
    |> replace "workspace_id" (`String "loop-workspace")
  in
  let loop_execute =
    execute 1 (`Assoc [ ("type", `String "reconcile") ])
    |> replace "expected_state_version" (`String "0")
    |> replace "execution_context" (`String bind_context)
    |> replace "input_version" (`String "0")
  in
  let events =
    ref
      Coordinator.
        [
          Intent (Admit (operation 1));
          Intent (Resource_request ("loop-resource", launch "loop-resource"));
          Intent (Bind_resource ("loop-resource", patch));
          Intent (Assign (assignment 1 "loop-resource", 0L));
          Attach
            ( "assignment-1",
              {
                run_id = "run-1";
                coordinator_id = "c";
                owner_epoch = 1L;
                revision = 1L;
                request_id = "attach-loop";
                resource_id = "loop-resource";
              } );
          Command ("assignment-1", wire loop_bind);
          Command ("assignment-1", wire loop_execute);
        ]
  in
  let shutdown = ref false and reads = ref 0 in
  let readable = ref (fun () -> false) in
  let rec next_event () =
    match !events with
    | event :: rest ->
        events := rest;
        Some event
    | [] when !shutdown -> None
    | [] ->
        let current =
          match Coordinator_store.load store with
          | Ok e -> e
          | Error _ -> failwith "loop readback"
        in
        let done_ =
          List.exists
            (fun (a : Coordinator_state.assignment) -> a.cleanup)
            (Coordinator_state.assignments (Coordinator_store.state current))
        in
        if done_ then (
          shutdown := true;
          Some
            (Coordinator.Command ("assignment-1", wire (frame "shutdown" []))))
        else if !readable () then (
          incr reads;
          check "loop bounded" (!reads < 200);
          Some (Coordinator.Read "assignment-1"))
        else (
          Unix.sleepf 0.005;
          next_event ())
  in
  Coordinator.run
    {
      store;
      client;
      next_write_id = loop_id;
      next_event;
      now = (fun () -> 0L);
      stream_timeout = 3.;
      attached = (fun ~resource_id:_ ~readable:ready -> readable := ready);
      gameplan;
      main_branch;
      stop_request = (fun ~resource_id:_ -> Error "unexpected restoration");
      outcome = (fun _ -> ());
      blocked = (fun reason -> failwith ("coordinator loop: " ^ reason));
    };
  check "actual loop completed and shut down" !shutdown;
  print_endline
    "coordinator raw attachment loop: real v3 Bind/Execute/Input-capable host \
     and confirmed ACK delivery"
