(* @archlint.module test
   @archlint.domain worker-transport *)

open Onton
module P = Onton_core.Worker_protocol

[@@@warning "-4"]

let check label condition = if not condition then failwith label

let field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let replace_field name replacement = function
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (key, value) ->
             if String.equal key name then (key, replacement) else (key, value))
           fields)
  | other -> other

let string name json =
  match field name json with
  | Some (`String value) -> value
  | _ -> failwith ("missing " ^ name)

let optional_string name json =
  match field name json with
  | Some `Null -> None
  | Some (`String value) -> Some value
  | _ -> failwith ("missing optional " ^ name)

let kind json = string "type" json
let counter json = Int64.of_string (string "state_version" json)
let option_json = function None -> `Null | Some value -> `String value

let operation_fields seq =
  [
    ("operation_id", `String ("op-" ^ string_of_int seq));
    ("attempt_id", `String ("attempt-" ^ string_of_int seq));
    ("assignment_seq", `String (string_of_int seq));
  ]

let frame kind fields =
  `Assoc
    ([
       ("version", `Int 2);
       ("type", `String kind);
       ("run_id", `String "run-1");
       ("patch_id", `String "1");
     ]
    @ fields)

let wire json = Yojson.Safe.to_string json ^ "\n"

let decoded json =
  match P.decode (wire json) with
  | Ok message -> message
  | Error _ -> failwith "host fixture is invalid"

let step state event =
  let next, outputs = P.step state event in
  (next, outputs)

let has_checkpoint outputs =
  List.exists (function P.Send (P.Checkpoint _) -> true | _ -> false) outputs

let test_checkpoint_attention_identity () =
  let patch_id = Onton_core.Types.Patch_id.of_string "patch" in
  let agent =
    Onton_core.Patch_agent.create
      ~branch:(Onton_core.Types.Branch.of_string "feature")
      patch_id
  in
  let operation : P.operation =
    P.{ operation_id = "op"; attempt_id = "attempt"; assignment_seq = 1L }
  in
  let initial =
    Worker_checkpoint.initial ~run_id:"run" ~patch_id ~workspace_id:"workspace"
      ~context:"context" ~agent
  in
  let attention =
    match
      Worker_checkpoint.advance initial ~operation ~guidance:"repair"
        ~workspace_id:None ~agent
        ~terminal:(Some (P.Attention, "", Some "renew"))
    with
    | Ok capsule -> capsule
    | Error why -> failwith why
  in
  let checkpoint =
    match
      Worker_checkpoint.advance attention ~operation ~guidance:"repair"
        ~workspace_id:None ~agent ~terminal:None
    with
    | Ok capsule -> capsule
    | Error why -> failwith why
  in
  check "same-operation checkpoint preserves attention identity"
    (Option.equal String.equal
       (Worker_checkpoint.attention_id checkpoint)
       (Some "renew"))

type child = {
  output : in_channel;
  input : out_channel;
  error : in_channel;
  pending : Buffer.t;
}

type host = {
  child : child;
  mutable model : P.t;
  mutable capsule : string;
  mutable version : int64;
  mutable attention : string option;
  workspace : string;
}

let binary () =
  match Sys.getenv_opt "ONTON_WORKER_BINARY" with
  | Some value when String.length value > 0 -> value
  | _ -> (
      match Sys.getenv_opt "ONTON_WORKER_BINARY_DEFAULT" with
      | Some value -> value
      | None -> failwith "worker binary not supplied")

let spawn_with_fixture ?forge_fixture ~gameplan ~repo_root ~home ~fake_bin
    ~prompt_log () =
  let command =
    String.concat " "
      (List.map Filename.quote
         [
           binary ();
           "--gameplan";
           gameplan;
           "--repo-root";
           repo_root;
           "--project-name";
           "worker-protocol-test";
           "--ack-timeout";
           "1";
         ])
  in
  let path =
    fake_bin ^ ":" ^ Option.value (Sys.getenv_opt "PATH") ~default:""
  in
  let env =
    Array.to_list (Unix.environment ())
    |> List.filter (fun entry ->
        not
          (String.starts_with ~prefix:"HOME=" entry
          || String.starts_with ~prefix:"PATH=" entry
          || String.starts_with ~prefix:"ONTON_FAKE_PROMPT=" entry
          || String.starts_with ~prefix:"ONTON_WORKER_FORGE_FIXTURE=" entry))
    |> fun values ->
    Array.of_list
      (("HOME=" ^ home) :: ("PATH=" ^ path)
      :: ("ONTON_FAKE_PROMPT=" ^ prompt_log)
      ::
      (match forge_fixture with
      | None -> values
      | Some fixture -> ("ONTON_WORKER_FORGE_FIXTURE=" ^ fixture) :: values))
  in
  let output, input, error = Unix.open_process_full command env in
  { output; input; error; pending = Buffer.create 4096 }

let spawn ~gameplan ~repo_root ~home ~fake_bin ~prompt_log =
  spawn_with_fixture ~gameplan ~repo_root ~home ~fake_bin ~prompt_log ()

let send child json =
  output_string child.input (wire json);
  flush child.input

let available child timeout =
  if String.contains (Buffer.contents child.pending) '\n' then true
  else
    let fd = Unix.descr_of_in_channel child.output in
    match Unix.select [ fd ] [] [] timeout with [], _, _ -> false | _ -> true

let recv ?(timeout = 10.) child =
  let rec line () =
    let bytes = Buffer.contents child.pending in
    match String.index_opt bytes '\n' with
    | Some index ->
        let result = String.sub bytes 0 index in
        Buffer.clear child.pending;
        Buffer.add_substring child.pending bytes (index + 1)
          (String.length bytes - index - 1);
        Yojson.Safe.from_string result
    | None ->
        if not (available child timeout) then failwith "worker output timed out";
        let chunk = Bytes.create 4096 in
        let count =
          Unix.read (Unix.descr_of_in_channel child.output) chunk 0 4096
        in
        if count = 0 then failwith "worker output closed";
        Buffer.add_subbytes child.pending chunk 0 count;
        line ()
  in
  line ()

let finish child =
  Unix.close_process_full (child.output, child.input, child.error)

let admission host =
  match P.admission_receipt host.model with
  | Some receipt -> receipt
  | None -> `Assoc [ ("mode", `String "fresh") ]

let bind host ~context =
  let request =
    frame "bind"
      [
        ("worker_id", `String "worker-1");
        ("workspace_id", `String host.workspace);
        ("context", `String context);
        ("capsule", `String host.capsule);
        ("state_version", `String (Int64.to_string host.version));
        ("attention_id", option_json host.attention);
        ("admission", admission host);
      ]
  in
  let model, _ = step (P.initial ()) (P.Host (decoded request)) in
  host.model <- model;
  send host.child request;
  let ready = recv host.child in
  check "bind ready" (String.equal (kind ready) "ready");
  if P.recovery_pending host.model then (
    let model, outputs = step host.model P.Recover_unfinished in
    check "unfinished receipt enters observation recovery"
      (List.exists (function P.Recover _ -> true | _ -> false) outputs);
    host.model <- model)

let execute ~seq ~version ~mode ?(renewal_id = None) work =
  frame "execute"
    (operation_fields seq
    @ [
        ("expected_state_version", `String (Int64.to_string version));
        ("work", work);
        ("guidance", `String "Inspect and validate the assigned patch");
        ("recovery_mode", `String mode);
        ("renewal_id", option_json renewal_id);
        ("renewed_intent", `Bool (Option.is_some renewal_id));
      ])

let start host request =
  let model, outputs = step host.model (P.Host (decoded request)) in
  check "host model admitted work"
    (List.exists (function P.Begin _ -> true | _ -> false) outputs);
  host.model <- model;
  send host.child request;
  let accepted = recv host.child in
  check "accepted assignment" (String.equal (kind accepted) "accepted")

let acknowledge host checkpoint =
  let proposal_id = string "proposal_id" checkpoint in
  let bytes = string "capsule" checkpoint in
  let decoded_capsule =
    match Worker_checkpoint.decode (Yojson.Safe.from_string bytes) with
    | Ok capsule -> capsule
    | Error reason -> failwith ("worker emitted invalid capsule: " ^ reason)
  in
  let next_version = counter checkpoint in
  check "capsule version"
    (Int64.equal (Worker_checkpoint.version decoded_capsule) next_version);
  let capsule_json = Yojson.Safe.from_string bytes in
  (match field "agent" capsule_json with
  | Some (`Assoc agent_fields) ->
      let truncated =
        replace_field "agent"
          (`Assoc (List.remove_assoc "queue" agent_fields))
          capsule_json
      in
      check "truncated capsule agent rejected"
        (Result.is_error (Worker_checkpoint.decode truncated))
  | _ -> failwith "capsule missing agent");
  check "operation capsule needs positive version"
    (Result.is_error
       (Worker_checkpoint.decode
          (replace_field "state_version" (`String "0") capsule_json)));
  (match field "terminal" capsule_json with
  | Some `Null ->
      check "nonterminal capsule rejects stale attention"
        (Result.is_error
           (Worker_checkpoint.decode
              (replace_field "attention_id" (`String "stale") capsule_json)))
  | Some (`Assoc _) -> ()
  | _ -> failwith "capsule missing terminal");
  let model, outputs =
    step host.model (P.Propose_checkpoint { proposal_id; capsule = bytes })
  in
  check "host model sees proposal" (has_checkpoint outputs);
  let seq = int_of_string (string "assignment_seq" checkpoint) in
  let ack =
    frame "checkpoint_ack"
      (operation_fields seq
      @ [
          ("proposal_id", `String proposal_id);
          ("state_version", `String (Int64.to_string next_version));
          ("capsule", `String bytes);
          ("accepted", `Bool true);
        ])
  in
  let model, _ = step model (P.Host (decoded ack)) in
  host.model <- model;
  host.capsule <- bytes;
  host.version <- next_version;
  host.attention <- Worker_checkpoint.attention_id decoded_capsule;
  send host.child ack

let result_status = function
  | "completed_ok" -> P.Completed_ok
  | "completed_no_commits" -> P.Completed_no_commits
  | "failed" -> P.Failed
  | "cancelled" -> P.Cancelled
  | "retryable" -> P.Retryable
  | "attention" -> P.Attention
  | "merged_observed" -> P.Merged_observed
  | other -> failwith ("unknown result " ^ other)

let observe_result host result =
  let model, _ = step host.model P.Cleanup_complete in
  let model, outputs =
    step model
      (P.Finish
         {
           status = result_status (string "status" result);
           diagnostics = string "diagnostics" result;
           renewal_id = optional_string "renewal_id" result;
         })
  in
  check "host model confirms terminal"
    (List.exists (function P.Send (P.Result _) -> true | _ -> false) outputs);
  host.model <- model;
  host.attention <- optional_string "renewal_id" result

let ack_result host result =
  let seq = int_of_string (string "assignment_seq" result) in
  let ack =
    frame "result_ack"
      (operation_fields seq
      @ [ ("state_version", `String (Int64.to_string (counter result))) ])
  in
  let model, _ = step host.model (P.Host (decoded ack)) in
  host.model <- model;
  send host.child ack

let complete ?(initial_checkpoints = 0) host =
  let rec loop checkpoints =
    let message = recv host.child in
    match kind message with
    | "checkpoint" ->
        if Int.equal checkpoints 0 then
          check "checkpoint blocks effects until ACK"
            (not (available host.child 0.05));
        acknowledge host message;
        loop (checkpoints + 1)
    | "observation" ->
        check "observation has diagnostic text"
          (String.length (string "text" message) > 0);
        loop checkpoints
    | "result" ->
        check "result follows a checkpoint" (checkpoints > 0);
        observe_result host message;
        message
    | "protocol_error" ->
        failwith ("unexpected protocol error: " ^ string "reason" message)
    | other -> failwith ("unexpected worker output: " ^ other)
  in
  loop initial_checkpoints

let write_file path content =
  let output = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out output)
    (fun () -> output_string output content)

let fixture () =
  let root = Filename.temp_dir "onton-worker-protocol-" "" in
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
  let managed = Filename.concat root "managed" in
  Onton_test_support.Git_env.sh ~dir:"/"
    (Printf.sprintf "git clone --no-local -q %s %s" (Filename.quote origin)
       (Filename.quote managed));
  Onton_test_support.Git_env.run_git ~cwd:managed
    [ "config"; "user.name"; "Onton Worker Test" ];
  Onton_test_support.Git_env.run_git ~cwd:managed
    [ "config"; "user.email"; "worker-test@example.invalid" ];
  let gameplan = Filename.concat root "gameplan.yaml" in
  write_file gameplan
    "projectName: worker-protocol-test\n\
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
    \    dependsOn: []\n";
  let fake_bin = Filename.concat root "bin" in
  Unix.mkdir fake_bin 0o755;
  let fake_codex = Filename.concat fake_bin "codex" in
  write_file fake_codex
    "#!/bin/sh\n\
     printf '%s\\n' \"$@\" > \"$ONTON_FAKE_PROMPT\"\n\
     printf '%s\\n' \
     '{\"type\":\"thread.started\",\"thread_id\":\"fake-thread\"}' \
     '{\"type\":\"turn.started\"}' \
     '{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"text\":\"done\"}}' \
     '{\"type\":\"turn.completed\"}'\n";
  Unix.chmod fake_codex 0o755;
  let prompt_log = Filename.concat root "prompt.log" in
  (root, origin, managed, gameplan, fake_bin, prompt_log)

let run_basic ~root ~managed ~gameplan ~fake_bin ~prompt_log =
  let child =
    spawn ~gameplan ~repo_root:managed ~home:root ~fake_bin ~prompt_log
  in
  let host =
    {
      child;
      model = P.initial ();
      capsule = "{}";
      version = 0L;
      attention = None;
      workspace = "workspace-1";
    }
  in
  bind host ~context:"Requirements from assigned patch";
  let reconcile = `Assoc [ ("type", `String "reconcile") ] in
  let first =
    execute ~seq:1 ~version:host.version ~mode:"retained_workspace" reconcile
  in
  start host first;
  let result = complete host in
  check "first reconciliation"
    (String.equal (string "status" result) "completed_ok");
  ack_result host result;
  let second =
    execute ~seq:2 ~version:host.version ~mode:"retained_workspace" reconcile
  in
  start host second;
  let result = complete host in
  check "second reconciliation"
    (String.equal (string "status" result) "completed_ok");
  ack_result host result;
  let session =
    `Assoc
      [
        ("type", `String "session");
        ("phase", `String "start");
        ("kind", `Null);
        ("turn", `String "TURN unique");
      ]
  in
  let third =
    execute ~seq:3 ~version:host.version ~mode:"retained_workspace" session
  in
  start host third;
  let result = complete host in
  check "session classified"
    (List.mem (string "status" result)
       [ "completed_ok"; "completed_no_commits"; "retryable" ]);
  ack_result host result;
  let prompt = In_channel.with_open_bin prompt_log In_channel.input_all in
  check "real backend received turn"
    (Base.String.is_substring prompt ~substring:"TURN unique");
  check "real backend received requirements"
    (Base.String.is_substring prompt
       ~substring:"Requirements from assigned patch");
  send child (frame "shutdown" []);
  check "worker shutdown" (finish child = Unix.WEXITED 0);
  print_endline "public worker: sequential checkpoints and backend prompt";
  (host.model, host.capsule, host.version, host.attention)

let run_recovery_seed ~root ~managed ~gameplan ~fake_bin ~prompt_log =
  let child =
    spawn ~gameplan ~repo_root:managed ~home:root ~fake_bin ~prompt_log
  in
  let host =
    {
      child;
      model = P.initial ();
      capsule = "{}";
      version = 0L;
      attention = None;
      workspace = "workspace-1";
    }
  in
  bind host ~context:"Requirements from assigned patch";
  let request =
    execute ~seq:1 ~version:host.version ~mode:"retained_workspace"
      (`Assoc [ ("type", `String "reconcile") ])
  in
  start host request;
  let result = complete host in
  check "recovery seed reconciliation"
    (String.equal (string "status" result) "completed_ok");
  ack_result host result;
  send child (frame "shutdown" []);
  check "seed worker shutdown" (finish child = Unix.WEXITED 0);
  (host.model, host.capsule, host.version, host.attention)

let run_replacement ~root ~origin ~managed ~gameplan ~fake_bin ~prompt_log ~seq
    ~expect_backend (model, capsule, version, attention) =
  let patch_path =
    Filename.concat root "worktrees/worker-protocol-test/patch-1"
  in
  write_file (Filename.concat patch_path "marker") "published marker\n";
  Onton_test_support.Git_env.run_git ~cwd:patch_path [ "add"; "marker" ];
  Onton_test_support.Git_env.run_git ~cwd:patch_path
    [ "commit"; "-q"; "-m"; "published marker" ];
  let marker =
    Onton_test_support.Git_env.git_capture ~cwd:patch_path
      [ "rev-parse"; "HEAD" ]
  in
  Onton_test_support.Git_env.run_git ~cwd:patch_path
    [ "push"; "-q"; "origin"; "HEAD:refs/heads/worker-protocol-test/patch-1" ];
  Onton_test_support.Git_env.sh ~dir:"/"
    ("rm -rf " ^ Filename.quote managed ^ " "
    ^ Filename.quote (Filename.concat root "worktrees"));
  let fresh = Filename.concat root "fresh" in
  Onton_test_support.Git_env.sh ~dir:"/"
    (Printf.sprintf "git clone --no-local -q %s %s" (Filename.quote origin)
       (Filename.quote fresh));
  let child =
    spawn ~gameplan ~repo_root:fresh ~home:root ~fake_bin ~prompt_log
  in
  let host =
    { child; model; capsule; version; attention; workspace = "workspace-2" }
  in
  bind host ~context:"Requirements from assigned patch";
  let session =
    `Assoc
      [
        ("type", `String "session");
        ("phase", `String "start");
        ("kind", `Null);
        ("turn", `String "RECOVERY TURN unique");
      ]
  in
  let request =
    execute ~seq ~version:host.version ~mode:"lost_workspace" session
  in
  start host request;
  let result = complete host in
  check "typed recovery result"
    (if expect_backend then
       List.mem (string "status" result)
         [ "completed_ok"; "completed_no_commits"; "retryable" ]
     else String.equal (string "status" result) "retryable");
  ack_result host result;
  let prompt = In_channel.with_open_bin prompt_log In_channel.input_all in
  if expect_backend then (
    check "fresh recovery prompt includes loss warning"
      (Base.String.is_substring prompt ~substring:"previous workspace");
    check "fresh recovery prompt includes assigned turn"
      (Base.String.is_substring prompt ~substring:"RECOVERY TURN unique");
    check "fresh recovery starts a new conversation"
      (not (Base.String.is_substring prompt ~substring:"\nresume\n")))
  else
    check "pending repair does not invoke backend"
      (not (Base.String.is_substring prompt ~substring:"RECOVERY TURN unique"));
  let recovered_path =
    Filename.concat root "worktrees/worker-protocol-test/patch-1"
  in
  let head =
    Onton_test_support.Git_env.git_capture ~cwd:recovered_path
      [ "rev-parse"; "HEAD" ]
  in
  check "fresh clone adopts observed pushed marker" (String.equal head marker);
  send child (frame "shutdown" []);
  check "replacement shutdown" (finish child = Unix.WEXITED 0);
  print_endline
    (if expect_backend then
       "public worker: separate-clone recovery and fresh prompt"
     else "public worker: separate-clone recovery with pending repair")

let run_unfinished_replacement ~root ~managed ~gameplan ~fake_bin ~prompt_log
    (model, capsule, version, attention) =
  let child =
    spawn ~gameplan ~repo_root:managed ~home:root ~fake_bin ~prompt_log
  in
  let host =
    { child; model; capsule; version; attention; workspace = "workspace-1" }
  in
  bind host ~context:"Requirements from assigned patch";
  let request =
    execute ~seq:2 ~version:host.version ~mode:"retained_workspace"
      (`Assoc [ ("type", `String "reconcile") ])
  in
  start host request;
  let proposal = recv host.child in
  check "unacknowledged proposal" (String.equal (kind proposal) "checkpoint");
  check "disconnected worker exits nonzero" (finish child <> Unix.WEXITED 0);
  let child =
    spawn ~gameplan ~repo_root:managed ~home:root ~fake_bin ~prompt_log
  in
  let replacement = { host with child; workspace = "workspace-2" } in
  bind replacement ~context:"Requirements from assigned patch";
  let result = complete replacement in
  check "unfinished admission completes through recovery"
    (List.mem (string "status" result)
       [ "retryable"; "merged_observed"; "attention" ]);
  ack_result replacement result;
  send child (frame "shutdown" []);
  check "unfinished replacement shutdown" (finish child = Unix.WEXITED 0);
  print_endline "public worker: unfinished admission replacement"

let expect_protocol_error child label =
  let response = recv child in
  check label (String.equal (kind response) "protocol_error")

let run_frame_faults ~root ~managed ~gameplan ~fake_bin ~prompt_log =
  let child =
    spawn ~gameplan ~repo_root:managed ~home:root ~fake_bin ~prompt_log
  in
  output_string child.input "{\"version\":2\n";
  flush child.input;
  expect_protocol_error child "malformed frame rejected";
  let host =
    {
      child;
      model = P.initial ();
      capsule = "{}";
      version = 0L;
      attention = None;
      workspace = "workspace-1";
    }
  in
  bind host ~context:"Requirements from assigned patch";
  send child
    (execute ~seq:1 ~version:host.version ~mode:"retained_workspace"
       (`Assoc [ ("type", `String "root_integration") ]));
  expect_protocol_error child "unsupported operation rejected";
  check "unsupported operation has no checkpoint" (not (available child 0.05));
  send child (frame "shutdown" []);
  check "malformed frame worker remains usable" (finish child = Unix.WEXITED 0);
  let child =
    spawn ~gameplan ~repo_root:managed ~home:root ~fake_bin ~prompt_log
  in
  send child
    (`Assoc
       [
         ("version", `Int 1);
         ("type", `String "bind");
         ("run_id", `String "run-1");
         ("patch_id", `String "1");
       ]);
  expect_protocol_error child "incompatible version rejected";
  check "incompatible version closes worker" (finish child <> Unix.WEXITED 0);
  let child =
    spawn ~gameplan ~repo_root:managed ~home:root ~fake_bin ~prompt_log
  in
  output_string child.input (String.make ((1024 * 1024) + 1) 'x');
  output_char child.input '\n';
  flush child.input;
  expect_protocol_error child "oversized frame rejected";
  check "oversized frame closes worker" (finish child <> Unix.WEXITED 0);
  print_endline "public worker: malformed, unsupported, version and size faults"

let run_checkpoint_rejection ~root ~managed ~gameplan ~fake_bin ~prompt_log =
  let child =
    spawn ~gameplan ~repo_root:managed ~home:root ~fake_bin ~prompt_log
  in
  let host =
    {
      child;
      model = P.initial ();
      capsule = "{}";
      version = 0L;
      attention = None;
      workspace = "workspace-1";
    }
  in
  bind host ~context:"Requirements from assigned patch";
  let request =
    execute ~seq:1 ~version:host.version ~mode:"retained_workspace"
      (`Assoc
         [
           ("type", `String "session");
           ("phase", `String "start");
           ("kind", `Null);
           ("turn", `String "REJECTED TURN");
         ])
  in
  start host request;
  let proposal = recv child in
  check "first frame is a checkpoint"
    (String.equal (kind proposal) "checkpoint");
  let rejected =
    frame "checkpoint_ack"
      (operation_fields 1
      @ [
          ("proposal_id", `String (string "proposal_id" proposal));
          ("state_version", `String (string "state_version" proposal));
          ("capsule", `String (string "capsule" proposal));
          ("accepted", `Bool false);
        ])
  in
  send child rejected;
  check "rejected checkpoint terminates worker" (finish child <> Unix.WEXITED 0);
  check "rejected admission does not run backend"
    (not (Sys.file_exists prompt_log));
  print_endline "public worker: rejected admission checkpoint blocks backend"

let run_admission_faults ~root ~managed ~gameplan ~fake_bin ~prompt_log =
  let child =
    spawn ~gameplan ~repo_root:managed ~home:root ~fake_bin ~prompt_log
  in
  let host =
    {
      child;
      model = P.initial ();
      capsule = "{}";
      version = 0L;
      attention = None;
      workspace = "workspace-1";
    }
  in
  bind host ~context:"Requirements from assigned patch";
  let work = `Assoc [ ("type", `String "reconcile") ] in
  let first =
    execute ~seq:1 ~version:host.version ~mode:"retained_workspace" work
  in
  start host first;
  let proposal = recv child in
  check "active proposal" (String.equal (kind proposal) "checkpoint");
  send child first;
  let replay = recv child in
  check "exact active replay is acceptance only"
    (String.equal (kind replay) "accepted");
  send child
    (execute ~seq:2 ~version:host.version ~mode:"retained_workspace" work);
  expect_protocol_error child "overlap rejected";
  send child
    (execute ~seq:1 ~version:host.version ~mode:"retained_workspace"
       (`Assoc
          [
            ("type", `String "session");
            ("phase", `String "start");
            ("kind", `Null);
            ("turn", `String "conflicting reuse");
          ]));
  expect_protocol_error child "changed identity reuse rejected";
  acknowledge host proposal;
  let result = complete ~initial_checkpoints:1 host in
  ack_result host result;
  send child (frame "cancel" (operation_fields 1));
  check "stale cancel does not produce another result"
    (not (available child 0.05));
  let stale_ack =
    frame "result_ack"
      (operation_fields 2
      @ [ ("state_version", `String (Int64.to_string host.version)) ])
  in
  send child stale_ack;
  expect_protocol_error child "stale result ACK rejected";
  let second =
    execute ~seq:2 ~version:host.version ~mode:"retained_workspace" work
  in
  start host second;
  let result = complete host in
  ack_result host result;
  send child (frame "shutdown" []);
  check "admission fault worker stays usable" (finish child = Unix.WEXITED 0);
  print_endline "public worker: overlap, replay and stale controls"

let run_attention_renewal ~root ~origin ~managed ~gameplan ~fake_bin ~prompt_log
    ~merge_after_attention (model, capsule, version, attention) =
  let branch = "worker-protocol-test/patch-1" in
  Onton_test_support.Git_env.run_git ~cwd:managed
    [ "push"; "-q"; "origin"; "--delete"; branch ];
  Onton_test_support.Git_env.sh ~dir:"/"
    ("rm -rf " ^ Filename.quote managed ^ " "
    ^ Filename.quote (Filename.concat root "worktrees"));
  let fresh = Filename.concat root "fresh" in
  Onton_test_support.Git_env.sh ~dir:"/"
    (Printf.sprintf "git clone --no-local -q %s %s" (Filename.quote origin)
       (Filename.quote fresh));
  let fixture_path = Filename.concat root "forge.json" in
  write_file fixture_path
    "{\"branch\":\"worker-protocol-test/patch-1\",\"state\":\"not_merged\"}";
  let child =
    spawn_with_fixture ~forge_fixture:fixture_path ~gameplan ~repo_root:fresh
      ~home:root ~fake_bin ~prompt_log ()
  in
  let host =
    { child; model; capsule; version; attention; workspace = "workspace-2" }
  in
  bind host ~context:"Requirements from assigned patch";
  let work =
    `Assoc
      [
        ("type", `String "session");
        ("phase", `String "start");
        ("kind", `Null);
        ("turn", `String "RENEWED TURN");
      ]
  in
  let first =
    execute ~seq:2 ~version:host.version ~mode:"lost_workspace" work
  in
  start host first;
  let result = complete host in
  check "disappeared publication requests attention"
    (String.equal (string "status" result) "attention");
  let renewal_id =
    match optional_string "renewal_id" result with
    | Some value -> value
    | None -> failwith "attention omitted renewal receipt"
  in
  check "attention did not invoke backend" (not (Sys.file_exists prompt_log));
  ack_result host result;
  send child (execute ~seq:3 ~version:host.version ~mode:"lost_workspace" work);
  expect_protocol_error child "plain retry cannot renew disappearance";
  send child
    (execute ~seq:3 ~version:host.version ~mode:"lost_workspace"
       ~renewal_id:(Some "wrong-receipt") work);
  expect_protocol_error child "wrong renewal receipt rejected";
  if merge_after_attention then
    write_file fixture_path
      "{\"branch\":\"worker-protocol-test/patch-1\",\"state\":\"merged\"}";
  let renewed =
    execute ~seq:3 ~version:host.version ~mode:"lost_workspace"
      ~renewal_id:(Some renewal_id) work
  in
  start host renewed;
  let result = complete host in
  if merge_after_attention then (
    check "confirmed merge wins renewed intent"
      (String.equal (string "status" result) "merged_observed");
    check "merge observation invokes no backend"
      (not (Sys.file_exists prompt_log)))
  else (
    check "renewed operation invokes fresh backend" (Sys.file_exists prompt_log);
    check "renewed result is typed"
      (List.mem (string "status" result)
         [ "completed_ok"; "completed_no_commits"; "retryable" ]);
    let prompt = In_channel.with_open_bin prompt_log In_channel.input_all in
    check "renewed prompt carries loss context"
      (Base.String.is_substring prompt ~substring:"previous workspace"));
  ack_result host result;
  (if not merge_after_attention then
     let recovered_path =
       Filename.concat root "worktrees/worker-protocol-test/patch-1"
     in
     let head =
       Onton_test_support.Git_env.git_capture ~cwd:recovered_path
         [ "rev-parse"; "HEAD" ]
     in
     let base =
       Onton_test_support.Git_env.git_capture ~cwd:fresh
         [ "rev-parse"; "refs/remotes/origin/main" ]
     in
     check "renewal recreates from confirmed current base"
       (String.equal head base));
  send child (frame "shutdown" []);
  check "renewal worker shutdown" (finish child = Unix.WEXITED 0);
  print_endline
    (if merge_after_attention then
       "public worker: merge observation wins renewed intent"
     else "public worker: attention requires explicit renewed intent")

let run_cancellation ~root ~managed ~gameplan ~fake_bin ~prompt_log
    ~shutdown_active =
  write_file
    (Filename.concat fake_bin "codex")
    "#!/bin/sh\n\
     printf '%s\\n' \"$@\" > \"$ONTON_FAKE_PROMPT\"\n\
     sleep 30 &\n\
     echo $! > \"$ONTON_FAKE_PROMPT.child\"\n\
     wait\n";
  let child =
    spawn ~gameplan ~repo_root:managed ~home:root ~fake_bin ~prompt_log
  in
  let host =
    {
      child;
      model = P.initial ();
      capsule = "{}";
      version = 0L;
      attention = None;
      workspace = "workspace-1";
    }
  in
  bind host ~context:"Requirements from assigned patch";
  let request =
    execute ~seq:1 ~version:host.version ~mode:"retained_workspace"
      (`Assoc
         [
           ("type", `String "session");
           ("phase", `String "start");
           ("kind", `Null);
           ("turn", `String "CANCEL TURN");
         ])
  in
  start host request;
  let rec await_backend remaining =
    if Sys.file_exists (prompt_log ^ ".child") then ()
    else if remaining = 0 then failwith "backend did not start"
    else if not (available child 0.1) then await_backend (remaining - 1)
    else
      let message = recv child in
      match kind message with
      | "checkpoint" ->
          acknowledge host message;
          await_backend (remaining - 1)
      | other -> failwith ("unexpected frame before cancellation: " ^ other)
  in
  await_backend 100;
  let child_pid =
    int_of_string
      (String.trim
         (In_channel.with_open_bin (prompt_log ^ ".child") In_channel.input_all))
  in
  (if shutdown_active then (
     let started = Unix.gettimeofday () in
     send child (frame "shutdown" []);
     check "active shutdown exits cleanly" (finish child = Unix.WEXITED 0);
     check "active shutdown does not wait for checkpoint timeout"
       (Unix.gettimeofday () -. started < 5.))
   else
     let cancel = frame "cancel" (operation_fields 1) in
     let model, outputs = step host.model (P.Host (decoded cancel)) in
     check "cancel targets active operation"
       (List.exists (function P.Cancel_active -> true | _ -> false) outputs);
     host.model <- model;
     send child cancel;
     let result =
       try complete host
       with End_of_file ->
         failwith
           ("cancelled worker exited: " ^ In_channel.input_all child.error)
     in
     check "cancelled result is typed"
       (String.equal (string "status" result) "cancelled");
     ack_result host result;
     send child (frame "shutdown" []);
     check "cancelled worker shuts down" (finish child = Unix.WEXITED 0));
  let rec child_stopped attempts =
    try
      Unix.kill child_pid 0;
      if attempts = 0 then false
      else (
        ignore (Unix.select [] [] [] 0.05);
        child_stopped (attempts - 1))
    with Unix.Unix_error (Unix.ESRCH, _, _) -> true
  in
  check "backend descendant terminated" (child_stopped 100);
  print_endline
    (if shutdown_active then "public worker: active shutdown stops descendant"
     else "public worker: cancellation stops backend descendant")

let () =
  test_checkpoint_attention_identity ();
  let root, origin, managed, gameplan, fake_bin, prompt_log = fixture () in
  Fun.protect
    ~finally:(fun () ->
      Onton_test_support.Git_env.sh ~dir:"/" ("rm -rf " ^ Filename.quote root))
    (fun () ->
      let committed =
        run_basic ~root ~managed ~gameplan ~fake_bin ~prompt_log
      in
      run_replacement ~root ~origin ~managed ~gameplan ~fake_bin ~prompt_log
        ~seq:4 ~expect_backend:false committed);
  let root, origin, managed, gameplan, fake_bin, prompt_log = fixture () in
  Fun.protect
    ~finally:(fun () ->
      Onton_test_support.Git_env.sh ~dir:"/" ("rm -rf " ^ Filename.quote root))
    (fun () ->
      let committed =
        run_recovery_seed ~root ~managed ~gameplan ~fake_bin ~prompt_log
      in
      run_replacement ~root ~origin ~managed ~gameplan ~fake_bin ~prompt_log
        ~seq:2 ~expect_backend:true committed);
  let root, _origin, managed, gameplan, fake_bin, prompt_log = fixture () in
  Fun.protect
    ~finally:(fun () ->
      Onton_test_support.Git_env.sh ~dir:"/" ("rm -rf " ^ Filename.quote root))
    (fun () ->
      let committed =
        run_recovery_seed ~root ~managed ~gameplan ~fake_bin ~prompt_log
      in
      run_unfinished_replacement ~root ~managed ~gameplan ~fake_bin ~prompt_log
        committed);
  let root, _origin, managed, gameplan, fake_bin, prompt_log = fixture () in
  Fun.protect
    ~finally:(fun () ->
      Onton_test_support.Git_env.sh ~dir:"/" ("rm -rf " ^ Filename.quote root))
    (fun () -> run_frame_faults ~root ~managed ~gameplan ~fake_bin ~prompt_log);
  let root, _origin, managed, gameplan, fake_bin, prompt_log = fixture () in
  Fun.protect
    ~finally:(fun () ->
      Onton_test_support.Git_env.sh ~dir:"/" ("rm -rf " ^ Filename.quote root))
    (fun () ->
      run_checkpoint_rejection ~root ~managed ~gameplan ~fake_bin ~prompt_log);
  let root, _origin, managed, gameplan, fake_bin, prompt_log = fixture () in
  Fun.protect
    ~finally:(fun () ->
      Onton_test_support.Git_env.sh ~dir:"/" ("rm -rf " ^ Filename.quote root))
    (fun () ->
      run_admission_faults ~root ~managed ~gameplan ~fake_bin ~prompt_log);
  let root, origin, managed, gameplan, fake_bin, prompt_log = fixture () in
  Fun.protect
    ~finally:(fun () ->
      Onton_test_support.Git_env.sh ~dir:"/" ("rm -rf " ^ Filename.quote root))
    (fun () ->
      let committed =
        run_recovery_seed ~root ~managed ~gameplan ~fake_bin ~prompt_log
      in
      run_attention_renewal ~root ~origin ~managed ~gameplan ~fake_bin
        ~prompt_log ~merge_after_attention:false committed);
  let root, origin, managed, gameplan, fake_bin, prompt_log = fixture () in
  Fun.protect
    ~finally:(fun () ->
      Onton_test_support.Git_env.sh ~dir:"/" ("rm -rf " ^ Filename.quote root))
    (fun () ->
      let committed =
        run_recovery_seed ~root ~managed ~gameplan ~fake_bin ~prompt_log
      in
      run_attention_renewal ~root ~origin ~managed ~gameplan ~fake_bin
        ~prompt_log ~merge_after_attention:true committed);
  let root, _origin, managed, gameplan, fake_bin, prompt_log = fixture () in
  Fun.protect
    ~finally:(fun () ->
      Onton_test_support.Git_env.sh ~dir:"/" ("rm -rf " ^ Filename.quote root))
    (fun () ->
      run_cancellation ~root ~managed ~gameplan ~fake_bin ~prompt_log
        ~shutdown_active:false);
  let root, _origin, managed, gameplan, fake_bin, prompt_log = fixture () in
  Fun.protect
    ~finally:(fun () ->
      Onton_test_support.Git_env.sh ~dir:"/" ("rm -rf " ^ Filename.quote root))
    (fun () ->
      run_cancellation ~root ~managed ~gameplan ~fake_bin ~prompt_log
        ~shutdown_active:true)
