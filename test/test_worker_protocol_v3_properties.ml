(* @archlint.module test
   @archlint.domain worker-protocol-v3 *)
open Onton_core

[@@@warning "-4-40-42"]

module V = Worker_protocol_v3
module P = Worker_protocol

let context ?(generation = "1") ?(mode = "feature") ?(root = "patch")
    ?(admitted = true) ?(base = "feature/root") ?(version = "0") () =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("version", `Int 3);
         ("run_id", `String "r");
         ("patch_id", `String "p");
         ("config_id", `String "config");
         ("plan_id", `String "plan");
         ("message_id", `String "m");
         ("generation", `String generation);
         ( "dependencies",
           `List
             [
               `Assoc
                 [
                   ("patch_id", `String "parent");
                   ("admitted", `Bool true);
                   ("revision", `String "abc");
                 ];
             ] );
         ("selected_base", `String base);
         ("execution_mode", `String mode);
         ("root_role", `String root);
         ("admitted", `Bool admitted);
         ("evidence_id", `String "ticket");
         ("guidance_ids", `List [ `String "g" ]);
         ("input_version", `String version);
       ])

let ok = function Ok x -> x | Error _ -> failwith "unexpected decode error"
let ctx () = ok (V.decode_context (context ()))

let bind () =
  ok
    (P.decode
       "{\"version\":2,\"type\":\"bind\",\"run_id\":\"r\",\"patch_id\":\"p\",\"worker_id\":\"w\",\"workspace_id\":\"ws\",\"context\":\"legacy\",\"capsule\":\"{}\",\"state_version\":\"0\",\"attention_id\":null,\"admission\":{\"mode\":\"fresh\"}}\n")

let request () =
  match
    P.decode
      "{\"version\":2,\"type\":\"execute\",\"run_id\":\"r\",\"patch_id\":\"p\",\"operation_id\":\"op\",\"attempt_id\":\"attempt\",\"assignment_seq\":\"1\",\"expected_state_version\":\"0\",\"work\":{\"type\":\"reconcile\"},\"guidance\":\"original\",\"recovery_mode\":\"retained_workspace\",\"renewal_id\":null,\"renewed_intent\":false}\n"
  with
  | Ok (P.Execute r) -> r
  | _ -> failwith "invalid fixture"

let execute () =
  V.
    {
      request = request ();
      work = Reconcile;
      context = ctx ();
      input_version = 0L;
    }

let has_begin outputs =
  List.exists (function V.Begin _ -> true | _ -> false) outputs

let has_recover outputs =
  List.exists (function V.Recover _ -> true | _ -> false) outputs

let check cond label = if not cond then failwith label

let test_lifecycle () =
  let b = V.Bind (bind (), ctx ()) in
  let t, ready = V.step (V.initial ()) (V.Host b) in
  check
    (List.exists (function V.Send (P.Ready _) -> true | _ -> false) ready)
    "bind";
  let e = execute () in
  let line = ok (V.encode_host (V.Execute e)) in
  check
    (match V.decode_host line with Ok (V.Execute _) -> true | _ -> false)
    "v3 execute roundtrip";
  let t, outputs = V.step t (V.Host (V.Execute e)) in
  check (has_begin outputs) "first admission";
  let receipt =
    match V.admission_receipt t with
    | Some j -> j
    | None -> failwith "receipt absent"
  in
  let t, outputs = V.step t (V.Host (V.Execute e)) in
  check (not (has_begin outputs)) "exact replay must not begin";
  let changed = { e with request = { e.request with guidance = "changed" } } in
  let _, outputs = V.step t (V.Host (V.Execute changed)) in
  check (not (has_begin outputs)) "changed replay must not begin";
  let receipt_bytes = Yojson.Safe.to_string receipt in
  let original = P.encode_host (bind ()) in
  let json = Yojson.Safe.from_string original in
  let fields =
    match json with `Assoc xs -> xs | _ -> failwith "bind object"
  in
  let fields = ("admission", receipt) :: List.remove_assoc "admission" fields in
  let replacement =
    ok (P.decode (Yojson.Safe.to_string (`Assoc fields) ^ "\n"))
  in
  let restored, ready =
    V.step (V.initial ()) (V.Host (V.Bind (replacement, ctx ())))
  in
  check
    (List.exists (function V.Send (P.Ready _) -> true | _ -> false) ready)
    "restore receipt";
  let restored, outputs = V.step restored (V.Host (V.Execute e)) in
  check (not (has_begin outputs)) "restored replay";
  let _, outputs = V.step restored V.Recover_unfinished in
  check (has_recover outputs) "unfinished recovery";
  ignore receipt_bytes

let test_integration () =
  let t, _ = V.step (V.initial ()) (V.Host (V.Bind (bind (), ctx ()))) in
  let e =
    {
      V.request = request ();
      work = V.Integrate_revision { contributor = "child"; revision = "abc123" };
      context = ctx ();
      input_version = 0L;
    }
  in
  let line = ok (V.encode_host (V.Execute e)) in
  let decoded = ok (V.decode_host line) in
  let t, out = V.step t (V.Host decoded) in
  check (has_begin out) "integration admitted";
  let changed =
    {
      e with
      work = V.Integrate_revision { contributor = "child"; revision = "def456" };
    }
  in
  let _, out = V.step t (V.Host (V.Execute changed)) in
  check (not (has_begin out)) "captured revision replay fence";
  let _, out =
    V.step t
      (V.Host
         (V.Execute
            {
              e with
              context = ok (V.decode_context (context ~generation:"2" ()));
            }))
  in
  check (not (has_begin out)) "context replay fence"

let test_input () =
  let identity = (request ()).identity in
  let t, _ = V.step (V.initial ()) (V.Host (V.Bind (bind (), ctx ()))) in
  let i =
    V.
      {
        input_id = "g1";
        version = 1L;
        guidance = "new";
        selected_base = "feature/next";
      }
  in
  let t, out = V.step t (V.Host (V.Input (identity, i))) in
  check
    (out = [] && V.desired_base t = Some "feature/root")
    "input is not a commit";
  let t, out = V.step t (V.Input_committed ("g1", 1L)) in
  check
    (V.desired_base t = Some "feature/next"
    && List.exists (function V.Apply_input _ -> true | _ -> false) out)
    "committed input";
  let _, out = V.step t (V.Host (V.Input (identity, i))) in
  check (out = []) "input exact replay";
  let _, out =
    V.step t (V.Host (V.Input (identity, { i with guidance = "altered" })))
  in
  check
    (List.exists (function V.Reject _ -> true | _ -> false) out)
    "changed input ID";
  let _, out = V.step t (V.Input_committed ("other", 2L)) in
  check
    (List.exists (function V.Reject _ -> true | _ -> false) out)
    "unmatched input commit"

let has_reject outputs =
  List.exists (function V.Reject _ -> true | _ -> false) outputs

let restore t context =
  let receipt = Option.get (V.admission_receipt t) in
  let fields =
    match Yojson.Safe.from_string (P.encode_host (bind ())) with
    | `Assoc fields -> fields
    | _ -> failwith "bind object"
  in
  let binding =
    ok
      (P.decode
         (Yojson.Safe.to_string
            (`Assoc
               (("admission", receipt) :: List.remove_assoc "admission" fields))
         ^ "\n"))
  in
  V.step (V.initial ()) (V.Host (V.Bind (binding, context)))

let test_retarget () =
  let identity = (request ()).identity in
  let initial, _ = V.step (V.initial ()) (V.Host (V.Bind (bind (), ctx ()))) in
  let input =
    V.
      {
        input_id = "retarget";
        version = 1L;
        guidance = "move";
        selected_base = "feature/next";
      }
  in
  let current =
    ok
      (V.decode_context
         (context ~generation:"2" ~base:input.selected_base ~version:"1" ()))
  in
  let e = { (execute ()) with context = current; input_version = 1L } in
  let pending, _ = V.step initial (V.Host (V.Input (identity, input))) in
  let _, outputs = V.step pending (V.Host (V.Execute e)) in
  check (has_reject outputs && not (has_begin outputs)) "uncommitted retarget";
  let committed, _ = V.step pending (V.Input_committed (input.input_id, 1L)) in
  let reject_context context version label =
    let _, outputs =
      V.step committed
        (V.Host (V.Execute { e with context; input_version = version }))
    in
    check (has_reject outputs && not (has_begin outputs)) label
  in
  reject_context (ctx ()) 1L "old base despite current envelope version";
  reject_context current 0L "stale envelope version";
  reject_context
    (ok (V.decode_context (context ~base:input.selected_base ())))
    1L "stale context version";
  reject_context
    (ok (V.decode_context (context ~base:"uncommitted" ~version:"1" ())))
    1L "uncommitted base";
  reject_context
    (ok (V.decode_context (context ~base:input.selected_base ~version:"2" ())))
    1L "future context version";
  List.iter
    (fun (key, value) ->
      let fields =
        match Yojson.Safe.from_string (V.context_bytes current) with
        | `Assoc fields -> fields
        | _ -> failwith "context object"
      in
      let changed =
        ok
          (V.decode_context
             (Yojson.Safe.to_string
                (`Assoc ((key, value) :: List.remove_assoc key fields))))
      in
      reject_context changed 1L ("changed scope: " ^ key))
    [
      ("run_id", `String "other");
      ("patch_id", `String "other");
      ("config_id", `String "other");
      ("plan_id", `String "other");
      ("execution_mode", `String "mainline");
      ("root_role", `String "root");
      ("admitted", `Bool false);
      ("generation", `String "0");
    ];
  let admitted, outputs = V.step committed (V.Host (V.Execute e)) in
  check (has_begin outputs) "current retarget execution admitted";
  let _, outputs = V.step admitted (V.Host (V.Execute e)) in
  check (not (has_begin outputs)) "retarget exact replay";
  let later_input =
    {
      input with
      input_id = "next";
      version = 2L;
      selected_base = "feature/later";
    }
  in
  let later, _ = V.step admitted (V.Host (V.Input (identity, later_input))) in
  let later, _ = V.step later (V.Input_committed ("next", 2L)) in
  let _, outputs = V.step later (V.Host (V.Execute e)) in
  check
    ((not (has_begin outputs)) && not (has_reject outputs))
    "historical exact replay after retarget";
  let later_context =
    ok
      (V.decode_context
         (context ~generation:"3" ~base:later_input.selected_base ~version:"2"
            ()))
  in
  let restored, outputs = restore later later_context in
  check (not (has_reject outputs)) "restore advanced binding";
  check
    (V.desired_base restored = Some later_input.selected_base)
    "restore latest desired base";
  let _, outputs = V.step restored V.Recover_unfinished in
  check
    (List.exists
       (function V.Recover historical -> historical = e | _ -> false)
       outputs)
    "recovery retains historical context";
  let _, outputs = V.step restored (V.Host (V.Execute e)) in
  check
    ((not (has_begin outputs)) && not (has_reject outputs))
    "restored historical exact replay";
  let _, outputs =
    V.step restored
      (V.Host (V.Execute { e with context = later_context; input_version = 2L }))
  in
  check (not (has_begin outputs)) "changed historical execution fenced";
  let completed, _ =
    V.step later
      (V.Worker_event
         (P.Propose_checkpoint { proposal_id = "cp"; capsule = "{}" }))
  in
  let completed, _ =
    V.step completed
      (V.Host
         (V.Legacy_control
            (ok
               (P.decode
                  "{\"version\":2,\"type\":\"checkpoint_ack\",\"run_id\":\"r\",\"patch_id\":\"p\",\"operation_id\":\"op\",\"attempt_id\":\"attempt\",\"assignment_seq\":\"1\",\"proposal_id\":\"cp\",\"state_version\":\"1\",\"capsule\":\"{}\",\"accepted\":true}\n"))))
  in
  let completed, _ = V.step completed (V.Worker_event P.Cleanup_complete) in
  let completed, outputs =
    V.step completed
      (V.Worker_event
         (P.Finish
            { status = P.Completed_ok; diagnostics = "done"; renewal_id = None }))
  in
  check
    (List.exists (function V.Send (P.Result _) -> true | _ -> false) outputs)
    "retargeted assignment completed";
  let completed, _ =
    V.step completed
      (V.Host
         (V.Legacy_control
            (ok
               (P.decode
                  "{\"version\":2,\"type\":\"result_ack\",\"run_id\":\"r\",\"patch_id\":\"p\",\"operation_id\":\"op\",\"attempt_id\":\"attempt\",\"assignment_seq\":\"1\",\"state_version\":\"1\"}\n"))))
  in
  let next =
    {
      e with
      context = later_context;
      input_version = 2L;
      request =
        {
          e.request with
          expected_state_version = 1L;
          operation =
            { operation_id = "next"; attempt_id = "next"; assignment_seq = 2L };
        };
    }
  in
  let regressed =
    {
      next with
      context =
        ok
          (V.decode_context
             (context ~generation:"1" ~base:later_input.selected_base
                ~version:"2" ()));
    }
  in
  let _, outputs = V.step completed (V.Host (V.Execute regressed)) in
  check (has_reject outputs && not (has_begin outputs)) "generation regression";
  let _, outputs = V.step completed (V.Host (V.Execute next)) in
  check (has_begin outputs) "next assignment uses latest context"

let test_boundaries () =
  check
    (Result.is_error (V.decode_context (context ~generation:"01" ())))
    "canonical counter";
  check (Result.is_error (V.decode_context (context ~mode:"unknown" ()))) "mode";
  check (Result.is_error (V.decode_context (context ~root:"wrong" ()))) "role";
  check
    (Result.is_error (V.decode_host "{\"version\":3,\"version\":3}\n"))
    "duplicate keys";
  check
    (Result.is_error (V.decode_host (String.make 1048576 'x' ^ "\n")))
    "bound";
  check
    (Result.is_error (V.decode_host (P.encode_host (bind ()))))
    "v2 must not be v3";
  let result =
    P.Result
      {
        identity = (request ()).identity;
        operation = (request ()).operation;
        state_version = 1L;
        status = P.Attention;
        diagnostics = "attention";
        renewal_id = Some (String.make 257 'r');
      }
  in
  check
    (match V.encode_worker result with
    | Ok line -> Result.is_error (V.decode_worker line)
    | Error _ -> true)
    "oversized renewal identifier";
  check
    (match P.decode (P.encode_host (bind ())) with
    | Ok (P.Bind _) -> true
    | _ -> false)
    "v2 retained";
  let op = (request ()).operation in
  let n = V.note ~assignment:op ~publication:"pr-1" (Ok "bytes") in
  check
    (V.accept_note ~assignment:op ~publication:"pr-1" n = Some "bytes")
    "exact note";
  check
    (V.accept_note ~assignment:op ~publication:"pr-2" n = None)
    "note publication fence";
  check
    (V.note ~assignment:op ~publication:"pr-1" (Ok (String.make 262145 'x'))
    = V.Oversized)
    "oversize note"

let public_api_coverage =
  QCheck2.Test.make ~count:100 ~name:"v3 public execution and note API"
    (QCheck2.Gen.int_bound 1_000_000) (fun seed ->
      try
        let generation = Int64.of_int (seed + 1) in
        let encoded_context =
          context ~generation:(Int64.to_string generation) ()
        in
        let decoded_context = ok (V.decode_context encoded_context) in
        let assignment = (request ()).operation in
        let publication = "pr-" ^ string_of_int seed in
        let note_bytes = "note-" ^ string_of_int seed in
        let note = V.note ~assignment ~publication (Ok note_bytes) in
        let accepted = V.accept_note ~assignment ~publication note in
        let execution = execute () in
        let request =
          { execution.request with guidance = "guidance-" ^ string_of_int seed }
        in
        let execution = { execution with request; context = decoded_context } in
        let encoded = V.encode_host (V.Execute execution) in
        let bound = V.Bind (bind (), decoded_context) in
        let state, _ = V.step (V.initial ()) (V.Host bound) in
        V.context_bytes decoded_context = encoded_context
        && V.execution_mode decoded_context = "feature"
        && V.assigned_patch decoded_context = "p"
        && V.root_role decoded_context = "patch"
        && V.message_generation decoded_context = generation
        && Option.is_some (V.admission_receipt state)
        && V.state_version state = Some 0L
        && accepted = Some note_bytes && Result.is_ok encoded
      with _ -> false)

let () =
  test_lifecycle ();
  test_integration ();
  test_input ();
  test_retarget ();
  test_boundaries ();
  let open QCheck2 in
  let total =
    Test.make ~count:200 ~name:"v3 decoders are total over arbitrary bytes"
      Gen.string (fun s ->
        try
          ignore (V.decode_host s);
          ignore (V.decode_worker s);
          ignore (V.decode_context s);
          true
        with _ -> false)
  in
  let frames =
    Test.make ~count:100 ~name:"v3 worker frames roundtrip exactly"
      (Gen.oneof_list
         [
           P.Completed_ok;
           P.Completed_no_commits;
           P.Failed;
           P.Cancelled;
           P.Retryable;
           P.Attention;
           P.Merged_observed;
         ])
      (fun status ->
        let r = request () in
        let renewal_id =
          if status = P.Attention then Some "attention-1" else None
        in
        let msg =
          P.Result
            {
              identity = r.identity;
              operation = r.operation;
              state_version = 1L;
              status;
              diagnostics = "done";
              renewal_id;
            }
        in
        match V.encode_worker msg with
        | Error _ -> false
        | Ok line -> V.decode_worker line = Ok msg)
  in
  let interleavings =
    Test.make ~count:120
      ~name:"v3 input interleavings never apply before commit"
      (Gen.list_size (Gen.int_bound 30) Gen.bool)
      (fun commit_flags ->
        try
          let identity = (request ()).identity in
          let initial, _ =
            V.step (V.initial ()) (V.Host (V.Bind (bind (), ctx ())))
          in
          let rec loop t version = function
            | [] -> true
            | commit :: rest ->
                let next = Int64.succ version in
                let input =
                  V.
                    {
                      input_id = "g-" ^ Int64.to_string next;
                      version = next;
                      guidance = "guidance";
                      selected_base = "base-" ^ Int64.to_string next;
                    }
                in
                let t, outputs =
                  V.step t (V.Host (V.Input (identity, input)))
                in
                if outputs <> [] then false
                else if commit then
                  let t, outputs =
                    V.step t (V.Input_committed (input.input_id, next))
                  in
                  V.desired_base t = Some input.selected_base
                  && List.exists
                       (function V.Apply_input _ -> true | _ -> false)
                       outputs
                  &&
                  let execution =
                    {
                      (execute ()) with
                      context =
                        ok
                          (V.decode_context
                             (context
                                ~generation:(Int64.to_string (Int64.succ next))
                                ~base:input.selected_base
                                ~version:(Int64.to_string next) ()));
                      input_version = next;
                    }
                  in
                  let _, outputs = V.step t (V.Host (V.Execute execution)) in
                  has_begin outputs
                  && (let _, stale =
                        V.step t
                          (V.Host
                             (V.Execute { execution with context = ctx () }))
                      in
                      has_reject stale && not (has_begin stale))
                  && loop t next rest
                else
                  let _, outputs =
                    V.step t (V.Input_committed ("wrong", next))
                  in
                  let candidate =
                    {
                      (execute ()) with
                      context =
                        ok
                          (V.decode_context
                             (context ~base:input.selected_base
                                ~version:(Int64.to_string next) ()));
                      input_version = next;
                    }
                  in
                  let _, premature = V.step t (V.Host (V.Execute candidate)) in
                  has_reject premature
                  && (not (has_begin premature))
                  && V.desired_base t
                     = Some
                         (if version = 0L then "feature/root"
                          else "base-" ^ Int64.to_string version)
                  && List.exists
                       (function V.Reject _ -> true | _ -> false)
                       outputs
                  &&
                  (* pending input remains exact, so a later different ID must not displace it *)
                  let _, changed =
                    V.step t
                      (V.Host
                         (V.Input (identity, { input with input_id = "other" })))
                  in
                  List.exists
                    (function V.Reject _ -> true | _ -> false)
                    changed
          in
          loop initial 0L commit_flags
        with _ -> false)
  in
  QCheck_base_runner.run_tests_main
    [ total; frames; interleavings; public_api_coverage ]
