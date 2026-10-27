(* @archlint.module test
   @archlint.domain worker-protocol-v3 *)
open Onton_core

[@@@warning "-4-40-42"]

module V = Worker_protocol_v3
module P = Worker_protocol

let context ?(generation = "1") ?(mode = "feature") ?(root = "patch")
    ?(admitted = true) () =
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
         ("selected_base", `String "feature/root");
         ("execution_mode", `String mode);
         ("root_role", `String root);
         ("admitted", `Bool admitted);
         ("evidence_id", `String "ticket");
         ("guidance_ids", `List [ `String "g" ]);
         ("input_version", `String "0");
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

let () =
  test_lifecycle ();
  test_integration ();
  test_input ();
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
                  && loop t next rest
                else
                  let _, outputs =
                    V.step t (V.Input_committed ("wrong", next))
                  in
                  V.desired_base t
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
  QCheck_base_runner.run_tests_main [ total; frames; interleavings ]
