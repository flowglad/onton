(* @archlint.module test
   @archlint.domain worker-protocol *)
[@@@warning "-4"]

open Onton_core
module P = Worker_protocol

let check b = if not b then failwith "worker protocol property failed"

let wire kind extra =
  Yojson.Safe.to_string
    (`Assoc
       ([
          ("version", `Int 1);
          ("type", `String kind);
          ("run_id", `String "run");
          ("patch_id", `String "patch");
        ]
       @ extra))
  ^ "\n"

let bind ?(attention = `Null) () =
  wire "bind"
    [
      ("worker_id", `String "worker");
      ("workspace_id", `String "space");
      ("context", `String "context");
      ("capsule", `String "{}");
      ("state_version", `String "0");
      ("attention_id", attention);
    ]

let execute ?(operation = "op1") ?(attempt = "attempt1") ?(seq = "1")
    ?(version = "0") ?(renewal = `Null) ?(renewed = false) () =
  wire "execute"
    [
      ("operation_id", `String operation);
      ("attempt_id", `String attempt);
      ("assignment_seq", `String seq);
      ("expected_state_version", `String version);
      ("work", `Assoc [ ("type", `String "reconcile") ]);
      ("guidance", `String "go");
      ("recovery_mode", `String "retained_workspace");
      ("renewal_id", renewal);
      ("renewed_intent", `Bool renewed);
    ]

let checkpoint_ack ?(seq = "1") ?(operation = "op1") ?(attempt = "attempt1")
    ?(proposal = "cp") ?(version = "1") ?(accepted = true) () =
  wire "checkpoint_ack"
    [
      ("operation_id", `String operation);
      ("attempt_id", `String attempt);
      ("assignment_seq", `String seq);
      ("proposal_id", `String proposal);
      ("state_version", `String version);
      ("capsule", `String "capsule");
      ("accepted", `Bool accepted);
    ]

let result_ack ?(seq = "1") ?(operation = "op1") ?(attempt = "attempt1")
    ?(version = "1") () =
  wire "result_ack"
    [
      ("operation_id", `String operation);
      ("attempt_id", `String attempt);
      ("assignment_seq", `String seq);
      ("state_version", `String version);
    ]

let cancel ?(seq = "1") ?(operation = "op1") ?(attempt = "attempt1") () =
  wire "cancel"
    [
      ("operation_id", `String operation);
      ("attempt_id", `String attempt);
      ("assignment_seq", `String seq);
    ]

let decoded line =
  match P.decode line with Ok x -> x | Error _ -> failwith "fixture decode"

let host t line = P.step t (P.Host (decoded line))

let begins outputs =
  List.exists (function P.Begin _ -> true | _ -> false) outputs

let stops outputs = List.exists (function P.Stop -> true | _ -> false) outputs

let cancel_effect outputs =
  List.exists (function P.Cancel_active -> true | _ -> false) outputs

let result outputs =
  List.exists (function P.Send (P.Result _) -> true | _ -> false) outputs

let initial () = fst (host (P.initial ()) (bind ()))
let admitted () = fst (host (initial ()) (execute ()))

let complete ?(status = P.Completed_ok) ?renewal t =
  let t, _ =
    P.step t (P.Propose_checkpoint { proposal_id = "cp"; capsule = "capsule" })
  in
  let t, _ = host t (checkpoint_ack ()) in
  let t, _ = P.step t P.Cleanup_complete in
  P.step t (P.Finish { status; diagnostics = "done"; renewal_id = renewal })

let totality =
  QCheck2.Test.make ~name:"wire decoder is total over arbitrary bytes"
    ~count:3000 QCheck2.Gen.string (fun s ->
      ignore (P.decode s);
      true)

let malformed =
  QCheck2.Test.make ~name:"invalid frames and counters never acquire authority"
    ~count:1 QCheck2.Gen.unit (fun () ->
      List.for_all
        (fun s -> match P.decode s with Error _ -> true | Ok _ -> false)
        [
          "{}";
          "{}\n";
          "{\"version\":1,\"version\":1}\n";
          String.make (1024 * 1024) 'x' ^ "\n";
          "\255\n";
          execute ~seq:"01" ();
          execute ~seq:"-1" ();
          execute ~seq:"9223372036854775808" ();
          wire "execute"
            [
              ("operation_id", `String "op");
              ("attempt_id", `String "at");
              ("assignment_seq", `String "1");
              ("expected_state_version", `String "0");
              ("work", `Assoc [ ("type", `String "root_integration") ]);
              ("guidance", `String "go");
              ("recovery_mode", `String "lost_workspace");
              ("renewal_id", `Null);
              ("renewed_intent", `Bool false);
            ];
        ])

let incompatible =
  QCheck2.Test.make ~name:"unsupported version is distinct" ~count:1
    QCheck2.Gen.unit (fun () ->
      match P.decode "{\"version\":2,\"type\":\"bind\"}\n" with
      | Error (P.Incompatible _) -> true
      | _ -> false)

let sequential =
  QCheck2.Test.make ~name:"exact replay and sequential result ACK gate" ~count:1
    QCheck2.Gen.unit (fun () ->
      let t = initial () in
      let t, first = host t (execute ()) in
      check (begins first);
      let t, replay = host t (execute ()) in
      check (not (begins replay));
      let _, overlap =
        host t (execute ~operation:"op2" ~attempt:"attempt2" ~seq:"2" ())
      in
      check (not (begins overlap));
      let t, finished = complete t in
      check (result finished);
      let _, blocked =
        host t
          (execute ~operation:"op2" ~attempt:"attempt2" ~seq:"2" ~version:"1" ())
      in
      check (not (begins blocked));
      let t, _ = host t (result_ack ()) in
      let t, next =
        host t
          (execute ~operation:"op2" ~attempt:"attempt2" ~seq:"2" ~version:"1" ())
      in
      check (begins next);
      let _, stale = host t (cancel ()) in
      check (not (cancel_effect stale));
      let _, conflict = host t (execute ~operation:"changed" ~seq:"1" ()) in
      check (not (begins conflict));
      true)

let acks =
  QCheck2.Test.make ~name:"only exact accepted checkpoint ACK unblocks terminal"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let t = admitted () in
      let t, _ =
        P.step t
          (P.Propose_checkpoint { proposal_id = "cp"; capsule = "capsule" })
      in
      let t, _ = host t (checkpoint_ack ~proposal:"stale" ()) in
      let t, _ = P.step t P.Cleanup_complete in
      let _, premature =
        P.step t
          (P.Finish
             {
               status = P.Completed_ok;
               diagnostics = "done";
               renewal_id = None;
             })
      in
      check (not (result premature));
      let t, _ = host t (checkpoint_ack ()) in
      let _, finished =
        P.step t
          (P.Finish
             {
               status = P.Completed_ok;
               diagnostics = "done";
               renewal_id = None;
             })
      in
      check (result finished);
      let t = admitted () in
      let t, _ =
        P.step t
          (P.Propose_checkpoint { proposal_id = "cp"; capsule = "capsule" })
      in
      let t, e = host t (checkpoint_ack ~accepted:false ()) in
      check (P.is_stopped t && stops e);
      true)

let renewal =
  QCheck2.Test.make ~name:"attention requires explicit, matching, new operation"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let t =
        fst (host (P.initial ()) (bind ~attention:(`String "receipt") ()))
      in
      let _, plain = host t (execute ()) in
      check (not (begins plain));
      let _, wrong =
        host t
          (execute ~operation:"op2" ~attempt:"attempt2"
             ~renewal:(`String "wrong") ~renewed:true ())
      in
      check (not (begins wrong));
      let _, implicit = host t (execute ~renewal:(`String "receipt") ()) in
      check (not (begins implicit));
      let t, accepted =
        host t (execute ~renewal:(`String "receipt") ~renewed:true ())
      in
      check (begins accepted);
      let _, dup =
        host t (execute ~renewal:(`String "receipt") ~renewed:true ())
      in
      check (not (begins dup));
      true)

let cancellation =
  QCheck2.Test.make ~name:"cancel and host loss are attempt scoped" ~count:1
    QCheck2.Gen.unit (fun () ->
      let t = admitted () in
      let t, e = host t (cancel ()) in
      check (cancel_effect e);
      let t, e = host t (cancel ()) in
      check (not (cancel_effect e));
      let t, e = P.step t P.Host_lost in
      check (P.is_stopped t && stops e && cancel_effect e);
      true)

let interleavings =
  QCheck2.Test.make
    ~name:"arbitrary ACK and cancel interleavings never begin twice" ~count:500
    QCheck2.Gen.(list_size (int_range 0 50) (int_range 0 5))
    (fun choices ->
      let t = admitted () in
      let _, begins_count =
        List.fold_left
          (fun (t, n) choice ->
            let t, e =
              match choice with
              | 0 -> host t (execute ())
              | 1 -> host t (checkpoint_ack ~proposal:"stale" ())
              | 2 -> host t (result_ack ())
              | 3 -> host t (cancel ~attempt:"stale" ())
              | 4 -> P.step t P.Cleanup_complete
              | _ ->
                  host t
                    (execute ~operation:"other" ~attempt:"other" ~seq:"2" ())
            in
            (t, n + if begins e then 1 else 0))
          (t, 0) choices
      in
      begins_count = 0)

let boundaries =
  QCheck2.Test.make ~name:"identifier byte bounds and exact frame boundary"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let ok = String.make 256 'x' and too_long = String.make 257 'x' in
      let frame name =
        Yojson.Safe.to_string
          (`Assoc
             [
               ("version", `Int 1);
               ("type", `String "shutdown");
               ("run_id", `String name);
               ("patch_id", `String "patch");
             ])
        ^ "\n"
      in
      (match P.decode (frame ok) with Ok _ -> true | Error _ -> false)
      && (match P.decode (frame too_long) with
        | Error _ -> true
        | Ok _ -> false)
      && (match P.decode (frame "") with Error _ -> true | Ok _ -> false)
      &&
      match
        P.decode
          "{\"version\":1,\"type\":\"shutdown\",\"run_id\":\"r\",\"patch_id\":\"p\",\"extra\":0}\n"
      with
      | Error _ -> true
      | Ok _ -> false)

let exact_ack_replay =
  QCheck2.Test.make ~name:"duplicate exact ACK is inert; changed bind stops"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let t = admitted () in
      let t, _ =
        P.step t
          (P.Propose_checkpoint { proposal_id = "cp"; capsule = "capsule" })
      in
      let t, _ = host t (checkpoint_ack ()) in
      let t, e = host t (checkpoint_ack ()) in
      check (e = [] && P.state_version t = Some 1L);
      let t, e = host t (bind ()) in
      check ((not (P.is_stopped t)) && e <> []);
      let changed =
        wire "bind"
          [
            ("worker_id", `String "worker");
            ("workspace_id", `String "different");
            ("context", `String "context");
            ("capsule", `String "{}");
            ("state_version", `String "0");
            ("attention_id", `Null);
          ]
      in
      let t, e = host t changed in
      check (P.is_stopped t && stops e);
      true)

let attention_transition =
  QCheck2.Test.make
    ~name:"terminal attention gates next operation until explicit renewal"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let t, emitted =
        complete ~status:P.Attention ~renewal:"receipt" (admitted ())
      in
      check (result emitted);
      let t, _ = host t (result_ack ()) in
      let next =
        execute ~operation:"op2" ~attempt:"attempt2" ~seq:"2" ~version:"1" ()
      in
      let _, plain = host t next in
      check (not (begins plain));
      let renewed =
        execute ~operation:"op2" ~attempt:"attempt2" ~seq:"2" ~version:"1"
          ~renewal:(`String "receipt") ~renewed:true ()
      in
      let t, yes = host t renewed in
      check (begins yes);
      let _, again = host t renewed in
      check (not (begins again));
      true)

let bounded_encode =
  QCheck2.Test.make ~name:"worker output stays within frame limit" ~count:1
    QCheck2.Gen.unit (fun () ->
      let identity = P.{ run_id = "r"; patch_id = "p" } in
      let operation =
        P.{ operation_id = "o"; attempt_id = "a"; assignment_seq = 1L }
      in
      let line =
        P.encode
          (P.Observation
             { identity; operation; text = String.make (1024 * 1024) 'x' })
      in
      String.length line <= 1024 * 1024
      &&
      match Yojson.Safe.from_string line with
      | `Assoc xs -> List.assoc_opt "type" xs = Some (`String "protocol_error")
      | _ -> false)

let () =
  QCheck_base_runner.run_tests_main
    [
      totality;
      malformed;
      incompatible;
      sequential;
      acks;
      renewal;
      cancellation;
      interleavings;
      boundaries;
      exact_ack_replay;
      attention_transition;
      bounded_encode;
    ]
