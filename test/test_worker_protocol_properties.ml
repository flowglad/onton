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
          ("version", `Int 2);
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
      ("admission", `Assoc [ ("mode", `String "fresh") ]);
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
          "{\"version\":2,\"version\":2}\n";
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
      match P.decode "{\"version\":1,\"type\":\"bind\"}\n" with
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
               ("version", `Int 2);
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
          "{\"version\":2,\"type\":\"shutdown\",\"run_id\":\"r\",\"patch_id\":\"p\",\"extra\":0}\n"
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
            ("admission", `Assoc [ ("mode", `String "fresh") ]);
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

let generated_wire_state =
  QCheck2.Test.make
    ~name:"generated state versions and diagnostics retain protocol framing"
    ~count:200
    QCheck2.Gen.(pair (int_range 0 100000) string)
    (fun (version, diagnostic) ->
      let binding =
        wire "bind"
          [
            ("worker_id", `String "worker");
            ("workspace_id", `String "space");
            ("context", `String "context");
            ("capsule", `String "{}");
            ("state_version", `String (string_of_int version));
            ("attention_id", `Null);
            ("admission", `Assoc [ ("mode", `String "fresh") ]);
          ]
      in
      let t, _ = host (P.initial ()) binding in
      let state_ok =
        P.state_version t = Some (Int64.of_int version) && not (P.is_stopped t)
      in
      let stopped, _ = P.step t P.Host_lost in
      let stopped_ok = P.is_stopped stopped in
      let encoded =
        P.encode (P.Protocol_error { identity = None; reason = diagnostic })
      in
      state_ok && stopped_ok
      && String.length encoded <= 1024 * 1024
      && String.length encoded > 0
      && encoded.[String.length encoded - 1] = '\n'
      &&
      match Yojson.Safe.from_string encoded with
      | `Assoc fields ->
          List.assoc_opt "type" fields = Some (`String "protocol_error")
      | _ -> false)

let receipt t =
  match P.admission_receipt t with
  | Some json -> json
  | None -> failwith "unbound"

let resume ?(mutate = Fun.id) t =
  let admission = mutate (receipt t) in
  let fields = match admission with `Assoc fields -> fields | _ -> [] in
  let get key fallback =
    Option.value (List.assoc_opt key fields) ~default:fallback
  in
  wire "bind"
    [
      ("worker_id", `String "replacement");
      ("workspace_id", `String "space");
      ("context", `String "context");
      ("capsule", get "capsule" (`String "{}"));
      ("state_version", get "state_version" (`String "0"));
      ("attention_id", get "attention_id" `Null);
      ("admission", admission);
    ]

let replace t = fst (host (P.initial ()) (resume t))

let assignment n =
  execute ~seq:(string_of_int n)
    ~operation:("op" ^ string_of_int n)
    ~attempt:("attempt" ^ string_of_int n)
    ~version:(string_of_int (n - 1))
    ()

let finish_assignment n t =
  let t, _ =
    P.step t (P.Propose_checkpoint { proposal_id = "cp"; capsule = "capsule" })
  in
  let t, _ =
    host t
      (checkpoint_ack ~seq:(string_of_int n)
         ~operation:("op" ^ string_of_int n)
         ~attempt:("attempt" ^ string_of_int n)
         ~version:(string_of_int n) ())
  in
  let t, _ = P.step t P.Cleanup_complete in
  let t, output =
    P.step t
      (P.Finish
         { status = P.Completed_ok; diagnostics = "done"; renewal_id = None })
  in
  check (result output);
  t

let acknowledge n t =
  fst
    (host t
       (result_ack ~seq:(string_of_int n)
          ~operation:("op" ^ string_of_int n)
          ~attempt:("attempt" ^ string_of_int n)
          ~version:(string_of_int n) ()))

let completed_history n =
  let rec loop t i =
    if i > n then t
    else
      let t, output = host t (assignment i) in
      check (begins output);
      loop (acknowledge i (finish_assignment i t)) (i + 1)
  in
  loop (initial ()) 1

let replacement_continuity =
  QCheck2.Test.make
    ~name:"replacement preserves sequence, reservations and exact replay"
    ~count:100
    QCheck2.Gen.(int_range 1 30)
    (fun n ->
      try
        let original = completed_history n in
        let t = replace original in
        check (not (P.is_stopped t));
        check (receipt t = receipt original);
        let _, replay = host t (assignment n) in
        check ((not (begins replay)) && result replay);
        let _, old = host t (assignment 1) in
        check (not (begins old));
        let _, reset = host t (execute ~version:(string_of_int n) ()) in
        check (not (begins reset));
        let _, reused =
          host t
            (execute ~operation:"op1" ~attempt:"new-attempt"
               ~seq:(string_of_int (n + 1))
               ~version:(string_of_int n) ())
        in
        check (not (begins reused));
        let _, next = host t (assignment (n + 1)) in
        check (begins next);
        true
      with _ -> false)

let replacement_lost_ack =
  QCheck2.Test.make
    ~name:
      "lost result ACK blocks successor until exact ACK, including repeated \
       replacements"
    ~count:100
    QCheck2.Gen.(int_range 1 20)
    (fun n ->
      try
        let t, _ = host (completed_history (n - 1)) (assignment n) in
        let t = replace (finish_assignment n t) |> replace in
        let _, blocked = host t (assignment (n + 1)) in
        check (not (begins blocked));
        let _, replay = host t (assignment n) in
        check (result replay && not (begins replay));
        let t, _ = host t (result_ack ~version:"0" ()) in
        let _, blocked = host t (assignment (n + 1)) in
        check (not (begins blocked));
        let t = acknowledge n t in
        let t, duplicate =
          host t
            (result_ack ~seq:(string_of_int n)
               ~operation:("op" ^ string_of_int n)
               ~attempt:("attempt" ^ string_of_int n)
               ~version:(string_of_int n) ())
        in
        check (duplicate = []);
        let t = replace t in
        let t, next = host t (assignment (n + 1)) in
        check (begins next);
        let t, _ = host t (result_ack ()) in
        let _, still_busy = host t (assignment (n + 2)) in
        check (not (begins still_busy));
        true
      with _ -> false)

let replacement_unfinished =
  QCheck2.Test.make
    ~name:"interrupted attempt never begins again and has explicit recovery"
    ~count:100 QCheck2.Gen.bool (fun checkpoint_committed ->
      try
        let t = admitted () in
        let t =
          if checkpoint_committed then
            let t, _ =
              P.step t
                (P.Propose_checkpoint
                   { proposal_id = "cp"; capsule = "capsule" })
            in
            fst (host t (checkpoint_ack ()))
          else t
        in
        let t = replace t |> replace in
        check (P.recovery_pending t);
        let _, replay = host t (execute ()) in
        check (not (begins replay));
        let _, blocked = host t (assignment 2) in
        check (not (begins blocked));
        let _, illegal = complete t in
        check (not (result illegal));
        let t, outputs = P.step t P.Recover_unfinished in
        check (not (P.recovery_pending t));
        check
          (List.exists (function P.Recover _ -> true | _ -> false) outputs);
        check (not (begins outputs));
        let t, duplicate = P.step t P.Recover_unfinished in
        check
          (not
             (List.exists
                (function P.Recover _ -> true | _ -> false)
                duplicate));
        let version = if checkpoint_committed then "2" else "1" in
        let t, _ =
          P.step t
            (P.Propose_checkpoint
               { proposal_id = "recovery"; capsule = "capsule" })
        in
        let t, _ = host t (checkpoint_ack ~proposal:"recovery" ~version ()) in
        let t, _ = P.step t P.Cleanup_complete in
        let t, output =
          P.step t
            (P.Finish
               {
                 status = P.Retryable;
                 diagnostics = "observed effects";
                 renewal_id = None;
               })
        in
        check (result output);
        let t, _ = host t (result_ack ~version ()) in
        let _, next =
          host (replace t)
            (execute ~seq:"2" ~operation:"op2" ~attempt:"attempt2" ~version ())
        in
        check (begins next);
        true
      with _ -> false)

let edit_field key value = function
  | `Assoc fields -> `Assoc ((key, value) :: List.remove_assoc key fields)
  | json -> json

let edit_entries f = function
  | `Assoc fields as json -> (
      match List.assoc_opt "entries" fields with
      | Some (`List entries) -> edit_field "entries" (`List (f entries)) json
      | _ -> json)
  | json -> json

let invalid_receipts =
  QCheck2.Test.make
    ~name:"malformed and inconsistent receipts fail before ready or execution"
    ~count:1 QCheck2.Gen.unit (fun () ->
      try
        let t = completed_history 3 in
        let mutations =
          [
            edit_field "state_version" (`String "0");
            edit_field "initial_state_version" (`String "1");
            edit_field "initial_attention_id" (`String "unknown");
            edit_field "entries" (`List []);
            edit_entries List.rev;
            edit_entries (function x :: xs -> x :: x :: xs | [] -> []);
            edit_entries
              (List.map (edit_field "result_acknowledged" (`Bool false)));
            edit_entries (List.map (edit_field "terminal" `Null));
            edit_field "unknown" `Null;
            edit_entries
              (List.map (fun entry ->
                   match entry with
                   | `Assoc fields -> (
                       match List.assoc_opt "request" fields with
                       | Some request ->
                           edit_field "request"
                             (edit_field "run_id" (`String "other") request)
                             entry
                       | None -> entry)
                   | _ -> entry));
          ]
        in
        List.iter
          (fun mutate ->
            match P.decode (resume ~mutate t) with
            | Error _ -> ()
            | Ok binding ->
                let state, output = P.step (P.initial ()) (P.Host binding) in
                check (P.is_stopped state && not (begins output));
                check
                  (not
                     (List.exists
                        (function P.Send (P.Ready _) -> true | _ -> false)
                        output)))
          mutations;
        (* Receipts are authenticated host assertions, not signatures. A changed
         self-consistent history still cannot authorize the old request. *)
        let changed =
          edit_entries
            (List.map (fun entry ->
                 match entry with
                 | `Assoc fields -> (
                     match List.assoc_opt "request" fields with
                     | Some request ->
                         edit_field "request"
                           (edit_field "guidance" (`String "changed") request)
                           entry
                     | None -> entry)
                 | _ -> entry))
        in
        let t, _ = host (P.initial ()) (resume ~mutate:changed t) in
        let _, altered = host t (assignment 3) in
        check ((not (begins altered)) && not (result altered));
        true
      with _ -> false)

let replacement_renewal =
  QCheck2.Test.make
    ~name:"replacement preserves attention and consumed renewal receipts"
    ~count:1 QCheck2.Gen.unit (fun () ->
      try
        let t, _ =
          complete ~status:P.Attention ~renewal:"receipt" (admitted ())
        in
        let t = replace (acknowledge 1 t) in
        let _, blocked = host t (assignment 2) in
        check (not (begins blocked));
        let request =
          execute ~seq:"2" ~operation:"op2" ~attempt:"attempt2" ~version:"1"
            ~renewal:(`String "receipt") ~renewed:true ()
        in
        let t, output = host t request in
        check (begins output);
        let t = replace t in
        let _, replay = host t request in
        check (not (begins replay));
        let t, _ = P.step t P.Recover_unfinished in
        let t = finish_assignment 2 t |> acknowledge 2 |> replace in
        let _, reused =
          host t
            (execute ~seq:"3" ~operation:"op3" ~attempt:"attempt3" ~version:"2"
               ~renewal:(`String "receipt") ~renewed:true ())
        in
        check (not (begins reused));
        true
      with _ -> false)

let bound_receipt_pair =
  QCheck2.Test.make
    ~name:"resume binds exact committed capsule, version, attention and patch"
    ~count:1 QCheck2.Gen.unit (fun () ->
      try
        let t = completed_history 3 in
        let binding = Yojson.Safe.from_string (resume t) in
        List.iter
          (fun (key, value) ->
            let line =
              Yojson.Safe.to_string (edit_field key value binding) ^ "\n"
            in
            let t, outputs = host (P.initial ()) line in
            check (P.is_stopped t);
            check
              (not
                 (List.exists
                    (function
                      | P.Send (P.Ready _) | P.Begin _ -> true | _ -> false)
                    outputs)))
          [
            ("capsule", `String "different");
            ("state_version", `String "4");
            ("attention_id", `String "different");
            ("patch_id", `String "different");
          ];
        let admission = receipt t in
        let malformed =
          [
            edit_entries
              (List.map (edit_field "result_acknowledged" `Null))
              admission;
            edit_entries
              (List.map (edit_field "terminal" (`Assoc [])))
              admission;
            edit_field "initial_state_version" (`String "01") admission;
            edit_entries
              (List.map (fun entry ->
                   match entry with
                   | `Assoc fields -> (
                       match List.assoc_opt "request" fields with
                       | Some request ->
                           edit_field "request"
                             (edit_field "assignment_seq"
                                (`String "9223372036854775807") request)
                             entry
                       | None -> entry)
                   | _ -> entry))
              admission;
          ]
        in
        List.iter
          (fun invalid ->
            match P.decode (resume ~mutate:(fun _ -> invalid) t) with
            | Error _ -> ()
            | Ok binding ->
                let t, output = P.step (P.initial ()) (P.Host binding) in
                check (P.is_stopped t && not (begins output)))
          malformed;
        true
      with _ -> false)

let committed_before_delivery =
  QCheck2.Test.make
    ~name:
      "host committed checkpoint and result ACK receipts survive lost delivery"
    ~count:1 QCheck2.Gen.unit (fun () ->
      try
        let running = admitted () in
        let pending, _ =
          P.step running
            (P.Propose_checkpoint { proposal_id = "cp"; capsule = "capsule" })
        in
        (* Pure prospective transition: host commits this receipt, but the live
         worker is still at pending and has not received its ACK. *)
        let committed, _ = host pending (checkpoint_ack ()) in
        let _, blocked =
          P.step pending
            (P.Finish
               {
                 status = P.Completed_ok;
                 diagnostics = "done";
                 renewal_id = None;
               })
        in
        check (not (result blocked));
        let replacement = replace committed in
        check (P.state_version replacement = Some 1L);
        let _, replay = host replacement (execute ()) in
        check (not (begins replay));
        let terminal = finish_assignment 1 running in
        let acknowledged = acknowledge 1 terminal in
        let _, blocked = host terminal (assignment 2) in
        check (not (begins blocked));
        let _, allowed = host (replace acknowledged) (assignment 2) in
        check (begins allowed);
        let missing =
          match Yojson.Safe.from_string (bind ()) with
          | `Assoc fields ->
              Yojson.Safe.to_string
                (`Assoc (List.remove_assoc "admission" fields))
              ^ "\n"
          | _ -> ""
        in
        check (match P.decode missing with Error _ -> true | Ok _ -> false);
        true
      with _ -> false)

let () =
  QCheck_base_runner.run_tests_main
    [
      bound_receipt_pair;
      committed_before_delivery;
      replacement_continuity;
      replacement_lost_ack;
      replacement_unfinished;
      invalid_receipts;
      replacement_renewal;
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
      generated_wire_state;
    ]
