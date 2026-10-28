(* @archlint.module test
   @archlint.domain worker-stream *)
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

module H = Worker_stream

let wire message = ok (V.encode_worker message)

let bound () =
  fst (ok (H.command H.initial (ok (V.encode_host (V.Bind (bind (), ctx ()))))))

let active () =
  fst (ok (H.command (bound ()) (ok (V.encode_host (V.Execute (execute ()))))))

let identity : P.identity = { run_id = "r"; patch_id = "p" }
let operation = (request ()).P.operation

let checkpoint version capsule =
  P.Checkpoint
    {
      identity;
      operation;
      proposal_id = "checkpoint-" ^ Int64.to_string version;
      previous_state_version = Int64.pred version;
      state_version = version;
      capsule;
    }

let property name gen f =
  QCheck2.Test.make ~name ~count:200 gen (fun x -> try f x with _ -> false)

let () =
  QCheck_base_runner.run_tests_main
    [
      property
        "actual Ready is retained across codec restart and duplicate delivery"
        QCheck2.Gen.unit (fun () ->
          let t = bound () in
          let frame =
            wire (P.Ready { identity; worker_id = "w"; state_version = 0L })
          in
          let observed, _ = ok (H.receive t frame) in
          let restored = ok (H.decode (H.encode observed)) in
          let twice, _ = ok (H.receive restored frame) in
          (not (H.ready t))
          && H.ready restored
          && H.encode twice = H.encode restored);
      property
        "late cancellation cannot rewrite an already captured terminal result"
        QCheck2.Gen.bool (fun cancel_first ->
          let cancel t =
            fst
              (ok
                 (H.command t
                    "{\"version\":3,\"type\":\"cancel\",\"run_id\":\"r\",\"patch_id\":\"p\",\"operation_id\":\"op\",\"attempt_id\":\"attempt\",\"assignment_seq\":\"1\"}\n"))
          in
          let t = active () in
          let t = if cancel_first then cancel t else t in
          let t, _ =
            ok (H.receive t (wire (checkpoint 1L "captured terminal")))
          in
          let t = if cancel_first then t else cancel t in
          let t = ok (H.decode (H.encode t)) in
          let result =
            P.Result
              {
                identity;
                operation;
                state_version = 1L;
                status = Completed_ok;
                diagnostics = "";
                renewal_id = None;
              }
          in
          let t, _ = ok (H.receive t (wire result)) in
          H.terminal t = Some result);
      property "stream decoders and inbound/command boundaries are total"
        QCheck2.Gen.string (fun bytes ->
          ignore (H.decode bytes);
          ignore (H.command H.initial bytes);
          ignore (H.receive (active ()) bytes);
          true);
      property
        "checkpoint input history retains older exact packets across restart"
        QCheck2.Gen.(int_range 1 10)
        (fun count ->
          let packet n : V.input =
            {
              input_id = "input-" ^ string_of_int n;
              version = Int64.of_int n;
              guidance = "";
              selected_base = "feature/root";
              evidence = [];
              controls = [];
              covered = [];
            }
          in
          let rec deliver state n =
            if n > count then state
            else
              let frame = ok (V.encode_host (V.Input (identity, packet n))) in
              deliver (fst (ok (H.command state frame))) (n + 1)
          in
          let state = deliver (bound ()) 1 in
          let restored = ok (H.decode (H.encode state)) in
          List.for_all
            (fun n ->
              H.delivered_input restored (packet n)
              && not
                   (H.delivered_input restored
                      { (packet n) with guidance = "changed" }))
            (List.init count (fun n -> n + 1)));
      property
        "cumulative input gaps require coverage of every intervening version"
        QCheck2.Gen.(int_range 2 10)
        (fun count ->
          let covered =
            List.init count (fun n ->
                ("input-" ^ string_of_int (n + 1), Int64.of_int (n + 1)))
          in
          let packet : V.input =
            {
              input_id = "input-" ^ string_of_int count;
              version = Int64.of_int count;
              guidance = "all guidance";
              selected_base = "feature/root";
              evidence = [];
              controls = [];
              covered;
            }
          in
          let command input =
            H.command (bound ())
              (ok (V.encode_host (V.Input (identity, input))))
          in
          Result.is_ok (command packet)
          && Result.is_error (command { packet with covered = List.tl covered }));
      property "inbound checkpoint cannot skip the closed next-version boundary"
        QCheck2.Gen.(int_range 2 10000)
        (fun n ->
          Result.is_error
            (H.receive (active ()) (wire (checkpoint (Int64.of_int n) "{}"))));
      property "checkpoint duplicate is idempotent and preserves exact ACK"
        QCheck2.Gen.(
          map
            (String.map (fun c -> Char.chr (32 + (Char.code c mod 95))))
            string)
        (fun capsule ->
          let bytes = wire (checkpoint 1L capsule) in
          let t, ack = ok (H.receive (active ()) bytes) in
          let twice, repeated = ok (H.receive t bytes) in
          H.encode twice = H.encode t
          && ack = repeated
          && H.receipt twice = H.receipt t);
      property "terminal/post-ACK receipts are distinct and roundtrip exactly"
        QCheck2.Gen.(
          oneof_list
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
          let t, _ = ok (H.receive (active ()) (wire (checkpoint 1L "{}"))) in
          let t, frames =
            ok
              (H.receive t
                 (wire
                    (P.Result
                       {
                         identity;
                         operation;
                         state_version = 1L;
                         status;
                         diagnostics = "exact";
                         renewal_id =
                           (if status = P.Attention then Some "renew" else None);
                       })))
          in
          let terminal = H.receipt t in
          let acked, ack = ok (H.acknowledge_result t) in
          frames = []
          && List.length ack = 1
          && terminal <> H.receipt acked
          && (not (H.acknowledged t))
          && H.acknowledged acked
          && H.encode (ok (H.decode (H.encode acked))) = H.encode acked);
      property
        "generated checkpoint/stale/restart interleavings preserve receipt and \
         capsule"
        QCheck2.Gen.(list_size (int_range 0 30) bool)
        (fun actions ->
          let t, version =
            List.fold_left
              (fun (t, version) valid ->
                let candidate =
                  checkpoint (Int64.succ version)
                    ("capsule-" ^ Int64.to_string version)
                in
                if valid then (
                  let next, _ = ok (H.receive t (wire candidate)) in
                  let restored = ok (H.decode (H.encode next)) in
                  if
                    H.receipt restored <> H.receipt next
                    || H.capsule restored <> H.capsule next
                  then failwith "roundtrip";
                  (restored, Int64.succ version))
                else
                  let stale =
                    P.Result
                      {
                        identity;
                        operation;
                        state_version = Int64.succ version;
                        status = P.Completed_ok;
                        diagnostics = "";
                        renewal_id = None;
                      }
                  in
                  if Result.is_ok (H.receive t (wire stale)) then
                    failwith "stale accepted";
                  (t, version))
              (active (), 0L)
              actions
          in
          let restored = ok (H.decode (H.encode t)) in
          H.encode restored = H.encode t && version >= 0L);
      property "unfinished replacement derives the exact committed receipt"
        QCheck2.Gen.(int_range 1 12)
        (fun n ->
          let t =
            List.fold_left
              (fun t version ->
                fst
                  (ok
                     (H.receive t
                        (wire
                           (checkpoint (Int64.of_int version)
                              ("capsule-" ^ string_of_int version))))))
              (active ())
              (List.init n (fun i -> i + 1))
          in
          let replacement, frames =
            ok
              (H.rebind t ~worker_id:"replacement" ~workspace_id:"new-workspace")
          in
          H.receipt replacement = H.receipt t
          && H.capsule replacement = H.capsule t
          && H.operation replacement = H.operation t
          && List.length frames = 1);
      property "caller ACKs cannot manufacture receipts" QCheck2.Gen.bool
        (fun result ->
          let t, checkpoint_ack =
            ok (H.receive (active ()) (wire (checkpoint 1L "{}")))
          in
          let finished, _ =
            ok
              (H.receive t
                 (wire
                    (P.Result
                       {
                         identity;
                         operation;
                         state_version = 1L;
                         status = P.Completed_ok;
                         diagnostics = "";
                         renewal_id = None;
                       })))
          in
          let _, result_ack = ok (H.acknowledge_result finished) in
          let frames = if result then result_ack else checkpoint_ack in
          List.for_all (fun frame -> Result.is_error (H.command t frame)) frames);
      property
        "Attention checkpoint interruption binds exact receipt and recovery \
         clears provisional attention"
        QCheck2.Gen.(int_range 1 10000)
        (fun n ->
          let id = "attention-" ^ string_of_int n in
          let capsule =
            Yojson.Safe.to_string (`Assoc [ ("attention_id", `String id) ])
          in
          let t, _ =
            ok (H.receive (active ()) (wire (checkpoint 1L capsule)))
          in
          let restored, _ =
            ok
              (H.rebind t ~worker_id:"replacement" ~workspace_id:"new-workspace")
          in
          let next, _ = ok (H.receive restored (wire (checkpoint 2L "{}"))) in
          H.receipt restored = H.receipt t
          && H.capsule restored = Some capsule
          && H.receipt next <> H.receipt restored);
      property
        "serialized receipt/capsule assertions must equal validated transcript"
        QCheck2.Gen.bool (fun change_receipt ->
          let t = active () in
          let fields =
            match Yojson.Safe.from_string (H.encode t) with
            | `Assoc xs -> xs
            | _ -> []
          in
          let key = if change_receipt then "receipt" else "capsule" in
          let bytes =
            Yojson.Safe.to_string
              (`Assoc
                 (List.map
                    (fun (k, v) ->
                      (k, if k = key then `String "fabricated" else v))
                    fields))
          in
          Result.is_error (H.decode bytes));
      property
        "host stream public projections are derived from one current protocol"
        QCheck2.Gen.bool (fun _ ->
          let t, _ =
            ok
              (Worker_stream.command Worker_stream.initial
                 (ok (V.encode_host (V.Bind (bind (), ctx ())))))
          in
          let t, _ =
            ok
              (Worker_stream.command t
                 (ok (V.encode_host (V.Execute (execute ())))))
          in
          let t, _ = ok (Worker_stream.receive t (wire (checkpoint 1L "{}"))) in
          let t = ok (Worker_stream.decode (Worker_stream.encode t)) in
          let replacement, _ =
            ok
              (Worker_stream.rebind t ~worker_id:"replacement"
                 ~workspace_id:"new")
          in
          Worker_stream.binding t <> None
          && Worker_stream.identity t = Some identity
          && Worker_stream.operation t = Some operation
          && Worker_stream.input t = None
          && Worker_stream.capsule t = Some "{}"
          && Worker_stream.receipt t <> None
          && Worker_stream.terminal t = None
          && (not (Worker_stream.acknowledged t))
          && Result.is_error (Worker_stream.acknowledge_result t)
          && Worker_stream.receipt replacement = Worker_stream.receipt t);
    ]
