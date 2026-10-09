(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton_core
module B = Branch_reconcile
module G = QCheck2.Gen
open B

let intent =
  B.
    {
      base = "main";
      policy = Rewrite;
      purpose = Provision_checkout "provision";
    }

let pending state =
  match B.operation state with
  | Some { pending = Some command; _ } -> command
  | Some { pending = None; _ } | None -> failwith "expected pending command"

let restore state =
  match B.decode (B.yojson_of_t state) with
  | Ok restored -> restored
  | Error reason -> failwith reason

let inspections_only commands =
  List.for_all
    (function
      | B.Execute { kind = Observe | Inspect; _ } | B.Completed _ -> true
      | B.Execute
          {
            kind =
              ( Commit_merge _ | Plan_remote_replay _ | Checkout_remote _
              | Pin _ | Integrate _ | Verify_recovery | Continue _ | Publish _
              | Confirm _ );
            _;
          }
      | B.Repair _ | B.Start_repair _ ->
          false)
    commands

let result state result =
  let command = pending state in
  B.step state (B.Result { token = command.token; at = 1.; result })

let replace key value = function
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (name, prior) -> (name, if name = key then value else prior))
           fields)
  | _ -> failwith "expected checkpoint object"

let tests =
  [
    QCheck2.Test.make
      ~name:"new provisioning requests recheck settled checkouts" ~count:200
      G.string (fun suffix ->
        try
          let first, _ = B.step B.empty (B.Request intent) in
          let old = pending first in
          let settled, _ = result first B.Checkout_ready in
          let next_intent =
            { intent with purpose = Provision_checkout ("request:" ^ suffix) }
          in
          let next, commands =
            B.step (restore settled) (B.Request next_intent)
          in
          let current = pending next in
          let stale, effects =
            B.step next
              (B.Result { token = old.token; at = 2.; result = Checkout_ready })
          in
          B.is_provisioning next_intent.purpose
          && inspections_only commands && commands <> []
          && old.token.operation <> current.token.operation
          && B.equal stale next && effects = []
          && B.equal (restore next) next
        with _ -> false);
    QCheck2.Test.make
      ~name:"empty provisioning request cannot create an invalid checkpoint"
      ~count:1 G.unit (fun () ->
        try
          let state, commands =
            B.step B.empty
              (B.Request { intent with purpose = Provision_checkout "" })
          in
          B.equal state B.empty && commands = []
          && B.equal (restore state) state
        with _ -> false);
    QCheck2.Test.make
      ~name:"provisioning settles without integration or publication" ~count:100
      G.bool (fun preserve ->
        try
          let requested =
            {
              intent with
              policy = (if preserve then Preserve_ancestry else Rewrite);
            }
          in
          let state, commands = B.step B.empty (B.Request requested) in
          let token = (pending state).token in
          let state, completed = result (restore state) B.Checkout_ready in
          let unchanged, duplicate =
            B.step state
              (B.Result { token; at = 2.; result = B.Checkout_ready })
          in
          let same, repeated = B.step state (B.Request requested) in
          inspections_only commands && inspections_only completed
          && B.phase state = Some B.Settled
          && B.publications state = []
          && B.integrations state = []
          && B.equal unchanged state && duplicate = [] && B.equal same state
          && repeated = []
          && B.equal (restore state) state
        with _ -> false);
    QCheck2.Test.make
      ~name:"provisioning retries retain identity and reject stale success"
      ~count:200 G.string (fun reason ->
        try
          let initial, _ = B.step B.empty (B.Request intent) in
          let original = pending initial in
          let waiting, effects =
            result initial (B.Retryable { reason; retry_after = None })
          in
          let restored = restore waiting in
          let next, commands = B.step restored (B.Tick 1000.) in
          let current = pending next in
          let stale, ignored =
            B.step next
              (B.Result
                 { token = original.token; at = 1001.; result = Checkout_ready })
          in
          original.token.operation = current.token.operation
          && original.token.command <> current.token.command
          && effects = [] && inspections_only commands && B.equal stale next
          && ignored = []
          && B.phase waiting
             = Some
                 (B.Waiting
                    {
                      until = 6.;
                      reason =
                        (if reason = "" then "reconciliation_retry" else reason);
                    })
        with _ -> false);
    QCheck2.Test.make
      ~name:"provisioning interruption histories never grant mutation authority"
      ~count:500
      (G.list_size (G.int_range 0 100) (G.int_range 0 6))
      (fun history ->
        try
          let initial, _ = B.step B.empty (B.Request intent) in
          let original = pending initial in
          let _, valid =
            List.fold_left
              (fun (state, valid) action ->
                let state = restore state in
                let event =
                  match action with
                  | 0 -> B.Recover
                  | 1 -> B.Tick 100000.
                  | 2 -> B.Resume
                  | 3 -> B.Request intent
                  | 4 ->
                      B.Result
                        {
                          token = original.token;
                          at = 0.;
                          result = Checkout_ready;
                        }
                  | _ -> (
                      match B.operation state with
                      | Some { pending = Some command; _ } ->
                          B.Result
                            {
                              token = command.token;
                              at = 0.;
                              result =
                                (if action = 5 then
                                   Retryable
                                     { reason = "offline"; retry_after = None }
                                 else Permanent "unsafe checkout");
                            }
                      | Some { pending = None; _ } | None -> B.Recover)
                in
                let next, commands = B.step state event in
                ( next,
                  valid && inspections_only commands
                  && B.publications next = []
                  && B.integrations next = []
                  && B.equal (restore next) next ))
              (initial, true) history
          in
          valid
        with _ -> false);
    QCheck2.Test.make
      ~name:"publication acknowledgements cannot complete provisioning"
      ~count:100 G.bool (fun use_pinned ->
        try
          let initial, _ = B.step B.empty (B.Request intent) in
          let stopped, effects =
            result initial (if use_pinned then B.Pinned else B.Published)
          in
          B.phase stopped = Some (B.Intervention "provisioning_result_mismatch")
          && effects = []
          && B.publications stopped = []
          && B.equal (restore stopped) stopped
        with _ -> false);
    QCheck2.Test.make
      ~name:"provisioning checkpoints reject invented publication authority"
      ~count:200 G.bool (fun receipt ->
        try
          let state, _ = B.step B.empty (B.Request intent) in
          let state, _ = result state B.Checkout_ready in
          let json = B.yojson_of_t state in
          let forged =
            if receipt then
              replace "publications"
                (`List
                   [
                     `Assoc
                       [
                         ("operation_id", `Int 1);
                         ( "published_intent",
                           `Assoc
                             [
                               ("base", `String "main");
                               ("policy", `List [ `String "Rewrite" ]);
                               ( "purpose",
                                 `List
                                   [
                                     `String "Provision_checkout";
                                     `String "provision";
                                   ] );
                             ] );
                         ("published_revision", `String (String.make 40 'a'));
                       ];
                   ])
                json
            else
              match json with
              | `Assoc fields ->
                  let active = List.assoc "active" fields in
                  replace "active"
                    (replace "candidate" (`String (String.make 40 'a')) active)
                    json
              | _ -> failwith "checkpoint"
          in
          Result.is_error (B.decode forged)
        with _ -> false);
    QCheck2.Test.make
      ~name:"provisioning preserves a queued reconciliation intent" ~count:200
      G.bool (fun publish ->
        try
          let state, _ = B.step B.empty (B.Request intent) in
          let first = pending state in
          let requested =
            {
              intent with
              purpose =
                (if publish then Publish_session "session" else Reconcile_base);
            }
          in
          let queued, effects = B.step state (B.Request requested) in
          let next, commands = result queued B.Checkout_ready in
          let second = pending next in
          let ignored, stale_effects =
            B.step next
              (B.Result
                 { token = first.token; at = 2.; result = Checkout_ready })
          in
          effects = []
          && first.token.operation <> second.token.operation
          && inspections_only commands
          && B.publications next = []
          && B.equal ignored next && stale_effects = []
          && (match B.operation next with
            | Some op ->
                (not (B.is_provisioning op.intent.purpose))
                && B.equal_intent op.intent requested
            | None -> false)
          && B.equal (restore next) next
        with _ -> false);
    QCheck2.Test.make
      ~name:"checkout completion cannot complete a reconciliation request"
      ~count:100 G.bool (fun publish ->
        try
          let purpose =
            if publish then B.Publish_session "session" else B.Reconcile_base
          in
          let state, _ = B.step B.empty (B.Request { intent with purpose }) in
          let stopped, effects = result state B.Checkout_ready in
          B.phase stopped = Some (B.Intervention "command_result_mismatch")
          && effects = []
          && B.publications stopped = []
        with _ -> false);
  ]

let () = QCheck_base_runner.run_tests_main tests
