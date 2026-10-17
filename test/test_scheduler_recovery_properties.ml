(* @archlint.module test
   @archlint.domain scheduler-recovery *)
open Base
open Onton_core
module R = Scheduler_recovery
module G = QCheck2.Gen
module T = QCheck2.Test

let scope n : R.scope =
  {
    head = Some (Int.to_string n);
    base = Some "main";
    pr = Some n;
    publication = None;
    queue = None;
  }

let kinds =
  [
    R.Forge_revision;
    Ci_evidence;
    Queue_removal;
    Native_stack;
    Pr_base;
    Pr_draft;
    Merge_attempt;
  ]

let kind = G.oneof_list kinds

let deadline = function
  | R.Ci_evidence -> 900.
  | Native_stack | Merge_attempt -> 0.
  | Forge_revision | Queue_removal | Pr_base | Pr_draft -> 120.

let observe state kind now =
  R.observe state ~now ~scope:(scope 1) ~blocked:[ kind ]

let get = function Some x -> x | None -> failwith "expected recovery ticket"

let ticket state kind =
  let state = observe state kind 0. in
  let state = observe state kind 1000. in
  (state, get (R.pending state))

let replace fields key value =
  `Assoc ((key, value) :: List.Assoc.remove fields key ~equal:String.equal)

let corrupt_checkpoints () =
  let state, t = ticket R.empty R.Pr_base in
  let json = R.yojson_of_t (R.claim state t) in
  match json with
  | `Assoc fields -> (
      let entries = List.Assoc.find_exn fields "entries" ~equal:String.equal in
      match entries with
      | `List [ (`Assoc entry as original) ] ->
          let with_entry entry = replace fields "entries" (`List [ entry ]) in
          let ticket_json = R.yojson_of_ticket t in
          let phase ticket =
            replace entry "phase" (`List [ `String "Running"; ticket ])
          in
          let invalid_tickets =
            match ticket_json with
            | `Assoc ticket_fields ->
                [
                  replace ticket_fields "id" (`Int 0);
                  replace ticket_fields "id" (`Int (t.id + 1));
                  replace ticket_fields "kind" (`List [ `String "Pr_draft" ]);
                  replace ticket_fields "scope"
                    (match
                       List.Assoc.find_exn ticket_fields "scope"
                         ~equal:String.equal
                     with
                    | `Assoc scope_fields ->
                        replace scope_fields "pr" (`Int 999)
                    | _ -> `Null);
                ]
            | _ -> []
          in
          [
            replace fields "next" (`Int 0);
            replace fields "now" (`Float (-1.));
            replace fields "now" (`Float Float.nan);
            replace fields "entries"
              (`List (List.init 8 ~f:(fun _ -> original)));
            replace fields "entries" (`List [ original; original ]);
            with_entry (replace entry "since" (`Float (-1.)));
            with_entry (replace entry "since" (`Float Float.nan));
            with_entry (replace entry "failures" (`Int (-1)));
            with_entry (replace entry "failures" (`Int 3));
          ]
          @ List.map invalid_tickets ~f:(fun ticket ->
              with_entry (phase ticket))
      | _ -> failwith "expected canonical entry")
  | _ -> failwith "expected canonical checkpoint"

let prop_scope_changes =
  T.make ~name:"changed scope gets a fresh grace and rejects stale completion"
    ~count:500
    G.(pair kind (int_range 1 10000))
    (fun (kind, epoch) ->
      try
        let state, old = ticket R.empty kind in
        let state = R.claim state old in
        let fresh_scope = scope (epoch + 1) in
        let fresh =
          R.observe state ~now:10000. ~scope:fresh_scope ~blocked:[ kind ]
        in
        let failed = R.failed state ~now:10000. ~scope:fresh_scope ~kind in
        let observed =
          R.observe failed ~now:10000. ~scope:fresh_scope ~blocked:[ kind ]
        in
        let twice = R.failed failed ~now:10000. ~scope:fresh_scope ~kind in
        let due =
          R.observe twice ~now:10000. ~scope:fresh_scope ~blocked:[ kind ]
        in
        R.equal fresh (R.complete ~now:10001. fresh old ~success:true)
        && (Float.(R.grace kind = 0.) || Option.is_none (R.pending fresh))
        && (Float.(R.grace kind = 0.) || Option.is_none (R.pending observed))
        && Option.exists (R.pending due) ~f:(fun ticket ->
            R.equal_scope ticket.scope fresh_scope)
        && Option.exists
             (R.pending
                (R.observe fresh
                   ~now:(10000. +. deadline kind)
                   ~scope:fresh_scope ~blocked:[ kind ]))
             ~f:(fun ticket -> R.equal_scope ticket.scope fresh_scope)
      with _ -> false)

let active_check id =
  Types.Ci_check.
    {
      name = "ci";
      id = Some id;
      conclusion = "in_progress";
      started_at = None;
      details_url = None;
      description = None;
      app_id = None;
      check_suite_id = None;
    }

let prop_ci_progress_interleavings =
  T.make ~name:"CI progress renews only unrequested waits across checkpoints"
    ~count:1000
    G.(list (triple (int_range 0 1000) bool bool))
    (fun steps ->
      try
        let observe state now marker =
          R.observe
            ~ci_checks:[ active_check marker ]
            state ~now ~scope:(scope 1) ~blocked:[ R.Ci_evidence ]
        in
        let initial = observe R.empty 0. 0 in
        let _, _, _, _, _, valid =
          List.fold steps ~init:(initial, 0., 0., 0, false, true)
            ~f:(fun
                (state, now, last_progress, marker, requested, valid)
                (elapsed, advance, checkpoint)
              ->
              let now = now +. Float.of_int elapsed in
              let marker = if advance then marker + 1 else marker in
              let last_progress =
                if advance && not requested then now else last_progress
              in
              let expected =
                requested || Float.(now -. last_progress >= 900.)
              in
              let state =
                if checkpoint then R.decode (R.yojson_of_t state) else state
              in
              let state = observe state now marker in
              ( state,
                now,
                last_progress,
                marker,
                expected,
                valid
                && Bool.equal (Option.is_some (R.pending state)) expected
                && Option.is_none (R.attention state) ))
        in
        valid
      with _ -> false)

let prop_ci_repaired_progress =
  T.make
    ~name:"CI repair progress preserves completion and eventually escalates"
    ~count:200
    G.(int_range 1001 2000)
    (fun progressed_at ->
      try
        let observe state now marker =
          R.observe
            ~ci_checks:[ active_check marker ]
            state ~now ~scope:(scope 1) ~blocked:[ R.Ci_evidence ]
        in
        let initial = observe R.empty 0. 1 in
        let due = observe initial 900. 1 in
        let ticket = get (R.pending due) in
        let repaired =
          R.complete ~now:1000. (R.claim due ticket) ticket ~success:true
        in
        let progressed_at = Float.of_int progressed_at in
        let progressing = observe repaired progressed_at 2 in
        let restored = R.decode (R.yojson_of_t progressing) in
        let before = observe restored (progressed_at +. 899.) 2 in
        let after = observe before (progressed_at +. 900.) 2 in
        Option.is_none (R.pending before)
        && Option.is_none (R.attention before)
        && Option.is_none (R.pending after)
        && Option.is_some (R.attention after)
      with _ -> false)

let prop_legacy_ci_checkpoint =
  T.make
    ~name:
      "legacy checkpoints retain CI ticket authority without progress metadata"
    ~count:200
    G.(int_range 0 4)
    (fun phase ->
      try
        let observed =
          R.observe
            ~ci_checks:[ active_check 1 ]
            R.empty ~now:0. ~scope:(scope 1) ~blocked:[ R.Ci_evidence ]
        in
        let due =
          R.observe
            ~ci_checks:[ active_check 1 ]
            observed ~now:900. ~scope:(scope 1) ~blocked:[ R.Ci_evidence ]
        in
        let ticket = get (R.pending due) in
        let running = R.claim due ticket in
        let state =
          match phase with
          | 0 -> observed
          | 1 -> due
          | 2 -> running
          | 3 -> R.complete ~now:1000. running ticket ~success:true
          | _ -> R.complete ~now:1000. running ticket ~success:false
        in
        let legacy =
          match R.yojson_of_t state with
          | `Assoc fields ->
              let entries =
                List.Assoc.find_exn fields "entries" ~equal:String.equal
              in
              let entries =
                match entries with
                | `List entries ->
                    `List
                      (List.map entries ~f:(function
                        | `Assoc fields ->
                            `Assoc
                              (List.Assoc.remove fields "progress"
                                 ~equal:String.equal)
                        | other -> other))
                | other -> other
              in
              replace fields "entries" entries
          | other -> other
        in
        let restored = R.decode legacy in
        R.contains restored R.Ci_evidence
        && Option.equal R.equal_ticket (R.pending state) (R.pending restored)
        && Option.equal String.equal (R.attention state) (R.attention restored)
        && Bool.equal (R.is_running state ticket) (R.is_running restored ticket)
        && Bool.equal
             (R.write_allowed state R.Ci_evidence)
             (R.write_allowed restored R.Ci_evidence)
      with _ -> false)

let tests =
  [
    prop_legacy_ci_checkpoint;
    prop_ci_progress_interleavings;
    prop_ci_repaired_progress;
    prop_scope_changes;
    T.make ~name:"malformed canonical checkpoints reset all repair authority"
      ~count:100 G.unit (fun () ->
        try
          List.for_all (corrupt_checkpoints ()) ~f:(fun json ->
              R.equal (R.decode json) R.empty)
        with _ -> false);
    T.make
      ~name:
        "scheduler recovery: total over malformed checkpoints and arbitrary \
         clocks"
      ~count:1000
      G.(
        pair
          (oneof
             [
               map (fun s -> `String s) string;
               map (fun i -> `Int i) int;
               return (`Float Float.nan);
               return (`Assoc []);
             ])
          (pair kind (oneof [ float; return Float.infinity; return Float.nan ])))
      (fun (json, (kind, now)) ->
        try
          let state = R.decode json in
          assert (R.equal state R.empty);
          assert (Option.is_none (R.decode_ticket json));
          let state = observe state kind now in
          ignore (R.decode (R.yojson_of_t state) : R.t);
          ignore (R.decode_ticket json : R.ticket option);
          true
        with _ -> false);
    T.make
      ~name:
        "scheduler recovery: fixed deadline, inclusive boundary, no heartbeat \
         churn" ~count:500 kind (fun kind ->
        try
          let state = observe R.empty kind 10. in
          if Float.(deadline kind = 0.) then Option.is_some (R.pending state)
          else
            let before = observe state kind (10. +. deadline kind -. 0.01) in
            R.equal state before
            && Option.is_none (R.pending before)
            && Option.is_some
                 (R.pending (observe before kind (10. +. deadline kind)))
        with _ -> false);
    T.make
      ~name:
        "scheduler recovery: two write failures request one repair and stop \
         writes"
      ~count:500
      (G.oneof_list [ R.Pr_base; R.Pr_draft ])
      (fun kind ->
        try
          let state = observe R.empty kind 0. in
          let fail state = R.failed state ~now:1. ~scope:(scope 1) ~kind in
          let state = observe (fail (fail state)) kind 1. in
          let ticket = get (R.pending state) in
          (not (R.write_allowed state kind))
          && R.equal_ticket ticket (get (R.pending (observe state kind 10000.)))
          && R.equal state (R.decode (R.yojson_of_t state))
        with _ -> false);
    T.make
      ~name:
        "scheduler recovery: accepted completion is retained; persistent gate \
         prompts operator" ~count:500 kind (fun kind ->
        try
          let state, ticket = ticket R.empty kind in
          let state =
            R.claim state ticket |> fun s ->
            R.complete ~now:2000. s ticket ~success:true
          in
          let wait = Float.max 120. (deadline kind) in
          let before = observe state kind (2000. +. wait -. 0.01) in
          let after = observe before kind (2000. +. wait) in
          Option.is_none (R.pending before)
          && Option.is_none (R.attention before)
          && Option.is_none (R.pending after)
          && Option.is_some (R.attention after)
          && R.equal after (R.complete ~now:1e8 after ticket ~success:false)
          && Option.is_none
               (R.attention
                  (R.observe after ~now:1e9 ~scope:(scope 1) ~blocked:[]))
        with _ -> false);
    T.make
      ~name:
        "scheduler recovery: stale completions cannot discharge a new revision"
      ~count:500 kind (fun kind ->
        try
          let state, old = ticket R.empty kind in
          let state = R.claim state old in
          let state =
            R.observe state ~now:1000. ~scope:(scope 2) ~blocked:[ kind ]
          in
          R.equal state (R.complete ~now:10000. state old ~success:true)
          && R.equal state (R.complete ~now:10000. state old ~success:false)
        with _ -> false);
    T.make ~name:"scheduler recovery: restart requeues lost execution ownership"
      ~count:500 (G.pair kind G.bool) (fun (kind, owned) ->
        try
          let state, ticket = ticket R.empty kind in
          let state = R.claim state ticket |> R.yojson_of_t |> R.decode in
          let restored =
            R.recover state ~running:(if owned then Some ticket else None)
          in
          if owned then
            R.is_running restored ticket && Option.is_none (R.pending restored)
          else Option.equal R.equal_ticket (R.pending restored) (Some ticket)
        with _ -> false);
    T.make
      ~name:
        "scheduler recovery: arbitrary observation, completion and restart \
         interleavings remain live"
      ~count:1000
      G.(pair kind (list (pair (int_range 0 7) (int_range 0 1000))))
      (fun (kind, ops) ->
        try
          let state, now, last, epoch =
            List.fold ops ~init:(R.empty, 0., None, 1)
              ~f:(fun (state, now, last, epoch) (op, advance) ->
                let now = now +. Float.of_int advance in
                let observe state blocked =
                  R.observe state ~now ~scope:(scope epoch) ~blocked
                in
                let state, last, epoch =
                  match op with
                  | 0 -> (observe state [], last, epoch)
                  | 1 -> (observe state [ kind ], last, epoch)
                  | 2 -> (
                      match R.pending state with
                      | None -> (state, last, epoch)
                      | Some t ->
                          let claimed = R.claim state t in
                          assert (R.is_running claimed t);
                          assert (Option.is_none (R.pending claimed));
                          (claimed, Some t, epoch))
                  | 3 | 4 ->
                      ( Option.value_map last ~default:state ~f:(fun t ->
                            let completed =
                              R.complete ~now state t ~success:(op = 3)
                            in
                            if R.is_running state t then (
                              assert (not (R.is_running completed t));
                              assert (Option.is_none (R.pending completed));
                              assert (
                                Bool.equal
                                  (Option.is_some (R.attention completed))
                                  (op = 4)))
                            else assert (R.equal state completed);
                            completed),
                        last,
                        epoch )
                  | 5 ->
                      let restored = R.decode (R.yojson_of_t state) in
                      assert (R.equal restored state);
                      let recovered = R.recover restored ~running:None in
                      Option.iter last ~f:(fun ticket ->
                          if R.is_running state ticket then
                            assert (
                              Option.equal R.equal_ticket (R.pending recovered)
                                (Some ticket)));
                      (recovered, last, epoch)
                  | 6 ->
                      ( R.failed state ~now ~scope:(scope epoch) ~kind,
                        last,
                        epoch )
                  | _ -> (state, last, epoch + 1)
                in
                (state, now, last, epoch))
          in
          let scope = scope epoch in
          let observed = R.observe state ~now ~scope ~blocked:[ kind ] in
          let due =
            R.observe observed ~now:(now +. 10000.) ~scope ~blocked:[ kind ]
          in
          (* No closing recovery/completion may conceal a broken transition. *)
          Option.is_some (R.pending due)
          || Option.is_some (R.attention due)
          || Option.exists last ~f:(R.is_running due)
        with _ -> false);
  ]

let () = QCheck_base_runner.run_tests_main tests
