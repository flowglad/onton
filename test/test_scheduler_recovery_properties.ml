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

let tests =
  [
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
                      | Some t -> (R.claim state t, Some t, epoch))
                  | 3 | 4 ->
                      ( Option.value_map last ~default:state ~f:(fun t ->
                            R.complete ~now state t ~success:(op = 3)),
                        last,
                        epoch )
                  | 5 ->
                      ( R.recover (R.decode (R.yojson_of_t state)) ~running:None,
                        last,
                        epoch )
                  | 6 ->
                      ( R.failed state ~now ~scope:(scope epoch) ~kind,
                        last,
                        epoch )
                  | _ -> (state, last, epoch + 1)
                in
                (state, now, last, epoch))
          in
          let state =
            Option.value_map last ~default:state ~f:(fun t ->
                R.complete ~now state t ~success:true)
          in
          let state = R.recover state ~running:None in
          let state =
            R.observe state ~now ~scope:(scope epoch) ~blocked:[ kind ]
          in
          let state =
            R.observe state ~now:(now +. 10000.) ~scope:(scope epoch)
              ~blocked:[ kind ]
          in
          Option.is_some (R.pending state) || Option.is_some (R.attention state)
        with _ -> false);
  ]

let () = QCheck_base_runner.run_tests_main tests
