(* @archlint.module test
   @archlint.domain orchestrator *)

open Onton_core
module B = Poll_batch
module G = QCheck2.Gen

type event =
  | Begin of int
  | Cancel
  | Deliver of int * int * bool
  | Finish_next
  | Empty

let show = function
  | Begin n -> Printf.sprintf "begin(%d)" n
  | Cancel -> "cancel"
  | Deliver (age, slot, accepted) ->
      Printf.sprintf "deliver(age=%d,slot=%d,accepted=%b)" age slot accepted
  | Finish_next -> "finish-next"
  | Empty -> "empty"

let event =
  G.oneof_weighted
    [
      (2, G.map (fun n -> Begin n) (G.int_range 0 20));
      (1, G.return Cancel);
      ( 6,
        G.map
          (fun (age, (slot, accepted)) -> Deliver (age, slot, accepted))
          (G.pair (G.int_range 0 3) (G.pair (G.int_range 0 22) G.bool)) );
      (8, G.return Finish_next);
      (1, G.return Empty);
    ]

let ticket epoch slot = Printf.sprintf "%d/%d" epoch slot

(* This oracle stores issued/completed deliveries independently, rather than
   consulting the implementation's pending set or completion decision. *)
let interleavings events =
  try
    let epoch = ref 0 in
    let issued = ref [] in
    let delivered = ref [] in
    let closed = ref false in
    let grants = ref 0 in
    let state = ref (B.create []) in
    let complete id =
      let expected =
        if !closed then false
        else
          match id with
          | None -> !issued = []
          | Some id ->
              if List.mem id !issued && not (List.mem id !delivered) then (
                delivered := id :: !delivered;
                List.for_all (fun id -> List.mem id !delivered) !issued)
              else false
      in
      let next, grant = B.complete !state id in
      assert (grant = expected);
      state := next;
      if expected then (
        closed := true;
        incr grants);
      assert (!grants <= 1)
    in
    let finish_next () =
      complete (List.find_opt (fun id -> not (List.mem id !delivered)) !issued)
    in
    List.iter
      (function
        | Begin n ->
            incr epoch;
            issued := List.init n (ticket !epoch);
            delivered := [];
            closed := false;
            grants := 0;
            state := B.create !issued
        | Cancel ->
            state := B.cancel !state;
            closed := true
        | Deliver (age, slot, _accepted) ->
            (* Rejected/error observations complete just like accepted ones;
               acceptance itself is owned by Forge_observation. *)
            complete (Some (ticket (!epoch - age) slot))
        | Finish_next -> finish_next ()
        | Empty -> complete None)
      events;
    (* Fair delivery must eventually grant a live batch's obligation, even
       after arbitrary duplicate, unknown, and rejected deliveries. *)
    let live = not !closed in
    List.iter (fun id -> complete (Some id)) !issued;
    complete None;
    if live then assert (!grants = 1);
    true
  with _ -> false

let tests =
  [
    QCheck2.Test.make
      ~name:
        "poll batch model: completion/rejection/cancellation/replacement \
         interleavings"
      ~count:1000
      ~print:(fun xs -> String.concat "; " (List.map show xs))
      (G.list_size (G.int_range 0 200) event)
      interleavings;
    QCheck2.Test.make
      ~name:
        "poll batch arbitrary identifiers are total and duplicate delivery is \
         inert"
      ~count:500
      (G.pair (G.list G.string) (G.option G.string))
      (fun (ids, id) ->
        try
          let state, _ = B.complete (B.create ids) id in
          let _, duplicate = B.complete state id in
          let _, cancelled = B.complete (B.cancel state) id in
          (not duplicate) && not cancelled
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "poll batch permutations grant exactly once on final distinct \
         completion"
      ~count:500
      (G.list (G.int_range 0 30))
      (fun priorities ->
        try
          let ids = List.mapi (fun i p -> (p, string_of_int i)) priorities in
          let order = List.map snd (List.sort compare ids) in
          let state = ref (B.create (List.map snd ids)) in
          List.iteri
            (fun i id ->
              let next, grant = B.complete !state (Some id) in
              assert (grant = (i = List.length ids - 1));
              let next, duplicate = B.complete next (Some id) in
              assert (not duplicate);
              state := next)
            order;
          let _, empty = B.complete !state None in
          empty = (ids = [])
        with _ -> false);
  ]

let () = QCheck_base_runner.run_tests_main tests
