open Base
open Onton_core
open Onton_core.Types

(** Property tests for [Poll_cycle.classify] / [Poll_cycle.plan].

    The poll cycle is split into:
    - an effectful per-patch driver (one fiber per patch, with
      [Eio.Time.with_timeout] around the HTTPS call) that translates the result
      into a {!Poll_outcome.t};
    - a pure [Poll_cycle.classify] / [Poll_cycle.plan] that turns the outcome
      into a [classification].

    These properties exercise the pure side under arbitrary {!Poll_outcome.t}
    interleavings to assert the guarantees the production bug exploited: a hang
    on one patch cannot leak into another patch's state, and the planning layer
    is total over the entire outcome variant. *)

(* -- Generators -- *)

let gen_patch_id =
  QCheck2.Gen.(map (fun i -> Patch_id.of_string (Printf.sprintf "p%d" i)) nat)

let gen_pr_number = QCheck2.Gen.(map Pr_number.of_int (int_range 1 9999))
let gen_was_merged = QCheck2.Gen.bool
let gen_pr_state = Onton_test_support.Test_generators.gen_pr_state

let gen_outcome =
  let open QCheck2.Gen in
  let timed_out =
    map (fun s -> Poll_outcome.Timed_out { seconds = s }) (float_range 1.0 60.0)
  in
  let transport =
    map
      (fun msg -> Poll_outcome.Transport_failed { msg })
      (string_size (int_range 0 32))
  in
  let http =
    map2
      (fun status msg -> Poll_outcome.Http_failed { status; msg })
      (oneof_list [ 400; 401; 403; 404; 422; 500; 502; 503 ])
      (string_size (int_range 0 32))
  in
  let graphql =
    map
      (fun msgs -> Poll_outcome.Graphql_failed msgs)
      (list_size (int_range 0 4) (string_size (int_range 0 16)))
  in
  let parse =
    map
      (fun msg -> Poll_outcome.Json_parse_failed msg)
      (string_size (int_range 0 32))
  in
  let ok = map (fun pr -> Poll_outcome.Ok_pr_state pr) gen_pr_state in
  oneof [ ok; timed_out; transport; http; graphql; parse ]

let gen_input =
  let open QCheck2.Gen in
  let* patch_id = gen_patch_id in
  let* pr_number = gen_pr_number in
  let* was_merged = gen_was_merged in
  let* outcome = gen_outcome in
  return Poll_cycle.{ patch_id; pr_number; was_merged; outcome }

(* Dedupe by patch_id within a list — distinct patches per cycle. *)
let dedupe_by_patch_id (inputs : Poll_cycle.input list) =
  let seen = Hash_set.create (module Patch_id) in
  List.filter inputs ~f:(fun i ->
      if Hash_set.mem seen i.Poll_cycle.patch_id then false
      else (
        Hash_set.add seen i.Poll_cycle.patch_id;
        true))

let gen_inputs =
  QCheck2.Gen.(
    let* inputs = list_size (int_range 0 12) gen_input in
    return (dedupe_by_patch_id inputs))

(* -- Properties -- *)

let print_inputs (inputs : Poll_cycle.input list) =
  Printf.sprintf "[%s]"
    (String.concat ~sep:"; "
       (List.map inputs ~f:(fun i ->
            Printf.sprintf "(%s,#%d,%s)"
              (Patch_id.to_string i.Poll_cycle.patch_id)
              (Pr_number.to_int i.Poll_cycle.pr_number)
              (Poll_outcome.show i.Poll_cycle.outcome))))

let prop_totality =
  let open QCheck2 in
  Test.make ~name:"Poll_cycle.classify is total over all outcomes" ~count:2000
    ~print:(fun i ->
      Printf.sprintf "(%s,%s)"
        (Patch_id.to_string i.Poll_cycle.patch_id)
        (Poll_outcome.show i.Poll_cycle.outcome))
    gen_input
    (fun input ->
      try
        let _ = Poll_cycle.classify input in
        true
      with _ -> false)

let prop_plan_preserves_length =
  let open QCheck2 in
  Test.make ~name:"Poll_cycle.plan output has the same length as input"
    ~count:500 ~print:print_inputs gen_inputs (fun inputs ->
      let plans = Poll_cycle.plan inputs in
      List.length plans = List.length inputs)

let prop_plan_preserves_order =
  let open QCheck2 in
  Test.make ~name:"Poll_cycle.plan preserves input ordering by patch_id"
    ~count:500 ~print:print_inputs gen_inputs (fun inputs ->
      let input_ids = List.map inputs ~f:(fun i -> i.Poll_cycle.patch_id) in
      let plan_ids =
        List.map (Poll_cycle.plan inputs) ~f:(fun (pid, _, _) -> pid)
      in
      List.equal Patch_id.equal input_ids plan_ids)

let prop_classify_per_input_only =
  (* Per-patch isolation: classify's output for a given input is identical no
     matter what surrounds it. This is automatic from the type signature
     (classify : input -> classification) but asserts it behaviourally. *)
  let open QCheck2 in
  Test.make
    ~name:
      "Poll_cycle.classify output is determined solely by its own input \
       (per-patch isolation)"
    ~count:500
    ~print:(fun (input, surrounding) ->
      Printf.sprintf "input=(%s,%s); surrounding=%s"
        (Patch_id.to_string input.Poll_cycle.patch_id)
        (Poll_outcome.show input.Poll_cycle.outcome)
        (print_inputs surrounding))
    (Gen.pair gen_input gen_inputs)
    (fun (input, surrounding) ->
      let plans_with =
        Poll_cycle.plan
          (input
          :: List.filter surrounding ~f:(fun i ->
              not
                (Patch_id.equal i.Poll_cycle.patch_id input.Poll_cycle.patch_id))
          )
      in
      let plans_alone = Poll_cycle.plan [ input ] in
      match (plans_with, plans_alone) with
      | (pid1, _, cls1) :: _, [ (pid2, _, cls2) ] ->
          Patch_id.equal pid1 pid2 && Poll_cycle.equal_classification cls1 cls2
      | _ -> false)

let prop_timeout_substitution_isolates =
  (* THE central property: substituting any patch's outcome with [Timed_out]
     leaves every OTHER patch's classification unchanged.

     This is the property whose violation was the bug — the production poller
     blocked all patches on one stuck connect. With the new fan-out, each
     patch's outcome is independent; the pure planning layer must reflect
     that. *)
  let open QCheck2 in
  Test.make
    ~name:
      "substituting Timed_out for any subset leaves other patches' \
       classifications unchanged"
    ~count:500
    ~print:(fun (inputs, mask) ->
      Printf.sprintf "inputs=%s; mask=[%s]" (print_inputs inputs)
        (String.concat ~sep:";" (List.map mask ~f:Bool.to_string)))
    Gen.(
      let* inputs = gen_inputs in
      let* mask = list_size (return (List.length inputs)) bool in
      return (inputs, mask))
    (fun (inputs, mask) ->
      let mask =
        List.take mask (List.length inputs)
        @ List.init
            (Int.max 0 (List.length inputs - List.length mask))
            ~f:(fun _ -> false)
      in
      let timeout_s = 30.0 in
      let timeout = Poll_outcome.Timed_out { seconds = timeout_s } in
      let substituted =
        List.map2_exn inputs mask ~f:(fun input replace ->
            if replace then { input with Poll_cycle.outcome = timeout }
            else input)
      in
      let plans_orig = Poll_cycle.plan inputs in
      let plans_sub = Poll_cycle.plan substituted in
      List.for_all2_exn plans_orig plans_sub
        ~f:(fun (pid1, _, cls1) (pid2, _, cls2) ->
          if not (Patch_id.equal pid1 pid2) then false
          else
            let was_substituted =
              let n =
                List.findi inputs ~f:(fun _ i ->
                    Patch_id.equal i.Poll_cycle.patch_id pid1)
                |> Option.map ~f:fst |> Option.value ~default:(-1)
              in
              n >= 0 && List.nth_exn mask n
            in
            if was_substituted then
              (* If the patch was substituted, its classification may differ —
                 but it must be Log_error (since timeouts always classify as
                 Log_error). That's a separate sub-property. *)
              match cls2 with
              | Poll_cycle.Log_error _ -> true
              | Apply_pr_state _ | Skip_fork _ | Rediscover_pr _ -> false
            else Poll_cycle.equal_classification cls1 cls2))

let prop_permutation_invariance =
  (* Reordering inputs yields the same multiset of (patch_id, classification)
     pairs. Combined with order preservation, this is a sanity check on
     plan = List.map classify. *)
  let open QCheck2 in
  Test.make
    ~name:
      "Poll_cycle.plan is permutation-invariant on (patch_id, classification) \
       multiset" ~count:500 ~print:print_inputs gen_inputs (fun inputs ->
      let shuffled = List.rev inputs in
      let key_set plans =
        List.map plans ~f:(fun (pid, _, cls) ->
            (Patch_id.to_string pid, Poll_cycle.show_classification cls))
        |> List.sort ~compare:(fun (a1, b1) (a2, b2) ->
            match String.compare a1 a2 with 0 -> String.compare b1 b2 | n -> n)
      in
      List.equal
        (fun (a1, b1) (a2, b2) -> String.equal a1 a2 && String.equal b1 b2)
        (key_set (Poll_cycle.plan inputs))
        (key_set (Poll_cycle.plan shuffled)))

let prop_timeout_always_log_error =
  let open QCheck2 in
  Test.make ~name:"Poll_cycle.classify of Timed_out is always Log_error"
    ~count:1000
    Gen.(pair gen_patch_id (float_range 0.0 600.0))
    (fun (patch_id, seconds) ->
      let input =
        Poll_cycle.
          {
            patch_id;
            pr_number = Pr_number.of_int 1;
            was_merged = false;
            outcome = Poll_outcome.Timed_out { seconds };
          }
      in
      match Poll_cycle.classify input with
      | Log_error { message } ->
          Base.String.is_substring message ~substring:"timed out"
      | Apply_pr_state _ | Skip_fork _ | Rediscover_pr _ -> false)

let () =
  QCheck_base_runner.run_tests_main
    [
      prop_totality;
      prop_plan_preserves_length;
      prop_plan_preserves_order;
      prop_classify_per_input_only;
      prop_timeout_substitution_isolates;
      prop_permutation_invariance;
      prop_timeout_always_log_error;
    ]
