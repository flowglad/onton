(* @archlint.module test
   @archlint.domain coordinator-state *)
open Onton_core

[@@@warning "-4-40-42"]

module S = Coordinator_state
module V = Worker_protocol_v3

let patch = Types.Patch_id.of_string "patch"
let other_patch = Types.Patch_id.of_string "patch-b"

let initial () =
  let s =
    S.create ~run_id:"run" ~config_id:"config" ~plan_id:"plan"
      ~plan_bytes:"plan" ~config_bytes:"config" ~capacity:2
      ~budget_bytes:8388608
  in
  fst
    (S.step s
       (Admit
          {
            id = "op";
            patch;
            used = 0;
            budget = 3;
            retry_after = 0L;
            attention = false;
            unfinished = true;
            maintenance = false;
          }))

let tests =
  [
    QCheck2.Test.make ~count:400
      ~name:
        "cumulative delivery includes every pending identity and preserves \
         input order"
      QCheck2.Gen.(list_size (int_range 0 20) (int_range 0 2))
      (fun kinds ->
        try
          let state, _ =
            List.fold_left
              (fun (state, n) kind ->
                let kind, payload =
                  match kind with
                  | 0 -> ("guidance", "guide")
                  | 1 -> ("automerge", "false")
                  | _ -> ("bump", "")
                in
                let state, out =
                  S.step state
                    (Control
                       {
                         id = "control-" ^ string_of_int n;
                         run_id = "run";
                         config_id = "config";
                         patch_id = Some "patch";
                         kind;
                         payload;
                       })
                in
                if out <> [ S.Accepted ] then failwith "control rejected";
                (state, n + 1))
              (initial (), 0)
              kinds
          in
          match
            Coordinator_inputs.pending state patch ~selected_base:"main"
          with
          | Error _ -> false
          | Ok None -> kinds = []
          | Ok (Some input) ->
              List.length input.covered = List.length kinds
              && input.version = Int64.of_int (List.length kinds)
              && (match V.decode_input (V.input_json input) with
                | Ok next -> next = input
                | Error _ -> false)
              && input.guidance
                 = String.concat "\n\n"
                     (List.filter_map
                        (function 0 -> Some "guide" | _ -> None)
                        kinds)
              && input.controls
                 = List.filter_map
                     (function
                       | 1 -> Some ("automerge", "false")
                       | 2 -> Some ("bump", "")
                       | _ -> None)
                     kinds
              && input.selected_base = "main"
              && input.covered
                 = List.mapi
                     (fun n _ ->
                       ("control-" ^ string_of_int n, Int64.of_int (n + 1)))
                     kinds
        with _ -> false);
    QCheck2.Test.make ~count:1
      ~name:
        "input lookup scopes control ids and propagates nested input coverage"
      QCheck2.Gen.unit (fun () ->
        try
          let ( let* ) option f =
            match option with None -> None | Some value -> f value
          in
          let step state event =
            let state, outcomes = S.step state event in
            if
              Stdlib.List.exists
                (function
                  | S.Accepted | S.Dispatch _ -> true
                  | S.Rejected _ | S.Already_applied | S.Recover _
                  | S.Maintain _ | S.Attention _ | S.Retire _
                  | S.Ready_to_sleep _ | S.Cancel_complete ->
                      false)
                outcomes
            then Some state
            else None
          in
          let assignment : S.assignment =
            {
              id = "assignment-b";
              operation = "op-b";
              patch = other_patch;
              resource = "resource-b";
              request = "attempt-b";
              reserved = true;
              retired = false;
              result_committed = false;
              cleanup = false;
            }
          in
          let input : V.input =
            {
              input_id = "shared-id";
              version = 2L;
              guidance = "guide";
              selected_base = "main";
              evidence = [];
              controls = [];
              covered = [ ("nested-id", 1L) ];
            }
          in
          let prepared =
            let* state =
              step (initial ())
                (Admit
                   {
                     id = "op-b";
                     patch = other_patch;
                     used = 0;
                     budget = 3;
                     retry_after = 0L;
                     attention = false;
                     unfinished = true;
                     maintenance = false;
                   })
            in
            let* state = step state (Input ("old-id", "patch-b", 1L, "old")) in
            let* state =
              step state (Resource_request ("resource-b", "launch-bytes"))
            in
            let* state =
              step state (Bind_resource ("resource-b", other_patch))
            in
            let* state = step state (Assign (assignment, 0L)) in
            let* state =
              step state
                (Owner_receipt
                   {
                     assignment = assignment.id;
                     version = 1L;
                     bytes = "capsule";
                     receipt = "receipt";
                   })
            in
            let* state =
              step state (Input_consumed (assignment.id, "old-id", 1L))
            in
            let* state =
              step state
                (Control
                   {
                     id = "shared-id";
                     run_id = "run";
                     config_id = "config";
                     payload = "opaque";
                     kind = "worker_observation";
                     patch_id = Some "patch";
                   })
            in
            let* payload =
              match
                V.encode_host
                  (V.Input ({ run_id = "run"; patch_id = "patch-b" }, input))
              with
              | Error _ -> None
              | Ok payload -> Some payload
            in
            step state (Input ("shared-id", "patch-b", 2L, payload))
          in
          match prepared with
          | None -> false
          | Some state -> (
              match
                Coordinator_inputs.pending state other_patch
                  ~selected_base:"main"
              with
              | Error _ | Ok None -> false
              | Ok (Some pending) ->
                  pending.guidance = "guide"
                  && List.equal
                       (fun (left_id, left_v) (right_id, right_v) ->
                         String.equal left_id right_id
                         && Int64.equal left_v right_v)
                       pending.covered
                       [ ("nested-id", 1L); ("shared-id", 2L) ])
        with _ -> false);
    QCheck2.Test.make ~count:400
      ~name:"input codec is total over arbitrary JSON bytes" QCheck2.Gen.string
      (fun bytes ->
        try
          ignore (V.decode_input (Yojson.Safe.from_string bytes));
          true
        with
        | Yojson.Json_error _ -> true
        | _ -> false);
  ]

let () = QCheck_base_runner.run_tests_main tests
