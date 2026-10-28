(* @archlint.module test
   @archlint.domain coordinator-state *)
open Onton_core

[@@@warning "-4-40-42"]

module S = Coordinator_state
module V = Worker_protocol_v3

let patch = Types.Patch_id.of_string "patch"

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
          | None -> kinds = []
          | Some input ->
              List.length input.covered = List.length kinds
              && input.version = Int64.of_int (List.length kinds)
              && (match V.decode_input (V.input_json input) with
                | Ok next -> next = input
                | Error _ -> false)
              && input.covered
                 = List.mapi
                     (fun n _ ->
                       ("control-" ^ string_of_int n, Int64.of_int (n + 1)))
                     kinds
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
