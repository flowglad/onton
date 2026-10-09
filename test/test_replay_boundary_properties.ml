(* @archlint.module test
   @archlint.domain branch-reconcile *)
open Base
open Onton_core
module B = Branch_reconcile
module F = Onton_core_test_support.Replay_fixture
module G = QCheck2.Gen

let property name gen f =
  QCheck2.Test.make ~name ~count:500 gen (fun input ->
      try f input with _ -> false)

let same = List.equal B.equal_boundary

let boundary_gen =
  G.(
    map2
      (fun n kind ->
        let revision = F.commit n in
        match kind with
        | 0 -> B.Recorded revision
        | 1 -> B.Inferred revision
        | 2 ->
            B.Reconstructed
              { original = F.commit (n + 100); upstream = revision }
        | 3 -> B.Patch_equivalent revision
        | 4 -> B.Subject_inferred revision
        | _ -> B.Plain)
      (int_range 1 30) (int_range 0 5))

let () =
  QCheck_base_runner.run_tests_main
    [
      property
        "replay choice totality with arbitrary boundary/evidence sequences"
        G.(pair (list boundary_gen) (list (pair (int_range 1 30) bool)))
        (fun (candidates, evidence) ->
          ignore
            (B.choose_boundary ~candidates
               ~evidence:
                 (List.map evidence ~f:(fun (n, reachable) ->
                      (F.commit n, reachable))));
          true);
      property "replay choice honors first reachable recorded boundary"
        G.(list (pair (int_range 1 30) bool))
        (fun entries ->
          let entries =
            List.dedup_and_sort entries ~compare:(fun (a, _) (b, _) ->
                Int.compare a b)
          in
          let candidates =
            List.map entries ~f:(fun (n, _) -> B.Recorded (F.commit n))
          in
          let evidence =
            List.map entries ~f:(fun (n, reachable) -> (F.commit n, reachable))
          in
          let expected =
            match List.find entries ~f:snd with
            | None -> B.Plain
            | Some (n, _) -> B.Recorded (F.commit n)
          in
          B.equal_boundary_choice
            (B.choose_boundary ~candidates ~evidence)
            (B.Chosen expected));
      property
        "missing primary ancestry evidence must be probed before fallback"
        G.(pair (int_range 1 30) bool)
        (fun (n, older_reachable) ->
          let primary = F.commit n and older = F.commit (n + 100) in
          B.equal_boundary_choice
            (B.choose_boundary
               ~candidates:[ Recorded primary; Recorded older ]
               ~evidence:[ (older, older_reachable) ])
            (B.Probe primary));
      property "owner receipt order survives duplicate targets and restart"
        G.(list_size (int_range 0 25) (pair (int_range 1 15) bool))
        (fun turns ->
          let initial =
            F.step B.empty (B.Materialized (B.New_branch (F.commit 1)))
          in
          let state, _, expected =
            List.foldi turns
              ~init:(initial, F.commit 100, [ F.commit 1 ])
              ~f:(fun i (state, source, expected) (n, noop) ->
                let target = F.commit n in
                let candidate = if noop then source else F.commit (1000 + i) in
                let state =
                  F.integrate ~name:(Int.to_string i) ~source ~target ~candidate
                    ~boundary:(Recorded (List.hd_exn expected))
                    ~noop state
                  |> F.restore
                in
                let expected =
                  target
                  :: List.filter expected ~f:(fun revision ->
                      not (B.Commit.equal revision target))
                in
                let probe =
                  F.request ~name:("inspect-" ^ Int.to_string i) state
                in
                assert (
                  same (F.boundaries probe)
                    (List.map expected ~f:(fun r -> B.Recorded r)));
                (state, candidate, expected))
          in
          let requested = F.request ~name:"final" state in
          same (F.boundaries requested)
            (List.map expected ~f:(fun r -> B.Recorded r)));
      property "adoption does not claim a replay boundary" G.bool
        (fun adopted ->
          let revision = F.commit 1 in
          let materialized =
            if adopted then B.Adopted_branch revision else B.New_branch revision
          in
          let state =
            F.step B.empty (B.Materialized materialized)
            |> F.restore |> F.request ~name:"first"
          in
          same (F.boundaries state)
            (if adopted then [] else [ B.Recorded revision ]));
      property "failed integration cannot replace recorded history"
        G.(pair (int_range 0 2) bool)
        (fun (failure, restart) ->
          let initial =
            F.step B.empty (B.Materialized (B.New_branch (F.commit 1)))
          in
          let settled =
            F.integrate ~name:"first" ~source:(F.commit 2) ~target:(F.commit 3)
              ~candidate:(F.commit 4)
              ~boundary:(Recorded (F.commit 1))
              ~noop:false initial
          in
          let attempted =
            F.request ~name:"second" settled
            |> F.observe ~source:(F.commit 4) ~target:(F.commit 5)
                 ~boundary:(Recorded (F.commit 3))
                 ~noop:false
            |> fun t -> F.reply t B.Pinned
          in
          let result =
            match failure with
            | 0 ->
                B.Conflict
                  { head = F.commit 4; sequencer = "rebase"; conflicts = 1 }
            | 1 -> B.Retryable { reason = "probe_failed"; retry_after = None }
            | _ -> B.Permanent "refused"
          in
          let failed = F.reply attempted result in
          let failed = if restart then F.restore failed else failed in
          List.equal B.equal_integration_receipt (B.integrations settled)
            (B.integrations failed)
          && Option.equal B.equal_materialization
               (B.materialization settled)
               (B.materialization failed)
          && same (F.boundaries failed) [ Recorded (F.commit 3) ]);
    ]
