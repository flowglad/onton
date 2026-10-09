(* @archlint.module test
   @archlint.domain branch-reconcile *)
open Base
open Onton_core
module B = Branch_reconcile
module F = Onton_core_test_support.Replay_fixture

(* These histories model commit identity, not patch equivalence. Squash/rebase
   merge tips deliberately have different identities from dependency commits. *)
let () =
  let start = F.commit 103 and source = F.commit 104 in
  let materialized = F.step B.empty (B.Materialized (B.New_branch start)) in
  (* A four-deep stack has branched from its third dependency. Squashing each
     dependency or force-pushing its ref must not replace that starting boundary. *)
  List.iter [ 201; 202; 203 ] ~f:(fun target ->
      let request = F.request ~name:(Int.to_string target) materialized in
      let boundary =
        F.selected
          ~reachable:[ F.commit 100; F.commit 101; F.commit 102; start; source ]
          (F.boundaries request)
      in
      assert (B.equal_boundary boundary (Recorded start));
      let observed =
        F.observe ~source ~target:(F.commit target) ~boundary ~noop:false
          request
      in
      let restarted = F.restore observed in
      assert (
        List.equal B.equal_boundary (F.boundaries restarted) [ Recorded start ]));
  (* A rewrite captured target 201 before the base advanced again to 202. Its
     receipt must retain 201, then the next request can observe the newer base. *)
  let pending =
    F.request ~name:"first-rewrite" materialized
    |> F.observe ~source ~target:(F.commit 201) ~boundary:(Recorded start)
         ~noop:false
  in
  let advanced_base =
    F.observation ~source ~target:(F.commit 202) ~boundary:(Recorded start)
      ~noop:false
  in
  let refreshed = F.step pending (B.Refresh advanced_base) in
  assert (B.equal pending refreshed);
  let completed =
    F.complete ~candidate:(F.commit 301) ~noop:false (F.restore pending)
  in
  let receipt = List.hd_exn (B.integrations completed) in
  assert (B.Commit.equal receipt.capture.target_revision (F.commit 201));
  let next = F.request ~name:"retarget" ~base:"new-main" completed in
  assert (
    List.equal B.equal_boundary (F.boundaries next)
      [ Recorded (F.commit 201); Recorded start ]);
  assert (
    B.equal_boundary
      (F.selected ~reachable:[ F.commit 201; F.commit 301 ] (F.boundaries next))
      (Recorded (F.commit 201)));
  (* Resetting the patch to its original history makes the newer receipt
     unreachable, but the materialization boundary still identifies patch work. *)
  assert (
    B.equal_boundary
      (F.selected ~reachable:[ start; source ] (F.boundaries next))
      (Recorded start));
  assert (
    B.equal_boundary
      (F.selected ~reachable:[ F.commit 999 ] (F.boundaries next))
      Plain);
  (* After an external merge creates 302 containing target 202, a no-op is evidence too: refresh the target receipt without inventing a
     rewrite, preserving prior boundaries for a later checkout reset. *)
  let noop =
    F.integrate ~name:"noop" ~source:(F.commit 302) ~target:(F.commit 202)
      ~candidate:(F.commit 302)
      ~boundary:(Recorded (F.commit 201))
      ~noop:true completed
  in
  let receipt = List.hd_exn (B.integrations noop) in
  assert (B.Commit.equal receipt.capture.target_revision (F.commit 202));
  assert (B.Commit.equal receipt.integrated_revision (F.commit 302));
  assert (B.equal_integration_evidence receipt.evidence Recovered_completion);
  let restarted = F.restore noop |> F.request ~name:"after-noop" in
  assert (
    List.equal B.equal_boundary (F.boundaries restarted)
      [ Recorded (F.commit 202); Recorded (F.commit 201); Recorded start ]);
  (* Missing ancestry observation is not negative ancestry evidence. *)
  assert (
    B.equal_boundary_choice
      (B.choose_boundary ~candidates:(F.boundaries restarted) ~evidence:[])
      (Probe (F.commit 202)));
  Stdlib.print_endline "owner replay boundary history scenarios: OK"
