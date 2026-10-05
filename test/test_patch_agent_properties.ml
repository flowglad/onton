(* @archlint.module test
   @archlint.domain patch-agent *)
open Base
open Onton_core
open Types

let agent id =
  Patch_agent.create ~branch:(Branch.of_string "branch") (Patch_id.of_string id)

let tests =
  [
    QCheck2.Test.make
      ~name:"publication settles only its captured refresh across interleavings"
      ~count:500
      QCheck2.Gen.(list (int_range 0 3))
      (fun operations ->
        let initial = agent "root" in
        let _, _, _, _, _, valid =
          List.fold operations ~init:(initial, initial, 0, 0, false, true)
            ~f:(fun (a, publication, version, captured, pending, valid) op ->
              let a, publication, version, captured, pending =
                match op with
                | 0 ->
                    ( Patch_agent.request_pr_body_refresh a,
                      publication,
                      version + 1,
                      captured,
                      true )
                | 1 -> (a, a, version, version, pending)
                | 2 ->
                    ( Patch_agent.acknowledge_pr_body_refresh a ~publication,
                      publication,
                      version,
                      captured,
                      pending && not (Int.equal version captured) )
                | _ ->
                    ( Patch_agent.set_pr_body_delivered a true,
                      publication,
                      version,
                      captured,
                      pending )
              in
              ( a,
                publication,
                version,
                captured,
                pending,
                valid
                && Bool.equal a.Patch_agent.pr_body_refresh.Patch_agent.pending
                     pending
                && Int.equal a.Patch_agent.pr_body_refresh.Patch_agent.version
                     version ))
        in
        valid);
    QCheck2.Test.make ~name:"another patch cannot settle refresh" ~count:100
      QCheck2.Gen.bool (fun delivered ->
        let a = Patch_agent.request_pr_body_refresh (agent "root") in
        let publication =
          agent "child" |> Patch_agent.request_pr_body_refresh |> fun a ->
          Patch_agent.set_pr_body_delivered a delivered
        in
        Patch_agent.equal a
          (Patch_agent.acknowledge_pr_body_refresh a ~publication));
  ]

let () = QCheck_base_runner.run_tests_main tests
