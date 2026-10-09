(* @archlint.module shell
   @archlint.domain prune-decision *)

open Base

type outcome = Pruned | Retained of Branch_reconcile.Commit.t list

let probe (io : Branch_reconcile_executor.io) args =
  let code, out, err = io.git args in
  if code = 0 then Ok out
  else
    Error
      (Printf.sprintf "recovery ref probe failed (%d): %s" code
         (String.strip err))

let repository_id io =
  Result.bind
    (probe io [ "rev-parse"; "--path-format=absolute"; "--git-common-dir" ])
    ~f:(fun path ->
      try Ok (Unix.realpath (String.strip path))
      with exn -> Error (Exn.to_string exn))

let inventory io extra =
  Result.bind
    (probe io
       ([
          "for-each-ref";
          "--format=%(refname)%00%(objectname)%00%(symref)%00%(objecttype)";
        ]
       @ extra
       @ [ "refs/onton/reconcile/" ]))
    ~f:Prune_decision.parse_recovery_refs

let reclaim_captured ~io ~project ~protected_projects ~required captured =
  let ( let* ) result f = Result.bind result ~f in
  let ( let+ ) result f = Result.map result ~f in
  let required =
    List.dedup_and_sort required ~compare:Branch_reconcile.Commit.compare
  in
  let* reachability =
    List.fold_result required ~init:[] ~f:(fun proofs revision ->
        let+ refs =
          inventory io
            [ "--contains=" ^ Branch_reconcile.Commit.to_string revision ]
        in
        (revision, refs) :: proofs)
  in
  let* plan =
    Prune_decision.plan_reclamation ~project ~protected_projects
      ~inventory:captured ~required ~reachability
  in
  match plan with
  | Prune_decision.Retain revisions -> Ok (Retained revisions)
  | Prune_decision.Reclaim refs -> (
      let* () =
        List.fold_result refs ~init:() ~f:(fun () reference ->
            let+ _ =
              probe io
                [
                  "update-ref";
                  "--no-deref";
                  "-d";
                  reference.Prune_decision.name;
                  Branch_reconcile.Commit.to_string reference.revision;
                ]
            in
            ())
      in
      let* remaining = inventory io [] in
      let* final =
        Prune_decision.plan_reclamation ~project ~protected_projects
          ~inventory:remaining ~required:[] ~reachability:[]
      in
      match final with
      | Prune_decision.Reclaim [] -> Ok Pruned
      | Prune_decision.Reclaim (_ :: _) | Prune_decision.Retain _ ->
          Error "recovery_ref_namespace_changed_during_prune")

let reclaim ~io ~project ~protected_projects ~required =
  Result.bind (inventory io []) ~f:(fun captured ->
      Result.bind
        (Prune_decision.plan_reclamation ~project ~protected_projects
           ~inventory:captured ~required:[] ~reachability:[]) ~f:(function
        | Prune_decision.Reclaim [] -> Ok Pruned
        | Prune_decision.Reclaim (_ :: _) ->
            reclaim_captured ~io ~project ~protected_projects ~required captured
        | Prune_decision.Retain revisions -> Ok (Retained revisions)))
