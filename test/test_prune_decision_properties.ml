(* @archlint.module test
   @archlint.domain prune-decision *)

open Base
open Onton_core
open Onton_core.Types

let patch i =
  Patch.
    {
      id = Patch_id.of_string (Printf.sprintf "patch-%d" i);
      title = Printf.sprintf "Patch %d" i;
      description = "";
      branch = Branch.of_string (Printf.sprintf "branch-%d" i);
      dependencies = [];
      spec = "";
      acceptance_criteria = [];
      files = [];
      classification = "";
      changes = [];
      test_stubs_introduced = [];
      test_stubs_implemented = [];
      complexity = None;
      precedents = [];
      required_context = [];
    }

let agent_for_patch ?(merged = false) (p : Patch.t) =
  let agent = Patch_agent.create ~branch:p.Patch.branch p.Patch.id in
  if merged then Patch_agent.mark_merged agent else agent

let agents_map agents =
  List.fold agents
    ~init:(Map.empty (module Patch_id))
    ~f:(fun acc agent ->
      Map.set acc ~key:agent.Patch_agent.patch_id ~data:agent)

let gen_unique_patches =
  QCheck2.Gen.(map (fun n -> List.init n ~f:patch) (int_range 0 20))

let gen_patches_and_flags =
  QCheck2.Gen.(
    gen_unique_patches >>= fun patches ->
    list_size (return (List.length patches)) bool >>= fun merged_flags ->
    return (patches, merged_flags))

let gen_patches_flags_and_extra =
  QCheck2.Gen.(
    gen_patches_and_flags >>= fun (patches, flags) ->
    int_range 0 10 >>= fun extra_count ->
    return (patches, flags, List.init extra_count ~f:(fun i -> patch (100 + i))))

let classify ?(closed_patch_ids = []) patches agents =
  Prune_decision.classify_snapshot ~patches ~agents:(agents_map agents)
    ~closed_patch_ids

module B = Branch_reconcile
module P = Prune_decision

let revision n =
  match B.Commit.make (Printf.sprintf "%040x" n) with
  | Some revision -> revision
  | None -> failwith "invalid fixture revision"

let ref_line project n =
  B.recovery_prefix ~project ~branch:"patch"
  ^ "/" ^ Int.to_string n ^ "\000"
  ^ B.Commit.to_string (revision n)
  ^ "\000\000commit\n"

let refs lines =
  match P.parse_recovery_refs lines with
  | Ok refs -> refs
  | Error reason -> failwith reason

let recovery_tests =
  let open QCheck2 in
  [
    Test.make ~name:"prune ref inventory parser is total" ~count:1000 Gen.string
      (fun text ->
        try
          ignore (P.parse_recovery_refs text);
          true
        with _ -> false);
    Test.make
      ~name:
        "prune checkpoint dependencies are deduplicated across surviving agents"
      ~count:200
      Gen.(int_range 0 20)
      (fun count ->
        try
          let agents =
            List.init count ~f:(fun i ->
                fst
                  (Patch_agent.reconcile_branch
                     (agent_for_patch (patch i))
                     (B.Materialized (B.New_branch (revision 1)))))
            |> agents_map
          in
          List.equal B.Commit.equal
            (P.recovery_dependencies agents)
            (if count = 0 then [] else [ revision 1 ])
        with _ -> false);
    Test.make ~name:"prune inventory retains migrated legacy revisions"
      ~count:200 Gen.bool (fun merged ->
        try
          let anchors =
            `List
              [ `Assoc [ ("sha", `String (B.Commit.to_string (revision 2))) ] ]
          in
          let agent =
            Patch_agent.migrate_legacy_branch_state ~published:false
              ~main_branch:(Branch.of_string "main") ~owner_missing:false
              ~anchors
              (agent_for_patch ~merged (patch 1))
          in
          List.mem
            (P.recovery_dependencies (agents_map [ agent ]))
            (revision 2) ~equal:B.Commit.equal
        with _ -> false);
    Test.make ~name:"prune malformed symbolic and duplicate refs fail closed"
      ~count:200 Gen.string (fun suffix ->
        try
          let line = ref_line "project" 1 in
          let symbolic =
            B.recovery_prefix ~project:"project" ~branch:"patch"
            ^ "/alias\000"
            ^ B.Commit.to_string (revision 1)
            ^ "\000refs/heads/main\000commit\n"
          in
          Result.is_error (P.parse_recovery_refs (line ^ line))
          && Result.is_error (P.parse_recovery_refs symbolic)
          && Result.is_error
               (P.parse_recovery_refs ("outside/" ^ suffix ^ "\000bad\000\n"))
          && Result.equal
               (List.equal P.equal_recovery_ref)
               String.equal (P.parse_recovery_refs "") (Ok [])
        with _ -> false);
    Test.make
      ~name:"prune inventory accepts both Git hash widths only for commits"
      ~count:200 Gen.bool (fun wide ->
        try
          let line =
            B.recovery_prefix ~project:"a" ~branch:"patch"
            ^ "/tip\000"
            ^ String.make (if wide then 64 else 40) 'a'
            ^ "\000\000"
          in
          Result.is_ok (P.parse_recovery_refs (line ^ "commit\n"))
          && Result.is_error (P.parse_recovery_refs (line ^ "blob\n"))
        with _ -> false);
    Test.make ~name:"prune exact namespace and dependency lifetimes" ~count:500
      Gen.(pair bool bool)
      (fun (needed, independent) ->
        try
          let owned = refs (ref_line "a" 1) in
          let foreign = refs (ref_line "ab" 2) in
          let inventory = owned @ foreign in
          let required = if needed then [ revision 1 ] else [] in
          let reachability =
            [ (revision 1, owned @ if independent then foreign else []) ]
          in
          let result =
            P.plan_reclamation ~protected_projects:[ "ab"; "b" ] ~project:"a"
              ~inventory ~required ~reachability
          in
          let expected =
            if needed && not independent then P.Retain [ revision 1 ]
            else P.Reclaim owned
          in
          Result.equal P.equal_reclamation String.equal result (Ok expected)
          && Result.equal P.equal_reclamation String.equal result
               (P.plan_reclamation ~protected_projects:[ "ab"; "b" ]
                  ~project:"a" ~inventory:(List.rev inventory)
                  ~required:(required @ required) ~reachability)
        with _ -> false);
    Test.make
      ~name:"prune missing ambiguous or stale reachability never deletes"
      ~count:300 Gen.bool (fun stale ->
        try
          let inventory = refs (ref_line "a" 1) in
          let wrong = refs (ref_line "a" 2) in
          let reachability = if stale then [ (revision 1, wrong) ] else [] in
          Result.is_error
            (P.plan_reclamation ~protected_projects:[ "ab"; "b" ] ~project:"a"
               ~inventory
               ~required:[ revision 1 ]
               ~reachability)
          && Result.is_error
               (P.plan_reclamation ~protected_projects:[ "ab"; "b" ]
                  ~project:"a" ~inventory
                  ~required:[ revision 1 ]
                  ~reachability:
                    [ (revision 1, inventory); (revision 1, inventory) ])
        with _ -> false);
    Test.make
      ~name:
        "prune retention remains correct across dependency and anchor \
         interleavings"
      ~count:500
      Gen.(list_size (int_range 0 100) (pair bool bool))
      (fun changes ->
        try
          let owned = refs (ref_line "a" 1)
          and foreign = refs (ref_line "b" 2) in
          List.for_all changes ~f:(fun (needed, anchored) ->
              let required = if needed then [ revision 1 ] else [] in
              let inventory = owned @ if anchored then foreign else [] in
              let result =
                P.plan_reclamation ~protected_projects:[ "ab"; "b" ]
                  ~project:"a" ~inventory ~required
                  ~reachability:[ (revision 1, inventory) ]
              in
              Result.equal P.equal_reclamation String.equal result
                (Ok
                   (if needed && not anchored then P.Retain [ revision 1 ]
                    else P.Reclaim owned)))
        with _ -> false);
    Test.make
      ~name:
        "prune canonical directory containment respects component boundaries"
      ~count:500 Gen.string (fun suffix ->
        let dir = "/tmp/project" in
        P.repository_within_project ~project_dir:dir
          ~git_dir:(dir ^ "/repo/" ^ suffix)
        && (not
              (P.repository_within_project ~project_dir:dir
                 ~git_dir:(dir ^ "-other/" ^ suffix)))
        && P.repository_within_project ~project_dir:dir ~git_dir:dir);
    Test.make
      ~name:"prune merged or closed patch retains unfinished reconciliation"
      ~count:200 Gen.bool (fun merged ->
        try
          let p = patch 1 in
          let agent = agent_for_patch ~merged p in
          let agent, _ =
            Patch_agent.reconcile_branch agent
              (B.Request
                 {
                   base = "main";
                   policy = B.Rewrite;
                   purpose = B.Reconcile_base;
                 })
          in
          P.equal_project_status
            (classify ~closed_patch_ids:[ p.Patch.id ] [ p ] [ agent ])
            P.Not_terminal
        with _ -> false);
  ]

let () =
  let open QCheck2 in
  let prop_total =
    Test.make ~name:"prune classify_snapshot is total" ~count:1000
      gen_patches_flags_and_extra (fun (patches, flags, extra_patches) ->
        try
          let agents =
            List.map2_exn patches flags ~f:(fun p merged ->
                agent_for_patch ~merged p)
            @ List.map extra_patches ~f:(agent_for_patch ~merged:true)
          in
          ignore (classify patches agents : Prune_decision.project_status);
          true
        with _ -> false)
  in
  let prop_empty_no_patches =
    Test.make ~name:"prune empty gameplan is No_patches" ~count:200
      gen_patches_flags_and_extra (fun (_, _, extra_patches) ->
        let agents = List.map extra_patches ~f:(agent_for_patch ~merged:true) in
        Prune_decision.equal_project_status (classify [] agents)
          Prune_decision.No_patches)
  in
  let prop_all_merged_iff_all_present_merged =
    Test.make
      ~name:"prune non-empty is All_terminal iff every patch agent is merged"
      ~count:1000 gen_patches_and_flags (fun (patches, flags) ->
        let agents =
          List.map2_exn patches flags ~f:(fun p merged ->
              agent_for_patch ~merged p)
        in
        let status = classify patches agents in
        let expected_all_terminal =
          (not (List.is_empty patches)) && List.for_all flags ~f:(fun x -> x)
        in
        Bool.equal
          (Prune_decision.equal_project_status status
             Prune_decision.All_terminal)
          expected_all_terminal)
  in
  let prop_missing_agent_not_merged =
    Test.make
      ~name:"prune missing gameplan agent is Not_terminal for non-empty patches"
      ~count:500
      Gen.(map (fun n -> List.init n ~f:patch) (int_range 1 20))
      (fun patches ->
        let agents =
          match patches with
          | [] -> []
          | _ :: rest -> List.map rest ~f:(agent_for_patch ~merged:true)
        in
        Prune_decision.equal_project_status (classify patches agents)
          Prune_decision.Not_terminal)
  in
  let prop_irrelevant_agents_do_not_change =
    Test.make ~name:"prune ignores agents outside the gameplan" ~count:1000
      gen_patches_flags_and_extra (fun (patches, flags, extra_patches) ->
        let base_agents =
          List.map2_exn patches flags ~f:(fun p merged ->
              agent_for_patch ~merged p)
        in
        let extra_agents =
          List.map extra_patches ~f:(fun p -> agent_for_patch ~merged:false p)
        in
        Prune_decision.equal_project_status
          (classify patches base_agents)
          (classify patches (base_agents @ extra_agents)))
  in
  let prop_patch_order_invariant =
    Test.make ~name:"prune classification is invariant under patch order"
      ~count:1000 gen_patches_and_flags (fun (patches, flags) ->
        let agents =
          List.map2_exn patches flags ~f:(fun p merged ->
              agent_for_patch ~merged p)
        in
        Prune_decision.equal_project_status (classify patches agents)
          (classify (List.rev patches) agents))
  in
  let prop_duplicate_patch_ids_follow_same_agent =
    Test.make
      ~name:"prune duplicate patch ids are classified by their shared agent"
      ~count:500 Gen.bool (fun merged ->
        let p = patch 1 in
        let patches = [ p; { p with title = "duplicate id" } ] in
        let expected =
          if merged then Prune_decision.All_terminal
          else Prune_decision.Not_terminal
        in
        Prune_decision.equal_project_status
          (classify patches [ agent_for_patch ~merged p ])
          expected)
  in
  let prop_closed_prs_are_terminal =
    Test.make ~name:"prune closed PRs are terminal alongside merged PRs"
      ~count:1000 gen_patches_and_flags (fun (patches, merged_flags) ->
        try
          let agents =
            List.map2_exn patches merged_flags ~f:(fun p merged ->
                agent_for_patch ~merged p)
          in
          let closed_patch_ids =
            List.filter_map (List.zip_exn patches merged_flags)
              ~f:(fun (p, merged) -> if merged then None else Some p.Patch.id)
          in
          let expected =
            if List.is_empty patches then Prune_decision.No_patches
            else Prune_decision.All_terminal
          in
          Prune_decision.equal_project_status
            (classify ~closed_patch_ids patches agents)
            expected
        with _ -> false)
  in
  let prop_closed_without_agent_is_not_terminal =
    Test.make
      ~name:"prune closed PR id without a corresponding agent is not terminal"
      ~count:200 Gen.unit (fun () ->
        let p = patch 1 in
        Prune_decision.equal_project_status
          (classify ~closed_patch_ids:[ p.Patch.id ] [ p ] [])
          Prune_decision.Not_terminal)
  in
  let suite =
    [
      prop_total;
      prop_empty_no_patches;
      prop_all_merged_iff_all_present_merged;
      prop_missing_agent_not_merged;
      prop_irrelevant_agents_do_not_change;
      prop_patch_order_invariant;
      prop_duplicate_patch_ids_follow_same_agent;
      prop_closed_prs_are_terminal;
      prop_closed_without_agent_is_not_terminal;
    ]
  in
  let errcode =
    QCheck_base_runner.run_tests ~verbose:true (suite @ recovery_tests)
  in
  if errcode <> 0 then Stdlib.exit errcode
