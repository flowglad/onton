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

let classify patches agents =
  Prune_decision.classify_snapshot ~patches ~agents:(agents_map agents)

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
      ~name:"prune non-empty is All_merged iff every patch agent is merged"
      ~count:1000 gen_patches_and_flags (fun (patches, flags) ->
        let agents =
          List.map2_exn patches flags ~f:(fun p merged ->
              agent_for_patch ~merged p)
        in
        let status = classify patches agents in
        let expected_all_merged =
          (not (List.is_empty patches)) && List.for_all flags ~f:(fun x -> x)
        in
        Bool.equal
          (Prune_decision.equal_project_status status Prune_decision.All_merged)
          expected_all_merged)
  in
  let prop_missing_agent_not_merged =
    Test.make
      ~name:"prune missing gameplan agent is Not_merged for non-empty patches"
      ~count:500
      Gen.(map (fun n -> List.init n ~f:patch) (int_range 1 20))
      (fun patches ->
        let agents =
          match patches with
          | [] -> []
          | _ :: rest -> List.map rest ~f:(agent_for_patch ~merged:true)
        in
        Prune_decision.equal_project_status (classify patches agents)
          Prune_decision.Not_merged)
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
          if merged then Prune_decision.All_merged
          else Prune_decision.Not_merged
        in
        Prune_decision.equal_project_status
          (classify patches [ agent_for_patch ~merged p ])
          expected)
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
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode
