open Base
open Onton_core
open Onton_core.Types
module Scheduler = Patch_controller_core

let modes = [ Scheduler.Naive; Scheduler.Greedy ]
let patch_id i = Patch_id.of_string (Printf.sprintf "p%d" i)
let branch i = Branch.of_string (Printf.sprintf "branch-%d" i)

let patch ?(dependencies = []) i =
  {
    Patch.id = patch_id i;
    title = "test";
    description = "test";
    branch = branch i;
    dependencies;
    spec = "";
    acceptance_criteria = [];
    changes = [];
    files = [];
    classification = "";
    test_stubs_introduced = [];
    test_stubs_implemented = [];
    complexity = None;
    precedents = [];
  }

let chain_graph =
  let dep = patch 1 in
  let mid = patch ~dependencies:[ dep.Patch.id ] 2 in
  let child = patch ~dependencies:[ mid.Patch.id ] 3 in
  Graph.of_patches [ dep; mid; child ]

let chain_dep = patch_id 1
let chain_mid = patch_id 2
let chain_child = patch_id 3
let set_of_ids ids = Set.of_list (module Patch_id) ids
let in_set set id = Set.mem set id

let deps_satisfied graph patch_id ~merged ~prs ~start_mode =
  Scheduler.start_deps_satisfied graph patch_id
    ~has_merged:(in_set (set_of_ids merged))
    ~has_pr:(in_set (set_of_ids prs))
    ~start_mode

let can_start graph patch_id ~merged ~prs ~start_mode =
  (not (List.mem merged patch_id ~equal:Patch_id.equal))
  && (not (List.mem prs patch_id ~equal:Patch_id.equal))
  && deps_satisfied graph patch_id ~merged ~prs ~start_mode

let prop_start_mode_parse_total =
  QCheck2.Test.make ~name:"start_mode_of_string is total" ~count:1_000
    QCheck2.Gen.(string_size ~gen:printable (int_range 0 80))
    (fun s ->
      try
        let _ = Scheduler.start_mode_of_string s in
        true
      with _ -> false)

let prop_start_mode_round_trip =
  QCheck2.Test.make ~name:"start_mode string round-trips"
    (QCheck2.Gen.oneof_list modes) (fun mode ->
      match
        Scheduler.start_mode_of_string (Scheduler.start_mode_to_string mode)
      with
      | Ok parsed -> Scheduler.equal_start_mode parsed mode
      | Error _ -> false)

let prop_chain_naive_blocks_mid_until_dep_merged =
  QCheck2.Test.make
    ~name:"chain: naive blocks mid until direct dependency merges"
    QCheck2.Gen.(pair bool bool)
    (fun (dep_has_pr, dep_merged) ->
      let prs = if dep_has_pr then [ chain_dep ] else [] in
      let merged = if dep_merged then [ chain_dep ] else [] in
      Bool.equal
        (deps_satisfied chain_graph chain_mid ~merged ~prs
           ~start_mode:Scheduler.Naive)
        dep_merged)

let prop_chain_greedy_allows_mid_when_dep_has_pr_or_merged =
  QCheck2.Test.make
    ~name:"chain: greedy allows mid when sole dependency has PR or merged"
    QCheck2.Gen.(pair bool bool)
    (fun (dep_has_pr, dep_merged) ->
      let prs = if dep_has_pr then [ chain_dep ] else [] in
      let merged = if dep_merged then [ chain_dep ] else [] in
      Bool.equal
        (deps_satisfied chain_graph chain_mid ~merged ~prs
           ~start_mode:Scheduler.Greedy)
        (dep_has_pr || dep_merged))

let prop_chain_naive_blocks_child_until_mid_merged =
  QCheck2.Test.make ~name:"chain: naive blocks child until each level is merged"
    QCheck2.Gen.(pair bool bool)
    (fun (mid_has_pr, mid_merged) ->
      let prs = if mid_has_pr then [ chain_mid ] else [] in
      let merged = chain_dep :: (if mid_merged then [ chain_mid ] else []) in
      Bool.equal
        (deps_satisfied chain_graph chain_child ~merged ~prs
           ~start_mode:Scheduler.Naive)
        mid_merged)

let prop_chain_greedy_allows_child_when_mid_has_pr =
  QCheck2.Test.make
    ~name:"chain: greedy allows child when sole dependency has PR"
    QCheck2.Gen.(pair bool bool)
    (fun (mid_has_pr, mid_merged) ->
      let prs = if mid_has_pr then [ chain_mid ] else [] in
      let merged = chain_dep :: (if mid_merged then [ chain_mid ] else []) in
      Bool.equal
        (deps_satisfied chain_graph chain_child ~merged ~prs
           ~start_mode:Scheduler.Greedy)
        (mid_has_pr || mid_merged))

type generated_graph = {
  graph : Graph.t;
  ids : Patch_id.t list;
  merged : Patch_id.t list;
  prs : Patch_id.t list;
}

let gen_generated_graph =
  let open QCheck2.Gen in
  let* n = int_range 1 7 in
  let indices = List.init n ~f:(fun i -> i + 1) in
  let rec gen_deps i =
    if i <= n then
      let* flags = list_size (return (i - 1)) bool in
      let deps =
        List.filter_mapi flags ~f:(fun j selected ->
            if selected then Some (patch_id (j + 1)) else None)
      in
      let* rest = gen_deps (i + 1) in
      return ((i, deps) :: rest)
    else return []
  in
  let* dep_specs = gen_deps 1 in
  let patches =
    List.map dep_specs ~f:(fun (i, dependencies) -> patch ~dependencies i)
  in
  let ids = List.map indices ~f:patch_id in
  let* merged_flags = list_size (return n) bool in
  let* pr_flags = list_size (return n) bool in
  let merged =
    List.filter_mapi merged_flags ~f:(fun i selected ->
        if selected then Some (patch_id (i + 1)) else None)
  in
  let prs =
    List.filter_mapi pr_flags ~f:(fun i selected ->
        if selected then Some (patch_id (i + 1)) else None)
  in
  return { graph = Graph.of_patches patches; ids; merged; prs }

let prop_naive_startable_subset_of_greedy =
  QCheck2.Test.make ~name:"naive-startable set is subset of greedy-startable"
    ~count:500 gen_generated_graph (fun { graph; ids; merged; prs } ->
      List.for_all ids ~f:(fun id ->
          (not (can_start graph id ~merged ~prs ~start_mode:Scheduler.Naive))
          || can_start graph id ~merged ~prs ~start_mode:Scheduler.Greedy))

let prop_greedy_is_strictly_stronger_witness =
  QCheck2.Test.make ~name:"greedy has a strict startability witness"
    QCheck2.Gen.unit (fun () ->
      deps_satisfied chain_graph chain_mid ~merged:[] ~prs:[ chain_dep ]
        ~start_mode:Scheduler.Greedy
      && not
           (deps_satisfied chain_graph chain_mid ~merged:[] ~prs:[ chain_dep ]
              ~start_mode:Scheduler.Naive))

let () =
  [
    prop_start_mode_parse_total;
    prop_start_mode_round_trip;
    prop_chain_naive_blocks_mid_until_dep_merged;
    prop_chain_greedy_allows_mid_when_dep_has_pr_or_merged;
    prop_chain_naive_blocks_child_until_mid_merged;
    prop_chain_greedy_allows_child_when_mid_has_pr;
    prop_naive_startable_subset_of_greedy;
    prop_greedy_is_strictly_stronger_witness;
  ]
  |> List.iter ~f:(fun test -> QCheck2.Test.check_exn test)
