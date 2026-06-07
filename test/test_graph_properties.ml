(* @archlint.module test
   @archlint.domain graph *)

open Onton_core

let graph_add_patch_is_idempotent =
  QCheck2.Test.make ~name:"add_patch is idempotent for ad-hoc nodes" ~count:200
    QCheck2.Gen.string_small (fun raw ->
      let pid = Types.Patch_id.of_string raw in
      let graph =
        Graph.add_patch (Graph.add_patch (Graph.of_patches []) pid) pid
      in
      List.length (Graph.all_patch_ids graph) = 1
      && Graph.depends_on graph pid ~dep:pid = false)

let graph_public_surface_is_linked =
  QCheck2.Test.make ~name:"graph public surface is linked" QCheck2.Gen.unit
    (fun () ->
      ignore Graph.add_dependency;
      ignore Graph.add_patch_with_deps;
      ignore Graph.dependents;
      ignore Graph.deps;
      ignore Graph.deps_satisfied;
      ignore Graph.initial_base;
      ignore Graph.open_pr_deps;
      ignore Graph.remove_patch;
      ignore Graph.sole_open_dep;
      ignore Graph.transitive_ancestors;
      true)

let () =
  QCheck2.Test.check_exn graph_add_patch_is_idempotent;
  QCheck2.Test.check_exn graph_public_surface_is_linked
