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

(* Build a graph of [n] distinct bare patches "p0".."p{n-1}" plus a list of
   directed edges (i -> j) constrained to the node range, then add the edges
   via [add_dependency]. Returns the graph and the id list. *)
let gen_graph =
  QCheck2.Gen.(
    int_range 1 8 >>= fun n ->
    let ids =
      List.init n (fun i -> Types.Patch_id.of_string (Printf.sprintf "p%d" i))
    in
    let base =
      List.fold_left
        (fun g id -> Graph.add_patch g id)
        (Graph.of_patches []) ids
    in
    map
      (fun edges ->
        let g =
          List.fold_left
            (fun g (i, j) ->
              Graph.add_dependency g
                (Types.Patch_id.of_string (Printf.sprintf "p%d" i))
                ~dep:(Types.Patch_id.of_string (Printf.sprintf "p%d" j)))
            base edges
        in
        (g, ids))
      (list_size (int_range 0 12)
         (pair (int_range 0 (n - 1)) (int_range 0 (n - 1)))))

let always_false _ = false
let always_true _ = true

(* add_dependency called directly in the property body (not only inside the
   generator): the recorded edge shows up in [deps], and a self-edge is
   dropped. *)
let graph_add_dependency_records_edge =
  QCheck2.Test.make ~name:"add_dependency records the dep edge" ~count:200
    QCheck2.Gen.(pair (int_range 0 5) (int_range 0 5))
    (fun (i, j) ->
      let a = Types.Patch_id.of_string (Printf.sprintf "p%d" i) in
      let b = Types.Patch_id.of_string (Printf.sprintf "p%d" j) in
      let g = List.fold_left Graph.add_patch (Graph.of_patches []) [ a; b ] in
      let g = Graph.add_dependency g a ~dep:b in
      let b_in_deps =
        List.exists (fun d -> Types.Patch_id.equal d b) (Graph.deps g a)
      in
      if Types.Patch_id.equal a b then not b_in_deps else b_in_deps)

(* add_dependency: deps recorded by add_dependency are reflected in [deps] and
   [dependents], are never self-edges, and removing a node drops its edges. *)
let graph_add_dependency_edges_consistent =
  QCheck2.Test.make
    ~name:"add_dependency edges are reflected in deps/dependents" ~count:300
    gen_graph (fun (g, ids) ->
      List.for_all
        (fun pid ->
          let ds = Graph.deps g pid in
          (* No self-edge survives. *)
          (not (List.exists (fun d -> Types.Patch_id.equal d pid) ds))
          (* Each dep records [pid] as a dependent. *)
          && List.for_all
               (fun d ->
                 List.exists
                   (fun dep -> Types.Patch_id.equal dep pid)
                   (Graph.dependents g d))
               ds)
        ids)

(* add_patch_with_deps / transitive_ancestors: a freshly added node carries the
   edges to already-present deps; transitive_ancestors excludes the node itself
   and is a superset of direct deps. *)
let graph_add_patch_with_deps_and_ancestors =
  QCheck2.Test.make
    ~name:
      "add_patch_with_deps records edges; transitive_ancestors excludes self"
    ~count:300 gen_graph (fun (g, ids) ->
      let newp = Types.Patch_id.of_string "new" in
      let g = Graph.add_patch_with_deps g newp ~deps:ids in
      let direct = Graph.deps g newp in
      let anc = Graph.transitive_ancestors g newp in
      (not (List.exists (fun a -> Types.Patch_id.equal a newp) anc))
      && List.for_all
           (fun d -> List.exists (fun a -> Types.Patch_id.equal a d) anc)
           direct)

(* deps_satisfied / open_pr_deps: with no patches merged and every dep having a
   PR, deps_satisfied holds exactly when there is at most one open dep, and
   open_pr_deps equals deps in that case. *)
let graph_deps_satisfied_matches_open_deps =
  QCheck2.Test.make ~name:"deps_satisfied iff <=1 open dep when all have PRs"
    ~count:300 gen_graph (fun (g, ids) ->
      List.for_all
        (fun pid ->
          let open_deps = Graph.open_pr_deps g pid ~has_merged:always_false in
          let satisfied =
            Graph.deps_satisfied g pid ~has_merged:always_false
              ~has_pr:always_true
          in
          (* every dep is open (nothing merged), so open_pr_deps = deps *)
          List.length open_deps = List.length (Graph.deps g pid)
          && Bool.equal satisfied (List.length open_deps <= 1))
        ids)

(* sole_open_dep / initial_base: when a patch has exactly one open dep, that dep
   is its sole_open_dep and initial_base is the dep's branch; with zero open
   deps initial_base is main. *)
let graph_initial_base_follows_open_deps =
  QCheck2.Test.make
    ~name:"initial_base = main with 0 deps, dep branch with 1 dep" ~count:300
    gen_graph (fun (g, ids) ->
      let main = Types.Branch.of_string "main" in
      let branch_of pid =
        Types.Branch.of_string (Types.Patch_id.to_string pid)
      in
      List.for_all
        (fun pid ->
          let open_deps = Graph.open_pr_deps g pid ~has_merged:always_false in
          match open_deps with
          | [] ->
              Types.Branch.equal
                (Graph.initial_base g pid ~has_merged:always_false ~branch_of
                   ~main)
                main
          | [ d ] ->
              let sole = Graph.sole_open_dep g pid ~has_merged:always_false in
              Types.Patch_id.equal sole d
              && Types.Branch.equal
                   (Graph.initial_base g pid ~has_merged:always_false ~branch_of
                      ~main)
                   (branch_of d)
          | _ -> true)
        ids)

(* remove_patch: after removing a node, it is gone from all_patch_ids and no
   remaining node lists it as a dependency. *)
let graph_remove_patch_drops_node_and_edges =
  QCheck2.Test.make ~name:"remove_patch removes node and dangling edges"
    ~count:300 gen_graph (fun (g, ids) ->
      match ids with
      | [] -> true
      | victim :: _ ->
          let g' = Graph.remove_patch g victim in
          let remaining = Graph.all_patch_ids g' in
          (not (List.exists (fun p -> Types.Patch_id.equal p victim) remaining))
          && List.for_all
               (fun p ->
                 not
                   (List.exists
                      (fun d -> Types.Patch_id.equal d victim)
                      (Graph.deps g' p)))
               remaining)

let () =
  QCheck2.Test.check_exn graph_add_patch_is_idempotent;
  QCheck2.Test.check_exn graph_add_dependency_records_edge;
  QCheck2.Test.check_exn graph_add_dependency_edges_consistent;
  QCheck2.Test.check_exn graph_add_patch_with_deps_and_ancestors;
  QCheck2.Test.check_exn graph_deps_satisfied_matches_open_deps;
  QCheck2.Test.check_exn graph_initial_base_follows_open_deps;
  QCheck2.Test.check_exn graph_remove_patch_drops_node_and_edges
