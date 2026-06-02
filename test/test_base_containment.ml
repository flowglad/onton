open Base
open Onton_core

(** Property/regression tests for {!Base_containment.contains_merged_siblings}.

    Git ancestry is mocked: [contained] is the set of merge-commit SHAs the base
    branch is treated as containing; the oracle ignores [descendant] and just
    membership-checks [ancestor]. This exercises every fan-in permutation,
    fail-closed-on-missing-SHA, and the "only the patch's own deps are
    consulted" invariant, purely. *)

module Gen = QCheck2.Gen
module Test = QCheck2.Test
module BC = Base_containment

let pid s = Types.Patch_id.of_string s
let br s = Types.Branch.of_string s
let main = br "main"

let mk_patch ~id ~deps : Types.Patch.t =
  {
    id = pid id;
    title = "";
    description = "";
    branch = br id;
    dependencies = List.map deps ~f:pid;
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

(* P -> {d1, d2, d3}, d2 a stacked child of d1, d3 an independent sibling. *)
let fanin_graph =
  Graph.of_patches
    [
      mk_patch ~id:"p" ~deps:[ "d1"; "d2"; "d3" ];
      mk_patch ~id:"d1" ~deps:[];
      mk_patch ~id:"d2" ~deps:[ "d1" ];
      mk_patch ~id:"d3" ~deps:[];
    ]

let branch_of p = br (Types.Patch_id.to_string p)
let merge_sha p = Some ("sha-" ^ Types.Patch_id.to_string p)
let sha_of s = "sha-" ^ s

(* Oracle from a set of contained SHAs (descendant ignored). *)
let oracle_of contained ancestor ~descendant:_ =
  List.mem contained ancestor ~equal:String.equal

let contains ~merged ~contained ?(merge_sha = merge_sha) () =
  BC.contains_merged_siblings ~graph:fanin_graph ~patch_id:(pid "p")
    ~has_merged:(fun d -> List.mem merged d ~equal:Types.Patch_id.equal)
    ~merge_sha ~branch_of ~main ~ancestor_oracle:(oracle_of contained)

(* ---------- regression: each last-but-one-merge permutation ---------- *)

(* {d1,d3 merged, d2 open}: base is d2's branch. It contains d1 (stacked child)
   but NOT d3 (independent sibling). → not contained. *)
let prop_open_d2_missing_d3 =
  Test.make ~name:"BC: {d1,d3 merged, d2 open}, base lacks d3 → false" ~count:1
    (Gen.return ()) (fun () ->
      not
        (contains ~merged:[ pid "d1"; pid "d3" ] ~contained:[ sha_of "d1" ] ()))

(* Same, but the base (d2) has since been rebased to absorb d3 → contained. *)
let prop_open_d2_after_rebase =
  Test.make ~name:"BC: {d1,d3 merged, d2 open}, base has d1+d3 → true" ~count:1
    (Gen.return ()) (fun () ->
      contains
        ~merged:[ pid "d1"; pid "d3" ]
        ~contained:[ sha_of "d1"; sha_of "d3" ]
        ())

let prop_open_d1 =
  Test.make ~name:"BC: {d2,d3 merged, d1 open}, base lacks them → false"
    ~count:1 (Gen.return ()) (fun () ->
      not (contains ~merged:[ pid "d2"; pid "d3" ] ~contained:[] ()))

let prop_open_d3 =
  Test.make ~name:"BC: {d1,d2 merged, d3 open}, base lacks them → false"
    ~count:1 (Gen.return ()) (fun () ->
      not (contains ~merged:[ pid "d1"; pid "d2" ] ~contained:[] ()))

(* ---------- boundary cases ---------- *)

let prop_all_merged_base_main =
  Test.make ~name:"BC: all deps merged → base main → true (no oracle)" ~count:1
    (Gen.return ()) (fun () ->
      contains ~merged:[ pid "d1"; pid "d2"; pid "d3" ] ~contained:[] ())

let prop_multi_open_false =
  Test.make ~name:"BC: >1 open dep → false (not at edge)" ~count:1
    (Gen.return ()) (fun () ->
      (* nothing merged → 3 open deps *)
      (not (contains ~merged:[] ~contained:[ sha_of "d1" ] ()))
      (* one merged → 2 open deps *)
      && not (contains ~merged:[ pid "d1" ] ~contained:[ sha_of "d1" ] ()))

let prop_fail_closed_missing_sha =
  Test.make ~name:"BC: merged dep with unknown merge_sha → false (fail-closed)"
    ~count:1 (Gen.return ()) (fun () ->
      let merge_sha p =
        if Types.Patch_id.equal p (pid "d3") then None else merge_sha p
      in
      (* d3 is contained by ancestry, but its SHA is unknown → still false. *)
      not
        (contains
           ~merged:[ pid "d1"; pid "d3" ]
           ~contained:[ sha_of "d1"; sha_of "d3" ]
           ~merge_sha ()))

(* Invariant (ii): only the patch's own merged deps are consulted. In a graph
   where [p -> {a, b}] (b open, a merged) and an unrelated patch [q] has also
   merged, [q]'s squash commit being absent from the base must NOT flip the
   result — the oracle is only asked about [a]. *)
let prop_only_own_deps_consulted =
  Test.make ~name:"BC: unrelated merges never flip containment (invariant ii)"
    ~count:1 (Gen.return ()) (fun () ->
      let graph =
        Graph.of_patches
          [
            mk_patch ~id:"p" ~deps:[ "a"; "b" ];
            mk_patch ~id:"a" ~deps:[];
            mk_patch ~id:"b" ~deps:[];
            mk_patch ~id:"q" ~deps:[];
            (* unrelated *)
          ]
      in
      let merged = [ pid "a"; pid "q" ] in
      (* base is b; it contains a but not q. q is not a dep of p. *)
      BC.contains_merged_siblings ~graph ~patch_id:(pid "p")
        ~has_merged:(fun d -> List.mem merged d ~equal:Types.Patch_id.equal)
        ~merge_sha ~branch_of ~main
        ~ancestor_oracle:(oracle_of [ sha_of "a" ]))

(* Property: with exactly one open dep and a non-main base, containment holds
   iff every *other* (merged) dep's SHA is in [contained]. *)
let prop_iff_all_merged_in_contained =
  Test.make ~name:"BC: contained iff all merged-dep SHAs ∈ base" ~count:300
    Gen.(
      (* choose which single dep stays open; the rest are merged *)
      let* open_dep = oneof_list [ "d1"; "d2"; "d3" ] in
      (* independently decide, per merged dep, whether its SHA is in the base *)
      let* d1_in = bool in
      let* d2_in = bool in
      let* d3_in = bool in
      return (open_dep, d1_in, d2_in, d3_in))
    (fun (open_dep, d1_in, d2_in, d3_in) ->
      let all = [ ("d1", d1_in); ("d2", d2_in); ("d3", d3_in) ] in
      let merged =
        List.filter_map all ~f:(fun (d, _) ->
            if String.equal d open_dep then None else Some (pid d))
      in
      let contained =
        List.filter_map all ~f:(fun (d, in_base) ->
            if String.equal d open_dep then None
            else if in_base then Some (sha_of d)
            else None)
      in
      let expected =
        List.for_all all ~f:(fun (d, in_base) ->
            String.equal d open_dep || in_base)
      in
      Bool.equal (contains ~merged ~contained ()) expected)

(* ---------- stale_chain_rebase_target (the frontier) ---------- *)

(* P fans in on a merged sibling [s] and the top of an open chain
   c3 ← c2 ← c1 (c1's deps all merged → its structural base is main, the
   content source). [s]'s squash lives on main only; the frontier must walk it
   up the chain one layer at a time — the FLI-3 seed-440463877 / seed-7
   livelock shape. The oracle here is per-branch: [healed] is the set of
   branch names treated as containing [s]'s squash. *)
let chain_graph =
  Graph.of_patches
    [
      mk_patch ~id:"p" ~deps:[ "s"; "c3" ];
      mk_patch ~id:"s" ~deps:[];
      mk_patch ~id:"c3" ~deps:[ "c2" ];
      mk_patch ~id:"c2" ~deps:[ "c1" ];
      mk_patch ~id:"c1" ~deps:[];
    ]

let frontier ?(graph = chain_graph) ?(merged = [ pid "s" ])
    ?(merge_sha = merge_sha) ~healed () =
  let ancestor_oracle ancestor ~descendant =
    String.equal ancestor (sha_of "s")
    && List.mem healed descendant ~equal:String.equal
  in
  BC.stale_chain_rebase_target ~graph ~patch_id:(pid "p")
    ~has_merged:(fun d -> List.mem merged d ~equal:Types.Patch_id.equal)
    ~merge_sha ~branch_of ~main ~ancestor_oracle

let eq_target = Option.equal Types.Patch_id.equal

let prop_frontier_starts_at_chain_bottom =
  Test.make
    ~name:"BCF-1: nothing healed → frontier is the deepest layer (base main)"
    ~count:1 (Gen.return ()) (fun () ->
      eq_target (frontier ~healed:[] ()) (Some (pid "c1")))

let prop_frontier_moves_up_one_layer_per_heal =
  Test.make ~name:"BCF-2: healing the frontier moves it up exactly one layer"
    ~count:1 (Gen.return ()) (fun () ->
      eq_target (frontier ~healed:[ "c1" ] ()) (Some (pid "c2"))
      && eq_target (frontier ~healed:[ "c1"; "c2" ] ()) (Some (pid "c3"))
      && eq_target (frontier ~healed:[ "c1"; "c2"; "c3" ] ()) None)

let prop_frontier_skips_stale_layers_below_fresh =
  Test.make
    ~name:
      "BCF-3: a stale layer below a fresh one is irrelevant — the target's own \
       base supplies the content" ~count:1 (Gen.return ()) (fun () ->
      (* c2 healed, c1 not: c3 rebases onto fresh branch-c2 directly. *)
      eq_target (frontier ~healed:[ "c2" ] ()) (Some (pid "c3")))

let prop_frontier_none_without_merged_deps =
  Test.make ~name:"BCF-4: no merged deps → None" ~count:1 (Gen.return ())
    (fun () -> eq_target (frontier ~merged:[] ~healed:[] ()) None)

let prop_frontier_fail_closed_missing_sha =
  Test.make
    ~name:"BCF-5: merged dep with unknown merge_sha → None (fail-closed)"
    ~count:1 (Gen.return ()) (fun () ->
      let merge_sha p =
        if Types.Patch_id.equal p (pid "s") then None else merge_sha p
      in
      eq_target (frontier ~merge_sha ~healed:[] ()) None)

let prop_frontier_none_off_the_edge =
  Test.make ~name:"BCF-6: >1 open dep (not at the edge) → None" ~count:1
    (Gen.return ()) (fun () ->
      (* s not merged → P has two open deps. *)
      eq_target (frontier ~merged:[ pid "c1" ] ~healed:[] ()) None)

let prop_frontier_fail_closed_on_interrupted_chain =
  Test.make ~name:"BCF-7: chain interrupted by a multi-open-dep layer → None"
    ~count:1 (Gen.return ()) (fun () ->
      (* c2 itself fans in on two open deps: the squash cannot flow past it
         until its own merges land. *)
      let graph =
        Graph.of_patches
          [
            mk_patch ~id:"p" ~deps:[ "s"; "c3" ];
            mk_patch ~id:"s" ~deps:[];
            mk_patch ~id:"c3" ~deps:[ "c2" ];
            mk_patch ~id:"c2" ~deps:[ "c1"; "x" ];
            mk_patch ~id:"c1" ~deps:[];
            mk_patch ~id:"x" ~deps:[];
          ]
      in
      eq_target (frontier ~graph ~healed:[] ()) None)

(* Spec property over arbitrary heal subsets: whenever a target exists, it is
   stale itself, its structural base already contains the squash (main always
   does), and every layer above it back up to B is stale too (otherwise a
   shallower fresh layer would have stopped the walk earlier and containment
   would flip sooner). *)
let prop_frontier_target_is_stale_with_fresh_base =
  Test.make
    ~name:
      "BCF-8: target is a stale layer whose own base is fresh (random heal \
       subsets)"
    ~count:200
    Gen.(
      let* c1 = bool in
      let* c2 = bool in
      let* c3 = bool in
      return (c1, c2, c3))
    (fun (c1, c2, c3) ->
      let healed =
        List.filter_map
          [ ("c1", c1); ("c2", c2); ("c3", c3) ]
          ~f:(fun (b, h) -> if h then Some b else None)
      in
      let fresh b = List.mem healed b ~equal:String.equal in
      match frontier ~healed () with
      | None -> fresh "c3" (* only when B itself already has the content *)
      | Some t ->
          let t = Types.Patch_id.to_string t in
          let base_fresh =
            match t with
            | "c3" -> fresh "c2"
            | "c2" -> fresh "c1"
            | "c1" -> true (* base is main, the content source *)
            | _ -> false
          in
          (not (fresh t)) && base_fresh)

let () =
  let runner = QCheck_base_runner.run_tests_main in
  ignore
    (runner
       [
         prop_open_d2_missing_d3;
         prop_open_d2_after_rebase;
         prop_open_d1;
         prop_open_d3;
         prop_all_merged_base_main;
         prop_multi_open_false;
         prop_fail_closed_missing_sha;
         prop_only_own_deps_consulted;
         prop_iff_all_merged_in_contained;
         prop_frontier_starts_at_chain_bottom;
         prop_frontier_moves_up_one_layer_per_heal;
         prop_frontier_skips_stale_layers_below_fresh;
         prop_frontier_none_without_merged_deps;
         prop_frontier_fail_closed_missing_sha;
         prop_frontier_none_off_the_edge;
         prop_frontier_fail_closed_on_interrupted_chain;
         prop_frontier_target_is_stale_with_fresh_base;
       ])
