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
       ])
