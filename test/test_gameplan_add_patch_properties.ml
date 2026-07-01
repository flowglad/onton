(* @archlint.module test
   @archlint.domain anchor *)

open Base
open Onton_core.Types
open Onton_test_support.Test_generators

(* [gen_gameplan] always yields at least one patch (base case [patch-0]). *)
let existing_ids (g : Gameplan.t) =
  List.map g.Gameplan.patches ~f:(fun p -> p.Patch.id)

(* Pair a gameplan with a random subset (possibly empty) of its own patch ids
   to use as valid dependencies. *)
let gen_gameplan_and_valid_deps =
  let open QCheck2.Gen in
  gen_gameplan >>= fun g ->
  match existing_ids g with
  | [] -> return (g, [])
  | ids ->
      list_size (int_range 0 (List.length ids)) (oneof_list ids) >>= fun deps ->
      return (g, deps)

let prop_totality =
  QCheck2.Test.make ~name:"add_patch: never raises" ~count:500
    QCheck2.Gen.(triple gen_gameplan string string)
    (fun (g, title, description) ->
      try
        ignore
          (Gameplan.add_patch g ~title ~description ~dependencies:[]
            : (Gameplan.t * Patch.t, string) Result.t);
        true
      with _ -> false)

let prop_valid_deps_appends_one_fresh_patch =
  QCheck2.Test.make ~name:"add_patch: valid deps append exactly one fresh patch"
    ~count:500 gen_gameplan_and_valid_deps (fun (g, deps) ->
      match
        Gameplan.add_patch g ~title:"t" ~description:"d" ~dependencies:deps
      with
      | Error _ -> false
      | Ok (g', patch) ->
          let before = existing_ids g in
          let grew_by_one =
            List.length g'.Gameplan.patches = List.length g.Gameplan.patches + 1
          in
          let appended_last =
            match List.last g'.Gameplan.patches with
            | Some p -> Patch.equal p patch
            | None -> false
          in
          (* Generated id is fresh (not among pre-existing ids). *)
          let id_fresh =
            not (List.mem before patch.Patch.id ~equal:Patch_id.equal)
          in
          (* Branch follows the shared {slug}/patch-{id} convention. *)
          let branch_ok =
            Branch.equal patch.Patch.branch
              (Gameplan.branch_of_id g patch.Patch.id)
          in
          (* Referential integrity: the new patch points only at pre-existing
             patches. This is exactly what keeps the dependency graph acyclic —
             a sink pointing backward can never close a cycle. *)
          let deps_backward =
            List.for_all patch.Patch.dependencies ~f:(fun d ->
                List.mem before d ~equal:Patch_id.equal)
          in
          grew_by_one && appended_last && id_fresh && branch_ok && deps_backward)

let prop_unknown_dep_rejected =
  QCheck2.Test.make ~name:"add_patch: unknown dependency id is rejected"
    ~count:500 gen_gameplan (fun g ->
      (* An id shaped unlike any generated id (those are "patch-N"/"addN"). *)
      let bogus = Patch_id.of_string "no-such-patch-zzz" in
      let deps = bogus :: existing_ids g in
      match
        Gameplan.add_patch g ~title:"t" ~description:"d" ~dependencies:deps
      with
      | Error _ -> true
      | Ok _ -> false)

let prop_deps_deduped_preserving_order =
  QCheck2.Test.make ~name:"add_patch: dependencies are deduped in order"
    ~count:500 gen_gameplan_and_valid_deps (fun (g, deps) ->
      (* Duplicate every dep; result must equal the order-preserving dedup. *)
      let doubled = deps @ deps in
      let expected =
        List.fold deps
          ~init:([], Set.empty (module Patch_id))
          ~f:(fun (acc, seen) d ->
            if Set.mem seen d then (acc, seen) else (d :: acc, Set.add seen d))
        |> fst |> List.rev
      in
      match
        Gameplan.add_patch g ~title:"t" ~description:"d" ~dependencies:doubled
      with
      | Error _ -> false
      | Ok (_, patch) ->
          List.equal Patch_id.equal patch.Patch.dependencies expected)

let () =
  let suite =
    [
      prop_totality;
      prop_valid_deps_appends_one_fresh_patch;
      prop_unknown_dep_rejected;
      prop_deps_deduped_preserving_order;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode
