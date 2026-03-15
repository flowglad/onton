open Base
open Types

type t = {
  deps_map : Patch_id.t list Map.M(Patch_id).t;
  dependents_map : Patch_id.t list Map.M(Patch_id).t;
  all_ids : Patch_id.t list;
}
[@@deriving sexp_of]

let of_patches (patches : Patch.t list) =
  let deps_map =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc { Patch.id; dependencies; _ } ->
        Map.set acc ~key:id ~data:dependencies)
  in
  let dependents_map =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc { Patch.id; dependencies; _ } ->
        List.fold dependencies ~init:acc ~f:(fun acc dep_id ->
            Map.update acc dep_id ~f:(fun existing ->
                id :: Option.value existing ~default:[])))
  in
  let all_ids = List.map patches ~f:(fun { Patch.id; _ } -> id) in
  { deps_map; dependents_map; all_ids }

let deps t patch_id = Map.find t.deps_map patch_id |> Option.value ~default:[]

let depends_on t patch_id ~dep =
  List.mem (deps t patch_id) dep ~equal:Patch_id.equal

let open_pr_deps t patch_id ~has_merged =
  deps t patch_id |> List.filter ~f:(fun d -> not (has_merged d))

let deps_satisfied t patch_id ~has_merged ~has_pr =
  let open_deps = open_pr_deps t patch_id ~has_merged in
  List.length open_deps <= 1
  && List.for_all (deps t patch_id) ~f:(fun d -> has_merged d || has_pr d)

let sole_open_dep t patch_id ~has_merged =
  match open_pr_deps t patch_id ~has_merged with
  | [ d ] -> d
  | _ -> invalid_arg "sole_open_dep: expected exactly 1 open dep"

let initial_base t patch_id ~has_merged ~branch_of ~main =
  match open_pr_deps t patch_id ~has_merged with
  | [] -> main
  | [ d ] -> branch_of d
  | _ -> invalid_arg "initial_base: more than 1 open dep"

let dependents t patch_id =
  Map.find t.dependents_map patch_id |> Option.value ~default:[]

let all_patch_ids t = t.all_ids

(* ── helpers for tests ── *)

let mk_id = Patch_id.of_int

let mk_patch id ?(title = "") ?(branch = "br") deps =
  {
    Patch.id = mk_id id;
    title;
    branch = Branch.of_string branch;
    dependencies = List.map deps ~f:mk_id;
  }

(* ── of_patches / all_patch_ids ── *)

let%test "empty graph has no patch ids" =
  let t = of_patches [] in
  List.is_empty (all_patch_ids t)

let%test "single patch with no deps appears in all_patch_ids" =
  let t = of_patches [ mk_patch 1 [] ] in
  List.equal Patch_id.equal (all_patch_ids t) [ mk_id 1 ]

let%test "all_patch_ids preserves insertion order" =
  let t = of_patches [ mk_patch 1 []; mk_patch 2 []; mk_patch 3 [] ] in
  List.equal Patch_id.equal (all_patch_ids t) [ mk_id 1; mk_id 2; mk_id 3 ]

(* ── deps ── *)

let%test "deps returns empty list for unknown patch" =
  let t = of_patches [ mk_patch 1 [] ] in
  List.is_empty (deps t (mk_id 99))

let%test "deps returns empty list for patch with no dependencies" =
  let t = of_patches [ mk_patch 1 [] ] in
  List.is_empty (deps t (mk_id 1))

let%test "deps returns correct dependencies" =
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  List.equal Patch_id.equal (deps t (mk_id 1)) [ mk_id 2; mk_id 3 ]

let%test "deps chain: A depends on B depends on C" =
  let t =
    of_patches [ mk_patch 1 [ 2 ]; mk_patch 2 [ 3 ]; mk_patch 3 [] ]
  in
  List.equal Patch_id.equal (deps t (mk_id 1)) [ mk_id 2 ]
  && List.equal Patch_id.equal (deps t (mk_id 2)) [ mk_id 3 ]
  && List.is_empty (deps t (mk_id 3))

(* ── depends_on ── *)

let%test "depends_on is true when dep is in deps list" =
  let t = of_patches [ mk_patch 1 [ 2 ] ] in
  depends_on t (mk_id 1) ~dep:(mk_id 2)

let%test "depends_on is false when dep is not in deps list" =
  let t = of_patches [ mk_patch 1 [ 2 ] ] in
  not (depends_on t (mk_id 1) ~dep:(mk_id 3))

let%test "depends_on is false for patch with no deps" =
  let t = of_patches [ mk_patch 1 [] ] in
  not (depends_on t (mk_id 1) ~dep:(mk_id 1))

(* ── dependents ── *)

let%test "dependents returns empty for patch with no dependents" =
  let t = of_patches [ mk_patch 1 [] ] in
  List.is_empty (dependents t (mk_id 1))

let%test "dependents returns empty for unknown patch" =
  let t = of_patches [ mk_patch 1 [] ] in
  List.is_empty (dependents t (mk_id 99))

let%test "dependents returns patches that depend on the given patch" =
  let t = of_patches [ mk_patch 1 [ 2 ]; mk_patch 3 [ 2 ]; mk_patch 2 [] ] in
  let ds = dependents t (mk_id 2) in
  List.length ds = 2
  && List.mem ds (mk_id 1) ~equal:Patch_id.equal
  && List.mem ds (mk_id 3) ~equal:Patch_id.equal

let%test "dependents is one-directional: dependency has no dependents unless specified" =
  let t = of_patches [ mk_patch 1 [ 2 ]; mk_patch 2 [] ] in
  List.is_empty (dependents t (mk_id 1))

(* ── open_pr_deps ── *)

let%test "open_pr_deps returns all deps when none have merged" =
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  let open_deps = open_pr_deps t (mk_id 1) ~has_merged:(fun _ -> false) in
  List.equal Patch_id.equal open_deps [ mk_id 2; mk_id 3 ]

let%test "open_pr_deps returns empty when all deps have merged" =
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  let open_deps = open_pr_deps t (mk_id 1) ~has_merged:(fun _ -> true) in
  List.is_empty open_deps

let%test "open_pr_deps filters out merged deps" =
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  let open_deps =
    open_pr_deps t (mk_id 1)
      ~has_merged:(fun id -> Patch_id.equal id (mk_id 2))
  in
  List.equal Patch_id.equal open_deps [ mk_id 3 ]

let%test "open_pr_deps returns empty for patch with no deps" =
  let t = of_patches [ mk_patch 1 [] ] in
  List.is_empty (open_pr_deps t (mk_id 1) ~has_merged:(fun _ -> false))

(* ── deps_satisfied ── *)

let%test "deps_satisfied true when no deps" =
  let t = of_patches [ mk_patch 1 [] ] in
  deps_satisfied t (mk_id 1)
    ~has_merged:(fun _ -> false)
    ~has_pr:(fun _ -> false)

let%test "deps_satisfied true when single dep has merged" =
  let t = of_patches [ mk_patch 1 [ 2 ] ] in
  deps_satisfied t (mk_id 1)
    ~has_merged:(fun _ -> true)
    ~has_pr:(fun _ -> false)

let%test "deps_satisfied true when single dep has open PR" =
  let t = of_patches [ mk_patch 1 [ 2 ] ] in
  deps_satisfied t (mk_id 1)
    ~has_merged:(fun _ -> false)
    ~has_pr:(fun _ -> true)

let%test "deps_satisfied false when dep has neither merged nor has PR" =
  let t = of_patches [ mk_patch 1 [ 2 ] ] in
  not
    (deps_satisfied t (mk_id 1)
       ~has_merged:(fun _ -> false)
       ~has_pr:(fun _ -> false))

let%test "deps_satisfied false when more than one open dep" =
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  not
    (deps_satisfied t (mk_id 1)
       ~has_merged:(fun _ -> false)
       ~has_pr:(fun _ -> true))

let%test "deps_satisfied true with two deps: one merged one has PR" =
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  deps_satisfied t (mk_id 1)
    ~has_merged:(fun id -> Patch_id.equal id (mk_id 2))
    ~has_pr:(fun id -> Patch_id.equal id (mk_id 3))

let%test "deps_satisfied false when two deps both unmerged even if both have PRs" =
  (* #(open_pr_deps) = 2 > 1, so false *)
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  not
    (deps_satisfied t (mk_id 1)
       ~has_merged:(fun _ -> false)
       ~has_pr:(fun _ -> true))

(* ── sole_open_dep ── *)

let%test "sole_open_dep returns the single unmerged dep" =
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  let result =
    sole_open_dep t (mk_id 1)
      ~has_merged:(fun id -> Patch_id.equal id (mk_id 2))
  in
  Patch_id.equal result (mk_id 3)

let%test "sole_open_dep raises when no open deps" =
  let t = of_patches [ mk_patch 1 [ 2 ] ] in
  try
    let _ = sole_open_dep t (mk_id 1) ~has_merged:(fun _ -> true) in
    false
  with Invalid_argument _ -> true

let%test "sole_open_dep raises when two open deps" =
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  try
    let _ = sole_open_dep t (mk_id 1) ~has_merged:(fun _ -> false) in
    false
  with Invalid_argument _ -> true

(* ── initial_base ── *)

let%test "initial_base returns main when no open deps" =
  let t = of_patches [ mk_patch 1 [ 2 ] ] in
  let main = Branch.of_string "main" in
  let result =
    initial_base t (mk_id 1)
      ~has_merged:(fun _ -> true)
      ~branch_of:(fun _ -> Branch.of_string "other")
      ~main
  in
  Branch.equal result main

let%test "initial_base returns branch_of sole open dep" =
  let t = of_patches [ mk_patch 1 [ 2 ] ] in
  let expected = Branch.of_string "patch-2-branch" in
  let result =
    initial_base t (mk_id 1)
      ~has_merged:(fun _ -> false)
      ~branch_of:(fun _ -> expected)
      ~main:(Branch.of_string "main")
  in
  Branch.equal result expected

let%test "initial_base passes the correct dep id to branch_of" =
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  let captured = ref None in
  let _ =
    try
      initial_base t (mk_id 1)
        ~has_merged:(fun id -> Patch_id.equal id (mk_id 3))
        ~branch_of:(fun id ->
          captured := Some id;
          Branch.of_string "br")
        ~main:(Branch.of_string "main")
    with Invalid_argument _ -> Branch.of_string "main"
  in
  (* dep 2 is the sole open dep since dep 3 is merged *)
  match !captured with
  | Some id -> Patch_id.equal id (mk_id 2)
  | None -> false

let%test "initial_base raises when more than one open dep" =
  let t = of_patches [ mk_patch 1 [ 2; 3 ] ] in
  try
    let _ =
      initial_base t (mk_id 1)
        ~has_merged:(fun _ -> false)
        ~branch_of:(fun _ -> Branch.of_string "br")
        ~main:(Branch.of_string "main")
    in
    false
  with Invalid_argument _ -> true

let%test "initial_base returns main when patch has no deps at all" =
  let t = of_patches [ mk_patch 1 [] ] in
  let main = Branch.of_string "main" in
  let result =
    initial_base t (mk_id 1)
      ~has_merged:(fun _ -> false)
      ~branch_of:(fun _ -> Branch.of_string "other")
      ~main
  in
  Branch.equal result main