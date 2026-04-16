open Base
open Onton.Types
open Onton.Worktree_plan

(* Generators *)

let gen_branch =
  QCheck2.Gen.(
    map Branch.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 1 20)))

let gen_op : op QCheck2.Gen.t =
  QCheck2.Gen.oneof
    [
      QCheck2.Gen.return Ensure_worktree;
      QCheck2.Gen.return Fetch_origin;
      QCheck2.Gen.map (fun b -> Rebase_onto b) gen_branch;
    ]

let gen_plan = QCheck2.Gen.list_size (QCheck2.Gen.int_range 0 8) gen_op

(* Reference predicate, written independently of the implementation: every
   filesystem op (Fetch_origin / Rebase_onto) must have an Ensure_worktree
   somewhere earlier in the list. *)
let reference_invariant (plan : t) =
  let rec loop ensured = function
    | [] -> true
    | Ensure_worktree :: rest -> loop true rest
    | Fetch_origin :: rest -> ensured && loop ensured rest
    | Rebase_onto _ :: rest -> ensured && loop ensured rest
  in
  loop false plan

let () =
  let open QCheck2 in
  let tests =
    [
      (* The safety invariant: any plan returned by [for_rebase] satisfies
         [ensures_worktree_before_fs]. This is the property the patch-48
         bug would have failed. *)
      Test.make ~name:"for_rebase: ensures worktree before any fs op" gen_branch
        (fun new_base -> ensures_worktree_before_fs (for_rebase ~new_base));
      Test.make ~name:"for_merge_conflict: ensures worktree before any fs op"
        gen_branch (fun base ->
          ensures_worktree_before_fs (for_merge_conflict ~base));
      (* Stronger: the very first op of these scenarios is Ensure_worktree.
         Catches regressions where someone reorders or prepends. *)
      Test.make ~name:"for_rebase: first op is Ensure_worktree" gen_branch
        (fun new_base ->
          match for_rebase ~new_base with
          | Ensure_worktree :: _ -> true
          | (Fetch_origin | Rebase_onto _) :: _ | [] -> false);
      Test.make ~name:"for_merge_conflict: first op is Ensure_worktree"
        gen_branch (fun base ->
          match for_merge_conflict ~base with
          | Ensure_worktree :: _ -> true
          | (Fetch_origin | Rebase_onto _) :: _ | [] -> false);
      (* Plans target origin/<base> rather than the local tracking ref —
         this is the "fail closed against stale local refs" requirement
         from the comments at the original bin/main.ml call sites. *)
      Test.make ~name:"for_rebase: rebase target is origin/<new_base>"
        gen_branch (fun new_base ->
          let target =
            Branch.of_string ("origin/" ^ Branch.to_string new_base)
          in
          List.exists (for_rebase ~new_base) ~f:(function
            | Rebase_onto t -> Branch.equal t target
            | Ensure_worktree | Fetch_origin -> false));
      Test.make ~name:"for_merge_conflict: rebase target is origin/<base>"
        gen_branch (fun base ->
          let target = Branch.of_string ("origin/" ^ Branch.to_string base) in
          List.exists (for_merge_conflict ~base) ~f:(function
            | Rebase_onto t -> Branch.equal t target
            | Ensure_worktree | Fetch_origin -> false));
      (* ensures_worktree_before_fs agrees with the independently-written
         reference predicate over arbitrary plans, including malformed ones
         (so we know the predicate itself is right, not just the planners). *)
      Test.make ~name:"ensures_worktree_before_fs: agrees with reference"
        gen_plan (fun plan ->
          Bool.equal
            (ensures_worktree_before_fs plan)
            (reference_invariant plan));
      (* Negative cases: a plan that does fs work without Ensure_worktree
         must be rejected. This is what would have caught patch 48's bug
         had the planner produced such a sequence. *)
      Test.make ~name:"ensures_worktree_before_fs: [Fetch_origin] is rejected"
        Gen.unit (fun () -> not (ensures_worktree_before_fs [ Fetch_origin ]));
      Test.make ~name:"ensures_worktree_before_fs: [Rebase_onto] is rejected"
        gen_branch (fun b -> not (ensures_worktree_before_fs [ Rebase_onto b ]));
      Test.make
        ~name:
          "ensures_worktree_before_fs: Fetch before Ensure (wrong order) is \
           rejected"
        Gen.unit (fun () ->
          not (ensures_worktree_before_fs [ Fetch_origin; Ensure_worktree ]));
      Test.make ~name:"ensures_worktree_before_fs: empty plan is accepted"
        Gen.unit (fun () -> ensures_worktree_before_fs []);
      Test.make
        ~name:"ensures_worktree_before_fs: lone Ensure_worktree is accepted"
        Gen.unit (fun () -> ensures_worktree_before_fs [ Ensure_worktree ]);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t);
  Stdlib.print_endline "all worktree_plan tests passed"
