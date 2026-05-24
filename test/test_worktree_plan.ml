open Base
open Onton_core.Types
open Onton_core.Worktree_plan

(* Generators *)

let gen_branch =
  QCheck2.Gen.(
    map Branch.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 1 20)))

let gen_ref_name =
  QCheck2.Gen.(
    map
      (fun b -> "origin/" ^ b)
      (string_size ~gen:(char_range 'a' 'z') (int_range 1 20)))

let gen_op : op QCheck2.Gen.t =
  QCheck2.Gen.oneof
    [
      QCheck2.Gen.return Ensure_worktree;
      QCheck2.Gen.return Fetch_origin;
      QCheck2.Gen.map2
        (fun ref_name slot -> Capture_anchor { ref_name; slot })
        gen_ref_name
        (QCheck2.Gen.int_range 0 4);
      QCheck2.Gen.map (fun b -> Rebase_onto b) gen_branch;
      QCheck2.Gen.map2
        (fun slot base -> Record_anchor_on_success { slot; base })
        (QCheck2.Gen.int_range 0 4)
        gen_branch;
    ]

let gen_plan = QCheck2.Gen.list_size (QCheck2.Gen.int_range 0 8) gen_op

(* Reference predicate, written independently of the implementation: every
   non-[Ensure_worktree] op must have an [Ensure_worktree] somewhere earlier
   in the list. *)
let reference_invariant (plan : t) =
  let rec loop ensured = function
    | [] -> true
    | Ensure_worktree :: rest -> loop true rest
    | Fetch_origin :: rest -> ensured && loop ensured rest
    | Capture_anchor _ :: rest -> ensured && loop ensured rest
    | Rebase_onto _ :: rest -> ensured && loop ensured rest
    | Record_anchor_on_success _ :: rest -> ensured && loop ensured rest
  in
  loop false plan

let first_op_is_ensure_worktree (plan : t) =
  match plan with
  | Ensure_worktree :: _ -> true
  | ( Fetch_origin | Capture_anchor _ | Rebase_onto _
    | Record_anchor_on_success _ )
    :: _
  | [] ->
      false

let plan_has_rebase_target (plan : t) target =
  List.exists plan ~f:(function
    | Rebase_onto t -> Branch.equal t target
    | Ensure_worktree | Fetch_origin | Capture_anchor _
    | Record_anchor_on_success _ ->
        false)

let plan_has_capture_for (plan : t) ref_name =
  List.exists plan ~f:(function
    | Capture_anchor { ref_name = r; _ } -> String.equal r ref_name
    | Ensure_worktree | Fetch_origin | Rebase_onto _
    | Record_anchor_on_success _ ->
        false)

let plan_has_record_for (plan : t) base =
  List.exists plan ~f:(function
    | Record_anchor_on_success { base = b; _ } -> Branch.equal b base
    | Ensure_worktree | Fetch_origin | Capture_anchor _ | Rebase_onto _ -> false)

let () =
  let open QCheck2 in
  let tests =
    [
      Test.make ~name:"for_rebase: ensures worktree before any fs op" gen_branch
        (fun new_base -> ensures_worktree_before_fs (for_rebase ~new_base));
      Test.make ~name:"for_merge_conflict: ensures worktree before any fs op"
        gen_branch (fun base ->
          ensures_worktree_before_fs (for_merge_conflict ~base));
      Test.make ~name:"for_start: ensures worktree before any fs op" gen_branch
        (fun base -> ensures_worktree_before_fs (for_start ~base));
      Test.make ~name:"for_rebase: first op is Ensure_worktree" gen_branch
        (fun new_base -> first_op_is_ensure_worktree (for_rebase ~new_base));
      Test.make ~name:"for_merge_conflict: first op is Ensure_worktree"
        gen_branch (fun base ->
          first_op_is_ensure_worktree (for_merge_conflict ~base));
      Test.make ~name:"for_start: first op is Ensure_worktree" gen_branch
        (fun base -> first_op_is_ensure_worktree (for_start ~base));
      Test.make ~name:"for_rebase: rebase target is origin/<new_base>"
        gen_branch (fun new_base ->
          let target =
            Branch.of_string ("origin/" ^ Branch.to_string new_base)
          in
          plan_has_rebase_target (for_rebase ~new_base) target);
      Test.make ~name:"for_merge_conflict: rebase target is origin/<base>"
        gen_branch (fun base ->
          let target = Branch.of_string ("origin/" ^ Branch.to_string base) in
          plan_has_rebase_target (for_merge_conflict ~base) target);
      (* for_start has no Rebase_onto — it just captures and records. *)
      Test.make ~name:"for_start: no Rebase_onto op" gen_branch (fun base ->
          not
            (List.exists (for_start ~base) ~f:(function
              | Rebase_onto _ -> true
              | Ensure_worktree | Fetch_origin | Capture_anchor _
              | Record_anchor_on_success _ ->
                  false)));
      Test.make
        ~name:"for_start: includes Capture_anchor for origin/<base> in slot 0"
        gen_branch (fun base ->
          plan_has_capture_for (for_start ~base)
            ("origin/" ^ Branch.to_string base));
      Test.make ~name:"for_start: includes Record_anchor_on_success for base"
        gen_branch (fun base -> plan_has_record_for (for_start ~base) base);
      Test.make ~name:"ensures_worktree_before_fs: agrees with reference"
        gen_plan (fun plan ->
          Bool.equal
            (ensures_worktree_before_fs plan)
            (reference_invariant plan));
      Test.make ~name:"ensures_worktree_before_fs: [Fetch_origin] is rejected"
        Gen.unit (fun () -> not (ensures_worktree_before_fs [ Fetch_origin ]));
      Test.make ~name:"ensures_worktree_before_fs: [Rebase_onto] is rejected"
        gen_branch (fun b -> not (ensures_worktree_before_fs [ Rebase_onto b ]));
      Test.make
        ~name:"ensures_worktree_before_fs: [Capture_anchor] alone is rejected"
        (Gen.pair gen_ref_name (Gen.int_range 0 4))
        (fun (ref_name, slot) ->
          not (ensures_worktree_before_fs [ Capture_anchor { ref_name; slot } ]));
      Test.make
        ~name:
          "ensures_worktree_before_fs: [Record_anchor_on_success] alone is \
           rejected"
        (Gen.pair (Gen.int_range 0 4) gen_branch)
        (fun (slot, base) ->
          not
            (ensures_worktree_before_fs
               [ Record_anchor_on_success { slot; base } ]));
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
