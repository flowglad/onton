open Base

let contains_merged_siblings ~graph ~patch_id ~has_merged ~merge_sha ~branch_of
    ~main ~ancestor_oracle =
  match Graph.open_pr_deps graph patch_id ~has_merged with
  | _ :: _ :: _ ->
      (* Not at the last-but-one-merge edge; the patch is not startable and
         [initial_base] would raise. Report [false] (irrelevant — the patch
         defers on [deps_satisfied] anyway). *)
      false
  | [] | [ _ ] ->
      (* [Graph.initial_base] is total for [<= 1] open dep: [[]] (all deps
         merged, including a root patch with no deps) resolves to [main]; [[d]]
         resolves to [d]'s branch. It only raises for [> 1] open deps, which the
         arm above excludes — so this call never raises. *)
      let base =
        Graph.initial_base graph patch_id ~has_merged ~branch_of ~main
      in
      if Types.Branch.equal base main then true
      else
        Graph.deps graph patch_id |> List.filter ~f:has_merged
        |> List.for_all ~f:(fun d ->
            match merge_sha d with
            | None -> false (* fail-closed until the merge SHA is known *)
            | Some sha ->
                ancestor_oracle sha ~descendant:(Types.Branch.to_string base))
