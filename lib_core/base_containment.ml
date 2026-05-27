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
