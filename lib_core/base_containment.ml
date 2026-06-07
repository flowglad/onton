(* @archlint.module core
   @archlint.domain base-containment *)

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

(* [main] is unused: a layer whose open deps are all merged resolves to main
   structurally, which is detected via [open_pr_deps = []] — the branch name
   itself is never compared. Kept in the signature for symmetry with
   [contains_merged_siblings] (callers thread the same inputs to both). *)
let stale_chain_rebase_target ~graph ~patch_id ~has_merged ~merge_sha ~branch_of
    ~main:_ ~ancestor_oracle =
  match Graph.open_pr_deps graph patch_id ~has_merged with
  | [] | _ :: _ :: _ ->
      (* No open dep (base is main — nothing to freshen) or not at the
         last-but-one-merge edge (the patch defers on dependency satisfaction;
         [sole_open_dep] is undefined). *)
      None
  | [ b ] -> (
      let merged_dep_shas =
        Graph.deps graph patch_id |> List.filter ~f:has_merged
        |> List.map ~f:merge_sha
      in
      if
        List.is_empty merged_dep_shas
        || List.exists merged_dep_shas ~f:Option.is_none
      then
        (* No merged siblings to absorb, or a merge SHA is not yet known —
           fail closed and create no demand; the cache recompute on a later
           tick re-derives the target once the SHA lands. *)
        None
      else
        let shas = List.filter_opt merged_dep_shas in
        let contains pid =
          let branch_s = Types.Branch.to_string (branch_of pid) in
          List.for_all shas ~f:(fun sha ->
              ancestor_oracle sha ~descendant:branch_s)
        in
        (* Walk [b]'s sole-open-dep chain downward. The frontier is the
           deepest stale layer whose own structural base already contains the
           SHAs — main contains every merged squash by definition, and a layer
           above a fresh layer rebases onto that fresh branch. The three-way
           result keeps "deeper layer is fresh" (the layer above it is the
           frontier) distinct from "chain blocked" (a fan-in layer with
           several open deps: the squash cannot flow past it until its own
           merges land, so NO layer above it is a useful target — rebasing one
           would absorb a base that still lacks the squash, the churn this
           function exists to prevent). *)
        let rec frontier c =
          if contains c then `Fresh
          else
            match Graph.open_pr_deps graph c ~has_merged with
            | [] -> `Target c (* base is main: the content source *)
            | [ d ] -> (
                match frontier d with
                | `Target _ as deeper -> deeper
                | `Fresh -> `Target c (* [d] is fresh — [c] is the frontier *)
                | `Blocked -> `Blocked)
            | _ :: _ :: _ -> `Blocked
        in
        match frontier b with `Target c -> Some c | `Fresh | `Blocked -> None)
