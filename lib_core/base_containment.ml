(* @archlint.module core
   @archlint.domain base-containment *)

open Base

(* Merged contributions belong to the mode's terminal branch. Their original
   commit identities may disappear when the feature root is rebased. An
   intermediate base can prove containment either by the recorded commits or
   by containing the current terminal branch. Owner integration receipts also
   accept repaired histories without requiring the agent to recreate ancestry.
   Per-commit proofs remain sufficient after unrelated terminal advances. *)
let accepted_terminal_integration ~agent_of_branch ~revision_of_branch ~terminal
    base =
  match agent_of_branch base with
  | None -> false
  | Some base_agent -> (
      match
        Branch_reconcile.integrations base_agent.Patch_agent.branch_reconcile
      with
      | [] -> false
      | receipt :: _ ->
          let terminal_head =
            match agent_of_branch terminal with
            | Some agent ->
                Option.first_some
                  (Patch_agent.expected_remote_head_oid agent)
                  agent.Patch_agent.head_oid
            | None -> revision_of_branch terminal
          in
          Option.value_map terminal_head ~default:false ~f:(fun head ->
              Option.equal String.equal receipt.capture.base_branch
                (Some (Types.Branch.to_string terminal))
              && String.equal
                   (Branch_reconcile.Commit.to_string
                      receipt.capture.target_revision)
                   head))

let contains_at_base ~mode ~graph ~patch_id ~has_merged ~merge_sha ~branch_of
    ~main ~agent_of_branch ~revision_of_branch ~ancestor_oracle base =
  let terminal = Execution_mode.terminal mode patch_id ~branch_of ~main in
  if Types.Branch.equal base terminal then true
  else
    let descendant = Types.Branch.to_string base in
    let merged = Graph.deps graph patch_id |> List.filter ~f:has_merged in
    List.for_all merged ~f:(fun d ->
        Option.value_map (merge_sha d) ~default:false ~f:(fun sha ->
            ancestor_oracle sha ~descendant))
    || accepted_terminal_integration ~agent_of_branch ~revision_of_branch
         ~terminal base
    ||
    match revision_of_branch terminal with
    | Some revision -> ancestor_oracle revision ~descendant
    | None ->
        (not (Types.Branch.equal terminal main))
        && ancestor_oracle (Types.Branch.to_string terminal) ~descendant

let contains_merged_siblings_with_mode ~mode ~graph ~patch_id ~has_merged
    ~merge_sha ~branch_of ~main ~agent_of_branch ~revision_of_branch
    ~ancestor_oracle =
  match
    Execution_mode.base mode graph patch_id ~has_merged ~branch_of ~main
  with
  | None -> false
  | Some base ->
      contains_at_base ~mode ~graph ~patch_id ~has_merged ~merge_sha ~branch_of
        ~main ~agent_of_branch ~revision_of_branch ~ancestor_oracle base

let stale_chain_rebase_target_with_mode ~mode ~graph ~patch_id ~has_merged
    ~merge_sha ~branch_of ~main ~agent_of_branch ~revision_of_branch
    ~ancestor_oracle =
  match Execution_mode.open_deps mode graph patch_id ~has_merged with
  | [] | _ :: _ :: _ ->
      (* No open dep (base is terminal — nothing to freshen) or not at the
         last-but-one-merge edge (the patch defers on dependency satisfaction;
         [sole_open_dep] is undefined). *)
      None
  | [ b ] -> (
      let merged = Graph.deps graph patch_id |> List.filter ~f:has_merged in
      if List.is_empty merged then None
      else
        let contains_base =
          contains_at_base ~mode ~graph ~patch_id ~has_merged ~merge_sha
            ~branch_of ~main ~agent_of_branch ~revision_of_branch
            ~ancestor_oracle
        in
        let contains pid = contains_base (branch_of pid) in
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
        let rec frontier visited c =
          if Set.mem visited c then `Blocked
          else if contains c then `Fresh
          else
            match Execution_mode.open_deps mode graph c ~has_merged with
            | [] ->
                let terminal =
                  Execution_mode.terminal mode c ~branch_of ~main
                in
                if contains_base terminal then `Target c else `Blocked
            | [ d ] -> (
                match frontier (Set.add visited c) d with
                | `Target _ as deeper -> deeper
                | `Fresh -> `Target c (* [d] is fresh — [c] is the frontier *)
                | `Blocked -> `Blocked)
            | _ :: _ :: _ -> `Blocked
        in
        match frontier (Set.empty (module Types.Patch_id)) b with
        | `Target c -> Some c
        | `Fresh | `Blocked -> None)

let contains_merged_siblings =
  contains_merged_siblings_with_mode ~mode:Execution_mode.mainline
    ~agent_of_branch:(fun _ -> None)
    ~revision_of_branch:(fun _ -> None)

let stale_chain_rebase_target =
  stale_chain_rebase_target_with_mode ~mode:Execution_mode.mainline
    ~agent_of_branch:(fun _ -> None)
    ~revision_of_branch:(fun _ -> None)
