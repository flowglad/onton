open Base

type sha = string [@@deriving show, eq, sexp_of, compare]

type ancestry =
  | Local_includes_remote
  | Local_missing_remote
  | No_remote_yet
  | Unknown
[@@deriving show, eq, sexp_of, compare]

type action = Force_push_if_includes | Initial_push
[@@deriving show, eq, sexp_of, compare]

type refusal =
  | No_commits_ahead_of_base
  | Worktree_missing
  | Branch_ref_missing of { branch : string }
  | Branch_switched of { expected : string; got : string option }
  | Local_missing_remote_commits of { local_sha : sha; remote_sha : sha }
[@@deriving show, eq, sexp_of, compare]

type decision = Push of action | Refuse of refusal
[@@deriving show, eq, sexp_of, compare]

let plan ~expected_branch ~worktree_path_exists ~worktree_head_branch
    ~branch_ref_sha ~remote_tracking_sha ~ancestry ~commits_ahead_of_base =
  if not worktree_path_exists then Refuse Worktree_missing
  else
    let head_matches =
      match worktree_head_branch with
      | Some b -> String.equal b expected_branch
      | None -> false
    in
    if not head_matches then
      Refuse
        (Branch_switched
           { expected = expected_branch; got = worktree_head_branch })
    else
      match branch_ref_sha with
      | None -> Refuse (Branch_ref_missing { branch = expected_branch })
      | Some local_sha -> (
          match commits_ahead_of_base with
          | Some 0 -> Refuse No_commits_ahead_of_base
          | None | Some _ -> (
              match (ancestry, remote_tracking_sha) with
              | Local_missing_remote, Some remote_sha ->
                  Refuse
                    (Local_missing_remote_commits { local_sha; remote_sha })
              | ( ( Local_missing_remote | Local_includes_remote | No_remote_yet
                  | Unknown ),
                  None ) ->
                  Push Initial_push
              | (Local_includes_remote | No_remote_yet | Unknown), Some _ ->
                  Push Force_push_if_includes))

let short_label = function
  | Push Force_push_if_includes -> "force_push"
  | Push Initial_push -> "initial_push"
  | Refuse No_commits_ahead_of_base -> "refuse_no_commits"
  | Refuse Worktree_missing -> "refuse_wt_missing"
  | Refuse (Branch_ref_missing _) -> "refuse_ref_missing"
  | Refuse (Branch_switched _) -> "refuse_branch_switched"
  | Refuse (Local_missing_remote_commits _) -> "refuse_local_behind"

let to_push_reject_classify_rejection (r : refusal) :
    Push_reject_classify.rejection option =
  match r with
  | No_commits_ahead_of_base | Worktree_missing -> None
  | Branch_ref_missing _ ->
      Some
        (Push_reject_classify.Local_state_unsafe
           { reason = "refuse_ref_missing" })
  | Branch_switched _ ->
      Some
        (Push_reject_classify.Local_state_unsafe
           { reason = "refuse_branch_switched" })
  | Local_missing_remote_commits _ ->
      Some
        (Push_reject_classify.Local_state_unsafe
           { reason = "refuse_local_behind" })
