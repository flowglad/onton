open Base

type op = Ensure_worktree | Fetch_origin | Rebase_onto of Types.Branch.t
[@@deriving show, eq, sexp_of, compare]

type t = op list [@@deriving show, eq, sexp_of, compare]

let origin_of branch =
  Types.Branch.of_string
    (Printf.sprintf "origin/%s" (Types.Branch.to_string branch))

let for_rebase ~new_base =
  [ Ensure_worktree; Fetch_origin; Rebase_onto (origin_of new_base) ]

let for_merge_conflict ~base =
  [ Ensure_worktree; Fetch_origin; Rebase_onto (origin_of base) ]

let ensures_worktree_before_fs (plan : t) =
  let rec loop ensured = function
    | [] -> true
    | Ensure_worktree :: rest -> loop true rest
    | (Fetch_origin | Rebase_onto _) :: rest -> ensured && loop ensured rest
  in
  loop false plan
