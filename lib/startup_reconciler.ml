open Base
open Types

type pr_discovery = {
  patch_id : Patch_id.t;
  pr_number : Pr_number.t;
  base_branch : Branch.t;
  merged : bool;
}
[@@deriving show, eq]

type worktree_recovery = {
  worktree_patch_id : Patch_id.t;
  worktree : Worktree.t;
}
[@@deriving show, eq]

type t = {
  discovered : pr_discovery list;
  recovered_worktrees : worktree_recovery list;
  reset_pending : Patch_id.t list;
  errors : (Patch_id.t * string) list;
}
[@@deriving show, eq]

(** Parse a single PR JSON object, returning [(pr_number, base_branch, merged)]
    for OPEN/MERGED PRs, [None] for CLOSED, or [Error] for unexpected shapes. *)
let parse_pr_entry fields =
  match
    ( List.Assoc.find fields ~equal:String.equal "number",
      List.Assoc.find fields ~equal:String.equal "state",
      List.Assoc.find fields ~equal:String.equal "baseRefName" )
  with
  | Some (`Int n), Some (`String "OPEN"), Some (`String base) ->
      Ok (Some (Pr_number.of_int n, Branch.of_string base, (* merged *) false))
  | Some (`Int n), Some (`String "MERGED"), Some (`String base) ->
      Ok (Some (Pr_number.of_int n, Branch.of_string base, (* merged *) true))
  | Some (`Int _), Some (`String "CLOSED"), _ -> Ok None
  | Some (`Int _), Some (`String state), _ ->
      Error (Printf.sprintf "unexpected PR state: %s" state)
  | _ -> Error "unexpected PR JSON field shape"

(** Query [gh pr list] for a branch, returning discovery info for the first
    non-CLOSED PR. Iterates through all matching PRs to handle cases where the
    most recent PR is CLOSED but an older one is still OPEN or MERGED. *)
let discover_pr ~process_mgr ~token ~owner ~repo ~branch =
  let args =
    [
      "gh";
      "pr";
      "list";
      "--repo";
      Printf.sprintf "%s/%s" owner repo;
      "--head";
      Branch.to_string branch;
      "--state";
      "all";
      "--json";
      "number,state,baseRefName";
    ]
  in
  let base_env = Unix.environment () in
  let env = Array.append [| Printf.sprintf "GH_TOKEN=%s" token |] base_env in
  try
    let buf = Buffer.create 256 in
    Eio.Process.run ~stdout:(Eio.Flow.buffer_sink buf) ~env process_mgr args;
    let output = Buffer.contents buf in
    match Yojson.Basic.from_string output with
    | `List entries ->
        let rec find_live = function
          | [] -> Ok None
          | `Assoc fields :: rest -> (
              match parse_pr_entry fields with
              | Ok (Some _ as result) -> Ok result
              | Ok None -> find_live rest
              | Error _ as e -> e)
          | _ :: _ ->
              Error (Printf.sprintf "unexpected PR JSON shape: %s" output)
        in
        find_live entries
    | _ -> Error (Printf.sprintf "unexpected JSON: %s" output)
  with exn -> Error (Stdlib.Printexc.to_string exn)

(** Discover existing worktrees and match them to patches by branch name. *)
let recover_worktrees ~process_mgr ~repo_root ~patches =
  let worktrees =
    try Worktree.list_with_branches ~process_mgr ~repo_root with _ -> []
  in
  List.filter_map worktrees ~f:(fun (path, branch) ->
      match
        List.find patches ~f:(fun (p : Patch.t) -> Branch.equal p.branch branch)
      with
      | Some patch ->
          let wt =
            Worktree.add_existing ~patch_id:patch.Patch.id ~branch ~path
          in
          Some { worktree_patch_id = patch.Patch.id; worktree = wt }
      | None -> None)

(** Find agents that were persisted with [busy=true] — these represent crashed
    sessions that need resetting. *)
let find_stale_busy ~agents =
  List.filter_map agents ~f:(fun (agent : Patch_agent.t) ->
      if agent.busy then Some agent.patch_id else None)

let reconcile ~process_mgr ~token ~owner ~repo ~patches ?(repo_root = ".")
    ?(agents = []) () =
  let discovered, errors =
    List.fold patches ~init:([], [])
      ~f:(fun (discovered, errors) (patch : Patch.t) ->
        match
          discover_pr ~process_mgr ~token ~owner ~repo ~branch:patch.branch
        with
        | Ok (Some (pr_number, base_branch, merged)) ->
            ( { patch_id = patch.id; pr_number; base_branch; merged }
              :: discovered,
              errors )
        | Ok None -> (discovered, errors)
        | Error err -> (discovered, (patch.id, err) :: errors))
  in
  let recovered_worktrees =
    recover_worktrees ~process_mgr ~repo_root ~patches
  in
  let reset_pending = find_stale_busy ~agents in
  {
    discovered = List.rev discovered;
    recovered_worktrees;
    reset_pending;
    errors = List.rev errors;
  }
