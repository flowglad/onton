(* @archlint.module shell
   @archlint.domain priority *)

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
  worktree_path : string;
}
[@@deriving show, eq]

type t = {
  discovered : pr_discovery list;
  recovered_worktrees : worktree_recovery list;
  reset_pending : Patch_id.t list;
  errors : (Patch_id.t * string) list;
  worktree_errors : string list;
}
[@@deriving show, eq]

(** Discover existing worktrees and match them to patches by branch name.
    Returns matched worktrees and an optional error string if listing failed. *)
let recover_worktrees_with (module W : Worktree.S) ~patches =
  let worktrees, list_error =
    try (W.list_with_branches (), None) with
    | Eio.Exn.Io _ as exn ->
        ( [],
          Some
            (Printf.sprintf "worktree discovery failed: %s"
               (Stdlib.Printexc.to_string exn)) )
    | Failure msg ->
        ([], Some (Printf.sprintf "worktree discovery failed: %s" msg))
    | Stdlib.Sys_error msg ->
        ([], Some (Printf.sprintf "worktree discovery failed: %s" msg))
  in
  let recovered =
    List.filter_map worktrees ~f:(fun (path, branch) ->
        match
          List.find patches ~f:(fun (p : Patch.t) ->
              Branch.equal p.branch branch)
        with
        | Some patch ->
            Some { worktree_patch_id = patch.Patch.id; worktree_path = path }
        | None -> None)
  in
  (recovered, list_error)

(** Find agents that were persisted with [busy=true] — these represent crashed
    sessions that need resetting. *)
let find_stale_busy ~agents =
  List.filter_map agents ~f:(fun (agent : Patch_agent.t) ->
      if agent.busy then Some agent.patch_id else None)

module Make (F : Forge.S) (W : Worktree.S) = struct
  (** Query GitHub REST API for a branch, returning discovery info for the first
      non-CLOSED PR. *)
  let discover_pr ~branch =
    match F.list_prs ~branch ~state:`All () with
    | Ok [] -> Ok None
    | Ok ((pr_number, base_branch, merged) :: _) ->
        Ok (Some (pr_number, base_branch, merged))
    | Error err -> Error (F.show_error err)

  let recover_worktrees ~patches = recover_worktrees_with (module W) ~patches

  let reconcile ~patches ?(agents = []) ?pre_recovered_worktrees () =
    let results =
      Eio.Fiber.List.map ~max_fibers:8
        (fun (patch : Patch.t) ->
          let r = discover_pr ~branch:patch.branch in
          (patch, r))
        patches
    in
    let discovered, errors =
      List.fold results ~init:([], [])
        ~f:(fun (discovered, errors) (patch, result) ->
          match result with
          | Ok (Some (pr_number, base_branch, merged)) ->
              ( { patch_id = patch.Patch.id; pr_number; base_branch; merged }
                :: discovered,
                errors )
          | Ok None -> (discovered, errors)
          | Error err -> (discovered, (patch.Patch.id, err) :: errors))
    in
    let recovered_worktrees, worktree_error =
      match pre_recovered_worktrees with
      | Some wts -> (wts, None)
      | None -> recover_worktrees ~patches
    in
    let reset_pending = find_stale_busy ~agents in
    {
      discovered = List.rev discovered;
      recovered_worktrees;
      reset_pending;
      errors = List.rev errors;
      worktree_errors = Option.to_list worktree_error;
    }
end
