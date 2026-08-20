(* @archlint.module shell
   @archlint.domain adhoc-branch *)

open Base
open Onton_core.Types

type outcome =
  | Added of { patch_id : Patch_id.t; change_id : Pr_number.t }
  | Already_registered of Patch_id.t
  | Unsupported_forge of string
  | Remote_not_found
  | Fetch_failed of string
  | No_open_change
  | Handle_collision of Patch_id.t
  | Forge_failed of string

module type FORGE = sig
  type error

  val name : string
  val show_error : error -> string
  val supports_reviews : bool

  val list_prs :
    branch:Branch.t ->
    ?base:Branch.t ->
    state:[ `Open | `All ] ->
    unit ->
    ((Pr_number.t * Branch.t * bool) list, error) Result.t
end

module type WORKTREE = sig
  val fetch_origin_branch :
    fetch_lock:Eio.Mutex.t -> branch:string -> Worktree.fetch_branch_result
end

module Make (Forge : FORGE) (W : WORKTREE) = struct
  let find_registered_branch runtime branch =
    Runtime.read runtime (fun snapshot ->
        Orchestrator.all_agents snapshot.Runtime.orchestrator
        |> List.find_map ~f:(fun (agent : Patch_agent.t) ->
            if Branch.equal agent.Patch_agent.branch branch then
              Some agent.Patch_agent.patch_id
            else None))

  let add ~runtime ~fetch_mutex ~register_change ~branch =
    if Forge.supports_reviews then Unsupported_forge Forge.name
    else
      match find_registered_branch runtime branch with
      | Some patch_id -> Already_registered patch_id
      | None -> (
          match
            W.fetch_origin_branch ~fetch_lock:fetch_mutex
              ~branch:(Branch.to_string branch)
          with
          | Worktree.Fetch_branch_no_remote_ref -> Remote_not_found
          | Worktree.Fetch_branch_error message -> Fetch_failed message
          | Worktree.Fetch_branch_ok -> (
              let main_branch =
                Runtime.read runtime (fun snapshot ->
                    Orchestrator.main_branch snapshot.Runtime.orchestrator)
              in
              match
                Forge.list_prs ~branch ~base:main_branch ~state:`Open ()
              with
              | Error error -> Forge_failed (Forge.show_error error)
              | Ok [] -> No_open_change
              | Ok ((change_id, base_branch, _) :: _) ->
                  let patch_id = Adhoc_target.branch_patch_id ~change_id in
                  let outcome = ref (Added { patch_id; change_id }) in
                  Runtime.update_orchestrator runtime (fun orchestrator ->
                      match
                        Orchestrator.all_agents orchestrator
                        |> List.find ~f:(fun (agent : Patch_agent.t) ->
                            Branch.equal agent.Patch_agent.branch branch)
                      with
                      | Some agent ->
                          outcome :=
                            Already_registered agent.Patch_agent.patch_id;
                          orchestrator
                      | None -> (
                          match
                            Orchestrator.find_agent orchestrator patch_id
                          with
                          | Some _ ->
                              outcome := Handle_collision patch_id;
                              orchestrator
                          | None ->
                              Orchestrator.add_agent orchestrator ~patch_id
                                ~branch ~base_branch ~pr_number:change_id));
                  (match !outcome with
                  | Added _ -> register_change ~patch_id ~pr_number:change_id
                  | Already_registered _ | Unsupported_forge _
                  | Remote_not_found | Fetch_failed _ | No_open_change
                  | Handle_collision _ | Forge_failed _ ->
                      ());
                  !outcome))
end
