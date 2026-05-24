module type S = sig
  val execute :
    patch_id:Types.Patch_id.t ->
    agent:Patch_agent.t ->
    fetch_lock:Eio.Mutex.t ->
    fail_label:string ->
    ancestor_ids:Types.Patch_id.t list ->
    Worktree_plan.t ->
    Worktree.rebase_result * string * Worktree_plan.anchor_event list
end

module Make (W : Worktree.S) (Env : Run_env.S) : S = struct
  module WS = Worktree_setup.Make (W) (Env)

  let execute ~patch_id ~(agent : Patch_agent.t) ~fetch_lock ~fail_label
      ~ancestor_ids (plan : Worktree_plan.t) =
    let default_path =
      Worktree.worktree_dir ~project_name:Env.project_name ~patch_id
    in
    (* Slot table for [Capture_anchor] -> [Record_anchor_on_success]
       handoff. Slots are sparse, so a small association list is plenty. *)
    let slots : (int * string option) list ref = ref [] in
    let set_slot k v =
      slots :=
        (k, v)
        :: Base.List.filter !slots ~f:(fun (k', _) -> not (Base.Int.equal k k'))
    in
    let get_slot k =
      Base.List.Assoc.find !slots ~equal:Base.Int.equal k
      |> Base.Option.bind ~f:(fun x -> x)
    in
    let rec loop ~path ~last_rebase ~events = function
      | [] -> (last_rebase, path, Base.List.rev events)
      | Worktree_plan.Ensure_worktree :: rest -> (
          match WS.ensure_worktree ~patch_id ~agent () with
          | Some p -> loop ~path:p ~last_rebase ~events rest
          | None ->
              Runtime_logging.log_event Env.runtime ~patch_id
                (Printf.sprintf
                   "Cannot %s — worktree missing and could not be created"
                   fail_label);
              ( Worktree.Error
                  (Printf.sprintf "%s failed: worktree missing" fail_label),
                path,
                Base.List.rev events ))
      | Worktree_plan.Fetch_origin :: rest -> (
          match W.fetch_origin ~fetch_lock ~path with
          | Result.Ok () -> loop ~path ~last_rebase ~events rest
          | Result.Error msg ->
              Runtime_logging.log_event Env.runtime ~patch_id
                (Printf.sprintf "Fetch failed before %s — %s" fail_label msg);
              ( Worktree.Error
                  (Printf.sprintf "fetch before rebase failed: %s" msg),
                path,
                Base.List.rev events ))
      | Worktree_plan.Capture_anchor { ref_name; slot } :: rest ->
          let sha = W.read_branch_sha ~path ~ref_name in
          set_slot slot sha;
          loop ~path ~last_rebase ~events rest
      | Worktree_plan.Rebase_onto target :: rest -> (
          (* Compute the upstream argument via Rebase_decision.plan, using
             the agent's recorded anchor history and a live HEAD-sha
             observation. The cherry-pick / patch-id detection inside
             W.rebase_onto remains as defense-in-depth; this resolved
             [upstream] is used only on its fallback path. *)
          let head_sha = W.read_branch_sha ~path ~ref_name:"HEAD" in
          let plan_input : Rebase_decision.input =
            {
              anchor = Anchor_history.newest (Patch_agent.anchor_history agent);
              recorded_history =
                Anchor_history.to_list (Patch_agent.anchor_history agent);
              base_branch =
                Base.Option.value agent.Patch_agent.base_branch
                  ~default:
                    (let target_s = Types.Branch.to_string target in
                     Types.Branch.of_string
                       (Base.Option.value
                          (Base.String.chop_prefix target_s ~prefix:"origin/")
                          ~default:target_s));
              head_sha;
            }
          in
          let ancestor_oracle ancestor ~descendant =
            W.is_ancestor ~path ~ancestor ~descendant
          in
          let plan = Rebase_decision.plan plan_input ~ancestor_oracle in
          let upstream =
            match plan with
            | Rebase_decision.Onto { upstream; _ } -> upstream
            | Rebase_decision.Plain _ -> Types.Branch.to_string target
          in
          match
            W.rebase_onto ~path ~target ~upstream ~project_name:Env.project_name
              ~ancestor_ids ()
          with
          | (Worktree.Ok | Worktree.Noop) as r ->
              loop ~path ~last_rebase:r ~events rest
          | (Worktree.Conflict _ | Worktree.Error _) as r ->
              (r, path, Base.List.rev events))
      | Worktree_plan.Record_anchor_on_success { slot; base } :: rest ->
          let events' =
            match last_rebase with
            | Worktree.Conflict _ | Worktree.Error _ -> events
            | Worktree.Ok | Worktree.Noop -> (
                match get_slot slot with
                | None -> Worktree_plan.Anchor_capture_failed :: events
                | Some sha -> (
                    match Anchor.make ~base ~sha ~observed_at_remote:true with
                    | Some a -> Worktree_plan.Anchor_recorded a :: events
                    | None -> Worktree_plan.Anchor_capture_failed :: events))
          in
          loop ~path ~last_rebase ~events:events' rest
    in
    loop ~path:default_path ~last_rebase:Worktree.Ok ~events:[] plan
end
