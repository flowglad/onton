(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)

let read (module Provider : Forge.S) (request : Forge_observation.request) =
  let outcome =
    match Provider.pr_state request.pr_number with
    | Ok state -> Poll_outcome.Ok_pr_state state
    | Error error -> Provider.poll_error error
  in
  Forge_observation.{ request; outcome }

type prepared = {
  patch_id : Types.Patch_id.t;
  requested : Patch_agent.t;
  ticket : Forge_observation.ticket;
}

let prepare ~runtime ~persist requests =
  if Base.List.is_empty requests then Ok []
  else
    Runtime.update_persisting runtime ~persist (fun snap ->
        let orchestrator, prepared =
          Base.List.fold requests ~init:(snap.Runtime.orchestrator, [])
            ~f:(fun (orch, prepared) (patch_id, requested, request) ->
              match Orchestrator.find_agent orch patch_id with
              | Some current when Poll_cycle.same_context ~requested ~current ->
                  let orch, ticket =
                    Orchestrator.begin_forge_observation orch patch_id ~request
                  in
                  let prepared =
                    match ticket with
                    | None -> prepared
                    | Some ticket ->
                        {
                          patch_id;
                          requested = Orchestrator.agent orch patch_id;
                          ticket;
                        }
                        :: prepared
                  in
                  (orch, prepared)
              | None | Some _ -> (orch, prepared))
        in
        ({ snap with Runtime.orchestrator }, Base.List.rev prepared))
