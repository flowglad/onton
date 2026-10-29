(* @archlint.module core
   @archlint.domain coordinator-state *)
open Base

[@@@warning "-4"]

module V = Worker_protocol_v3

let pending state patch ~selected_base =
  let patch_id = Types.Patch_id.to_string patch in
  let inputs =
    Coordinator_state.inputs state
    |> List.filter ~f:(fun (_, p, _, _, used) ->
        String.equal p patch_id && not used)
    |> List.sort ~compare:(fun (_, _, a, _, _) (_, _, b, _, _) ->
        Int64.compare a b)
  in
  let controls = Coordinator_state.controls state in
  let invalid () = Error "unknown or malformed pending coordinator input" in
  let rec gather guidance evidence actions covered base latest = function
    | [] ->
        Ok
          (Option.map latest ~f:(fun (input_id, version) ->
               {
                 V.input_id;
                 version;
                 guidance = String.concat ~sep:"\n\n" (List.rev guidance);
                 evidence = List.rev evidence;
                 controls = List.rev actions;
                 covered = List.rev covered;
                 selected_base = base;
               }))
    | (id, _, version, payload, _) :: rest -> (
        let next ?(nested_covered = []) guidance evidence actions base =
          let covered = List.rev_append nested_covered covered in
          gather guidance evidence actions ((id, version) :: covered) base
            (Some (id, version))
            rest
        in
        match
          List.find controls ~f:(fun c ->
              String.equal c.Coordinator_state.id id
              && String.equal c.run_id (Coordinator_state.run_id state)
              && String.equal c.config_id (Coordinator_state.config_id state)
              && Option.equal String.equal c.patch_id (Some patch_id)
              && String.equal c.payload payload)
        with
        | Some c when String.equal c.kind "guidance" ->
            next (payload :: guidance) evidence actions base
        | Some c
          when List.mem [ "automerge"; "bump" ] c.kind ~equal:String.equal ->
            next guidance evidence ((c.kind, c.payload) :: actions) base
        | Some _ -> invalid ()
        | None -> (
            match Coordinator_observation.decode payload with
            | Ok observation
              when String.equal id
                     ("evidence:"
                    ^ observation.Coordinator_observation.scope.ticket) ->
                next guidance (observation :: evidence) actions base
            | Ok _ -> invalid ()
            | Error _ -> (
                match V.decode_host payload with
                | Ok (V.Input (identity, input))
                  when String.equal identity.run_id
                         (Coordinator_state.run_id state)
                       && String.equal identity.patch_id patch_id
                       && String.equal input.input_id id
                       && Int64.equal input.version version ->
                    next ~nested_covered:input.covered
                      (input.guidance :: guidance)
                      (List.rev_append input.evidence evidence)
                      (List.rev_append input.controls actions)
                      input.selected_base
                | Ok _ | Error _ -> invalid ())))
  in
  gather [] [] [] [] selected_base None inputs
