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
  let rec gather guidance evidence actions covered base latest = function
    | [] ->
        Option.map latest ~f:(fun (input_id, version) ->
            {
              V.input_id;
              version;
              guidance = String.concat ~sep:"\n\n" (List.rev guidance);
              evidence = List.rev evidence;
              controls = List.rev actions;
              covered = List.rev covered;
              selected_base = base;
            })
    | (id, _, version, payload, _) :: rest ->
        let guidance, evidence, actions, base =
          match
            List.find controls ~f:(fun c ->
                String.equal c.Coordinator_state.id id)
          with
          | Some c when String.equal c.kind "guidance" ->
              (payload :: guidance, evidence, actions, base)
          | Some c
            when List.mem [ "automerge"; "bump" ] c.kind ~equal:String.equal ->
              (guidance, evidence, (c.kind, c.payload) :: actions, base)
          | _ -> (
              match Coordinator_observation.decode payload with
              | Ok observation ->
                  (guidance, observation :: evidence, actions, base)
              | Error _ -> (
                  match V.decode_host payload with
                  | Ok (V.Input (_, input)) ->
                      ( input.guidance :: guidance,
                        List.rev_append input.evidence evidence,
                        List.rev_append input.controls actions,
                        input.selected_base )
                  | _ -> (payload :: guidance, evidence, actions, base)))
        in
        gather guidance evidence actions ((id, version) :: covered) base
          (Some (id, version))
          rest
  in
  gather [] [] [] [] selected_base None inputs
