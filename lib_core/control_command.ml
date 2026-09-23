(* @archlint.module core
   @archlint.domain control-command *)

(* Commands accepted by the task-local headless control socket. Each command
   has an id so callers can safely retry an absolute state change. *)
type t =
  | Set_automerge of {
      id : string;
      patch_id : Types.Patch_id.t;
      enabled : bool;
    }

let decode = function
  | `Assoc fields -> (
      let find name = List.assoc_opt name fields in
      match (find "id", find "type", find "payload") with
      | Some (`String id), Some (`String "set_automerge"), Some (`Assoc payload)
        when id <> "" -> (
          match
            (List.assoc_opt "patch_id" payload, List.assoc_opt "enabled" payload)
          with
          | Some (`String patch_id), Some (`Bool enabled) when patch_id <> "" ->
              Ok
                (Set_automerge
                   { id; patch_id = Types.Patch_id.of_string patch_id; enabled })
          | _ -> Error "invalid command")
      | _ -> Error "invalid command")
  | _ -> Error "invalid command"

let id = function Set_automerge { id; _ } -> id
