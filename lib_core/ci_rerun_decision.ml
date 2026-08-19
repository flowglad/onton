(* @archlint.module core
   @archlint.domain ci-rerun *)

open Base

type workflow_status = Completed | Pending | Malformed [@@deriving show, eq]

let workflow_run_id_from_url url =
  let marker = "/actions/runs/" in
  match String.substr_index url ~pattern:marker with
  | None -> None
  | Some pos ->
      let start = pos + String.length marker in
      let rest = String.drop_prefix url start in
      let len =
        match String.findi rest ~f:(fun _ ch -> not (Char.is_digit ch)) with
        | None -> String.length rest
        | Some (idx, _) -> idx
      in
      let id = String.prefix rest len in
      if String.is_empty id then None else Int.of_string_opt id

let unique_workflow_checks checks =
  let _, rev_checks =
    List.fold checks
      ~init:(Set.empty (module Int), [])
      ~f:(fun (seen, acc) (check : Types.Ci_check.t) ->
        let run_id =
          Option.bind check.details_url ~f:workflow_run_id_from_url
        in
        match run_id with
        | Some id when Set.mem seen id -> (seen, acc)
        | Some id -> (Set.add seen id, check :: acc)
        | None -> (seen, check :: acc))
  in
  List.rev rev_checks

let workflow_status_of_response body =
  match try Some (Yojson.Safe.from_string body) with _ -> None with
  | None -> Malformed
  | Some json -> (
      match Json.string_field "status" json with
      | Some "completed" -> Completed
      | Some _ -> Pending
      | None -> Malformed)
