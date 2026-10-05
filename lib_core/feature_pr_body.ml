(* @archlint.module core
   @archlint.domain execution-mode *)
open Base
open Types

let patch_heading (patch : Patch.t) =
  "### Patch " ^ Patch_id.to_string patch.id ^ ": " ^ patch.title ^ "\n\n"

let section title content =
  if String.is_empty (String.strip content) then ""
  else "## " ^ title ^ "\n\n" ^ content ^ "\n\n"

let render ~(gameplan : Gameplan.t) ~(patches : Patch.t list) ~notes =
  let changes =
    List.filter_map patches ~f:(fun patch ->
        let bullets =
          List.map patch.Patch.changes ~f:(fun s -> "- " ^ s)
          |> String.concat ~sep:"\n"
        in
        let content =
          String.concat ~sep:"\n\n"
            (List.filter [ patch.description; bullets ] ~f:(fun s ->
                 not (String.is_empty (String.strip s))))
        in
        if String.is_empty content then None
        else Some (patch_heading patch ^ content))
    |> String.concat ~sep:"\n\n"
  in
  let specs =
    List.filter_map patches ~f:(fun patch ->
        if String.is_empty (String.strip patch.Patch.spec) then None
        else Some (patch_heading patch ^ "```\n" ^ patch.spec ^ "\n```"))
    |> String.concat ~sep:"\n\n"
  in
  let implementation_notes =
    List.filter_map patches ~f:(fun patch ->
        match List.Assoc.find notes patch.Patch.id ~equal:Patch_id.equal with
        | Some content when not (String.is_empty (String.strip content)) ->
            Some (patch_heading patch ^ String.strip content)
        | Some _ | None -> None)
    |> String.concat ~sep:"\n\n"
  in
  section "Summary"
    (String.concat ~sep:"\n\n"
       (List.filter [ gameplan.problem_statement; gameplan.solution_summary ]
          ~f:(fun s -> not (String.is_empty (String.strip s)))))
  ^ section "Changes" changes
  ^ section "Gameplan Specification"
      (if String.is_empty (String.strip gameplan.final_state_spec) then ""
       else "```\n" ^ gameplan.final_state_spec ^ "\n```")
  ^ section "Patch Specifications" specs
  ^ section "Implementation Notes" implementation_notes
