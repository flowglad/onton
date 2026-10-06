(* @archlint.module core
   @archlint.domain execution-mode *)
open Base
open Types

type contribution = { changes : string; spec : string; notes : string }

let patch_heading (patch : Patch.t) =
  "### Patch " ^ Patch_id.to_string patch.id ^ ": " ^ patch.title ^ "\n\n"

let section title content =
  if String.is_empty (String.strip content) then ""
  else "## " ^ title ^ "\n\n" ^ content ^ "\n\n"

let render_contribution ~(patch : Patch.t) ~notes =
  let content =
    match patch.changes with
    | [] -> patch.description
    | changes ->
        List.filter_map changes ~f:(fun change ->
            let change = String.strip change in
            if String.is_empty change then None else Some ("- " ^ change))
        |> String.concat ~sep:"\n"
  in
  let changes =
    if String.is_empty (String.strip content) then ""
    else patch_heading patch ^ content
  in
  let spec =
    if String.is_empty (String.strip patch.spec) then ""
    else patch_heading patch ^ "```\n" ^ patch.spec ^ "\n```"
  in
  let notes =
    match notes with
    | Some content when not (String.is_empty (String.strip content)) ->
        patch_heading patch ^ String.strip content
    | Some _ | None -> ""
  in
  { changes; spec; notes }

let render_contributions ~(gameplan : Gameplan.t) contributions =
  let collect f =
    List.filter_map contributions ~f:(fun contribution ->
        let content = f contribution in
        if String.is_empty content then None else Some content)
    |> String.concat ~sep:"\n\n"
  in
  section "Summary"
    (String.concat ~sep:"\n\n"
       (List.filter [ gameplan.problem_statement; gameplan.solution_summary ]
          ~f:(fun s -> not (String.is_empty (String.strip s)))))
  ^ section "Architectural Design" gameplan.architecture_design
  ^ section "Changes" (collect (fun c -> c.changes))
  ^ section "Gameplan Specification"
      (if String.is_empty (String.strip gameplan.final_state_spec) then ""
       else "```\n" ^ gameplan.final_state_spec ^ "\n```")
  ^ section "Patch Specifications" (collect (fun c -> c.spec))
  ^ section "Implementation Notes" (collect (fun c -> c.notes))

let render ~gameplan ~patches ~notes =
  render_contributions ~gameplan
    (List.map patches ~f:(fun patch ->
         render_contribution ~patch
           ~notes:(List.Assoc.find notes patch.Patch.id ~equal:Patch_id.equal)))
