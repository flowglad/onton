(* @archlint.module core
   @archlint.domain session-driver *)

type response_transport = File_artifacts | Pi_tools

let response_transport ~backend_name =
  if String.equal (String.lowercase_ascii backend_name) "pi" then Pi_tools
  else File_artifacts

let render_opt_out ~response_transport ~artifact_path =
  let response =
    match response_transport with
    | File_artifacts -> Printf.sprintf "write your reason to `%s`" artifact_path
    | Pi_tools ->
        "call `onton_respond` with `kind: \"opt_out\"` and your reason as \
         `content`"
  in
  Printf.sprintf
    "\n\n\
     Before making your first commit, you may opt out of this patch. If you \
     decide the work should not proceed, %s and end your turn without \
     committing. The supervisor will surface that message and stop this patch \
     without pushing or opening a PR. This option is unavailable after any \
     patch commit or PR exists."
    response

type t = { context : string; turn : string }

let create ~context ~turn = { context; turn }

let render ~resume_session t =
  match resume_session with None -> t.context ^ t.turn | Some _ -> t.turn

type patch_revision = Absent | Revision of string | Unavailable

type recovery_context = {
  requirements : string;
  operation : string;
  guidance : string list;
  patch_revision : patch_revision;
  base_revision : string;
}

let create_recovery ~requirements ~operation ~guidance ~patch_revision
    ~base_revision =
  { requirements; operation; guidance; patch_revision; base_revision }

let render_recovery context =
  let section label value =
    Printf.sprintf "%s (%d bytes):\n%s\n" label (String.length value) value
  in
  let guidance =
    context.guidance
    |> List.mapi (fun index text ->
        section (Printf.sprintf "Guidance %d" (index + 1)) text)
    |> String.concat ""
  in
  "The previous workspace and conversation are unavailable. Treat local "
  ^ "session IDs, candidate commits, sequencer state, and completion claims "
  ^ "from that workspace as unavailable. Inspect the current checkout and "
  ^ "remote history before editing. Validate the resulting work and report "
  ^ "what you observed.\n"
  ^ section "Patch requirements" context.requirements
  ^ section "Outstanding operation" context.operation
  ^ section "Current guidance" guidance
  ^ section "Observed patch revision"
      (match context.patch_revision with
      | Absent -> "absent"
      | Revision revision -> revision
      | Unavailable -> "unavailable")
  ^ section "Observed base revision" context.base_revision
