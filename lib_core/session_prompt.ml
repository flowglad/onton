(* @archlint.module core
   @archlint.domain session-driver *)

type t = { context : string; turn : string }

let create ~context ~turn = { context; turn }

let render ~resume_session t =
  match resume_session with None -> t.context ^ t.turn | Some _ -> t.turn

type recovery_context = {
  requirements : string;
  operation : string;
  guidance : string list;
  patch_revision : string option;
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
      (Option.value context.patch_revision ~default:"absent")
  ^ section "Observed base revision" context.base_revision
