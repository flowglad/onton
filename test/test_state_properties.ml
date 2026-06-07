(* @archlint.module test
   @archlint.domain state *)

open Onton_core

let state_updates_are_composable =
  QCheck2.Test.make ~name:"state update helpers preserve untouched fields"
    ~count:200 QCheck2.Gen.bool (fun value ->
      let patch_id = Types.Patch_id.of_string "p" in
      let state =
        State.empty
        |> State.update_patch_ctx ~f:(fun ctx ->
            State.Patch_ctx.set_has_pr ctx ~patch_id ~value)
      in
      let open State in
      State.Patch_ctx.has_pr state.patch_ctx ~patch_id = value
      && State.Comments.all_pending state.comments = [])

let state_public_surface_is_linked =
  QCheck2.Test.make ~name:"state public surface is linked" QCheck2.Gen.unit
    (fun () ->
      ignore State.update_comments;
      true)

let () =
  QCheck2.Test.check_exn state_updates_are_composable;
  QCheck2.Test.check_exn state_public_surface_is_linked
