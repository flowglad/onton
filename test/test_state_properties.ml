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

(* [update_comments] threads [f] over the comments context: marking a generated
   comment pending must make [is_pending] observe the same boolean. *)
let update_comments_round_trips_pending =
  QCheck2.Test.make ~name:"update_comments threads set_pending through state"
    ~count:200
    QCheck2.Gen.(pair (int_range 1 1000) bool)
    (fun (cid, value) ->
      let patch_id = Types.Patch_id.of_string "p" in
      let comment =
        {
          Types.Comment.id = Types.Comment_id.of_int cid;
          thread_id = None;
          body = "b";
          path = None;
          line = None;
          commit_sha = None;
          original_commit_sha = None;
          outdated = false;
          last_reply_author = None;
        }
      in
      let state =
        State.update_comments State.empty ~f:(fun comments ->
            State.Comments.set_pending comments ~comment ~patch_id ~value)
      in
      let open State in
      Stdlib.( = )
        (State.Comments.is_pending state.comments ~comment ~patch_id)
        value)

let state_public_surface_is_linked =
  QCheck2.Test.make ~name:"state public surface is linked" QCheck2.Gen.unit
    (fun () ->
      ignore State.update_comments;
      true)

let () =
  QCheck2.Test.check_exn state_updates_are_composable;
  QCheck2.Test.check_exn update_comments_round_trips_pending;
  QCheck2.Test.check_exn state_public_surface_is_linked
