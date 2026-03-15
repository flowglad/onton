open Base
open Types

type violation = { invariant : string; details : string }
[@@deriving show, eq, sexp_of]

let patch_ctx (state : State.t) = state.State.patch_ctx

let check_ci_failure_count_non_negative (state : State.t) =
  State.Patch_ctx.known_patch_ids (patch_ctx state)
  |> List.filter_map ~f:(fun patch_id ->
      let count =
        State.Patch_ctx.ci_failure_count (patch_ctx state) ~patch_id
      in
      if count < 0 then
        Some
          {
            invariant = "ci_failure_count_non_negative";
            details =
              Printf.sprintf "patch %d has ci_failure_count=%d"
                (Patch_id.to_int patch_id) count;
          }
      else None)

let check_resolved_not_pending (state : State.t) =
  (* A resolved comment should not be pending for any patch *)
  let patch_ids = State.Patch_ctx.known_patch_ids (patch_ctx state) in
  (* We can only check comments we know about via pending entries.
     If a comment is resolved, it must not be pending for any patch. *)
  (* This is a structural invariant — enforced by callers, checked here. *)
  (* Without iterating all comments (no iterator on Comments.t), we rely
     on the invariant being checked when mutations happen. For now,
     we return an empty list as we cannot enumerate comments from the
     opaque Comments.t type. A future patch will add comment enumeration. *)
  ignore (patch_ids : Patch_id.t list);
  []

let check_busy_implies_has_session (state : State.t) =
  State.Patch_ctx.known_patch_ids (patch_ctx state)
  |> List.filter_map ~f:(fun patch_id ->
      let busy = State.Patch_ctx.is_busy (patch_ctx state) ~patch_id in
      let has_session =
        State.Patch_ctx.has_session (patch_ctx state) ~patch_id
      in
      if busy && not has_session then
        Some
          {
            invariant = "busy_implies_has_session";
            details =
              Printf.sprintf "patch %d is busy but has no session"
                (Patch_id.to_int patch_id);
          }
      else None)

let all_checks =
  [
    check_ci_failure_count_non_negative;
    check_busy_implies_has_session;
    check_resolved_not_pending;
  ]

let check_invariants (state : State.t) : violation list =
  List.concat_map all_checks ~f:(fun check -> check state)

let check_invariants_exn (state : State.t) : unit =
  match check_invariants state with
  | [] -> ()
  | violations ->
      let msg =
        List.map violations ~f:(fun v ->
            Printf.sprintf "[%s] %s" v.invariant v.details)
        |> String.concat ~sep:"; "
      in
      failwith (Printf.sprintf "Invariant violations: %s" msg)

let should_check_at_runtime () =
  match Stdlib.Sys.getenv_opt "ONTON_CHECK_INVARIANTS" with
  | Some "1" | Some "true" -> true
  | _ -> false

let maybe_check_invariants_exn (state : State.t) : unit =
  if should_check_at_runtime () then check_invariants_exn state
