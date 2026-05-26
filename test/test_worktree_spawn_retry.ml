open Base
open Onton

(** Unit tests for {!Worktree.retry_transient_spawn} /
    {!Worktree.is_transient_spawn_failure} — the bounded retry that absorbs
    transient [posix_spawn] failures (e.g. EAGAIN under process-table pressure)
    so a one-off spawn failure isn't mistaken for a git verdict or crash an op.

    The freshly-spawned-git integration tests
    ([test_worktree_start_point_integration], [test_push_plan_integration])
    flaked under a heavily parallel runner when a git spawn transiently failed;
    these tests lock in the retry policy deterministically without depending on
    real process pressure. *)

let failures = ref 0
let total = ref 0

let check name cond =
  Int.incr total;
  if cond then Stdlib.Printf.printf "  ok: %s\n" name
  else (
    Int.incr failures;
    Stdlib.Printf.printf "  FAIL: %s\n" name)

(* A genuine git verdict (process ran, exited non-zero) — must never be retried,
   or [ref_exists]-style callers would retry every "ref not found". *)
let child_error () =
  Eio.Exn.create (Eio.Process.E (Eio.Process.Child_error (`Exited 1)))

let test_classification () =
  check "Failure is transient"
    (Worktree.is_transient_spawn_failure (Failure "spawn: resource unavailable"));
  check "Unix_error EAGAIN is transient"
    (Worktree.is_transient_spawn_failure
       (Unix.Unix_error (Unix.EAGAIN, "fork", "")));
  check "cancellation is NOT transient"
    (not
       (Worktree.is_transient_spawn_failure
          (Eio.Cancel.Cancelled (Failure "cancelled"))));
  check "Child_error (git verdict) is NOT transient"
    (not (Worktree.is_transient_spawn_failure (child_error ())))

let test_retry_then_success () =
  let attempts = ref 0 in
  let r =
    Worktree.retry_transient_spawn ~attempts:4 (fun () ->
        Int.incr attempts;
        if !attempts < 3 then failwith "spawn: resource temporarily unavailable"
        else 42)
  in
  check "transient failure retried to success" (r = 42 && !attempts = 3)

let test_verdict_not_retried () =
  let attempts = ref 0 in
  let err = child_error () in
  let reraised =
    try
      ignore
        (Worktree.retry_transient_spawn ~attempts:4 (fun () ->
             Int.incr attempts;
             raise err));
      false
    with e -> Poly.equal e err
  in
  check "verdict re-raised on first attempt (not retried)"
    (reraised && !attempts = 1)

let test_persistent_transient_exhausts () =
  let attempts = ref 0 in
  let raised =
    try
      ignore
        (Worktree.retry_transient_spawn ~attempts:3 (fun () ->
             Int.incr attempts;
             failwith "persistent EAGAIN"));
      false
    with
    | Failure _ -> true
    | _ -> false
  in
  check "persistent transient exhausts attempts then raises"
    (raised && !attempts = 3)

let () =
  Stdlib.print_endline "Worktree spawn-retry:";
  (* [retry_transient_spawn] yields between attempts, so run inside a scheduler. *)
  Eio_main.run (fun _env ->
      test_classification ();
      test_retry_then_success ();
      test_verdict_not_retried ();
      test_persistent_transient_exhausts ());
  if !failures > 0 then (
    Stdlib.Printf.printf "%d/%d checks FAILED\n" !failures !total;
    Stdlib.exit 1)
  else Stdlib.Printf.printf "all %d checks passed\n" !total
