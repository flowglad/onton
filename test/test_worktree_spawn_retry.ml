(* @archlint.module test
   @archlint.domain session-meta *)

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

(* Exercise the real runner with faults at the public process-manager boundary,
   so the tests verify supervisor creation rather than just the retry helper. *)
let faulting_mgr (type tag) (mgr : tag Eio.Process.mgr_ty Eio.Resource.t)
    on_spawn =
  let (Eio.Resource.T (state, ops)) = mgr in
  let module Original = (val Eio.Resource.get ops Eio.Process.Pi.Mgr) in
  let module Faults = struct
    include Original

    let spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
      on_spawn ();
      Original.spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args
  end in
  Eio.Resource.T (state, Eio.Process.Pi.mgr (module Faults))

let test_supervisor_retry env =
  let attempts = ref 0 in
  let mgr =
    faulting_mgr (Eio.Stdenv.process_mgr env) (fun () ->
        Int.incr attempts;
        if !attempts < 3 then raise (Unix.Unix_error (Unix.EAGAIN, "spawn", "")))
  in
  let file = Stdlib.Filename.temp_file "onton-spawn-retry-" "" in
  Stdlib.Fun.protect
    ~finally:(fun () -> Unix.unlink file)
    (fun () ->
      let code, out, err =
        Process_tree.run ~process_mgr:mgr ~clock:(Eio.Stdenv.clock env)
          ~env:(Unix.environment ())
          [
            "sh";
            "-c";
            "printf started >> \"$1\"; printf captured; printf diagnostic >&2; \
             exit 7";
            "sh";
            file;
          ]
      in
      let executions =
        Stdlib.In_channel.with_open_bin file Stdlib.In_channel.input_all
      in
      check "supervisor retries EAGAIN to successful spawn" (!attempts = 3);
      check "started command with nonzero exit runs exactly once"
        (code = 7 && String.equal executions "started");
      check "retried supervisor preserves stdout and stderr"
        (String.equal out "captured" && String.equal err "diagnostic"))

let test_supervisor_spawn_failures env =
  let run on_spawn =
    Process_tree.run
      ~process_mgr:(faulting_mgr (Eio.Stdenv.process_mgr env) on_spawn)
      ~clock:(Eio.Stdenv.clock env) ~env:(Unix.environment ()) [ "true" ]
  in
  let attempts = ref 0 in
  let exhausted =
    try
      ignore
        (run (fun () ->
             Int.incr attempts;
             raise (Unix.Unix_error (Unix.EAGAIN, "spawn", ""))));
      false
    with Unix.Unix_error (Unix.EAGAIN, _, _) -> true
  in
  check "supervisor persistent EAGAIN exhausts four attempts"
    (exhausted && !attempts = 4);
  attempts := 0;
  let permanent =
    try
      ignore
        (run (fun () ->
             Int.incr attempts;
             raise
               (Eio.Exn.create
                  (Eio.Process.E (Eio.Process.Executable_not_found "shim")))));
      false
    with Eio.Io (Eio.Process.E (Eio.Process.Executable_not_found _), _) ->
      true
  in
  check "supervisor permanent spawn error is not retried"
    (permanent && !attempts = 1);
  attempts := 0;
  let cancelled =
    Eio.Cancel.sub (fun caller ->
        try
          ignore
            (run (fun () ->
                 Int.incr attempts;
                 Eio.Cancel.cancel caller (Failure "cancel during retry");
                 raise (Unix.Unix_error (Unix.EAGAIN, "spawn", ""))));
          false
        with exn when Worktree.has_cancellation exn -> true)
  in
  check "cancellation between retries prevents another supervisor spawn"
    (cancelled && !attempts = 1)

let () =
  Stdlib.print_endline "Worktree spawn-retry:";
  (* [retry_transient_spawn] yields between attempts, so run inside a scheduler. *)
  Eio_main.run (fun env ->
      test_classification ();
      test_retry_then_success ();
      test_verdict_not_retried ();
      test_persistent_transient_exhausts ();
      test_supervisor_retry env;
      test_supervisor_spawn_failures env);
  if !failures > 0 then (
    Stdlib.Printf.printf "%d/%d checks FAILED\n" !failures !total;
    Stdlib.exit 1)
  else Stdlib.Printf.printf "all %d checks passed\n" !total
