(** Tests for [Onton.Rlimit]: POSIX RLIMIT_NOFILE bindings.

    We exercise [get_nofile] and [try_raise_nofile_soft] in-process; we do not
    test the preflight integration in [bin/main.ml] because that would require
    spawning the full [onton] binary under different ulimit values. *)

open Onton.Rlimit

let fail_if cond msg = if cond then failwith msg

let test_get_returns_sane_limits () =
  let lim = get_nofile () in
  fail_if (lim.soft <= 0) (Printf.sprintf "soft=%d must be > 0" lim.soft);
  fail_if (lim.hard < lim.soft)
    (Printf.sprintf "hard=%d must be ≥ soft=%d" lim.hard lim.soft)

let test_try_raise_is_capped_at_hard () =
  (* Ask for something far beyond the hard cap. Must not raise, and must
     return a soft that does not exceed hard. *)
  let before = get_nofile () in
  let after = try_raise_nofile_soft ~target:((before.hard * 2) + 1_000_000) in
  fail_if (after.soft > after.hard)
    (Printf.sprintf "soft=%d must be ≤ hard=%d after raise" after.soft
       after.hard)

let test_try_raise_noop_when_already_sufficient () =
  let before = get_nofile () in
  (* Asking for the current soft or lower must not reduce it — the function
     promises to *raise*, not to lower. *)
  let after = try_raise_nofile_soft ~target:1 in
  fail_if (after.soft < before.soft)
    (Printf.sprintf "soft unexpectedly reduced %d → %d" before.soft after.soft)

let () =
  test_get_returns_sane_limits ();
  test_try_raise_is_capped_at_hard ();
  test_try_raise_noop_when_already_sufficient ();
  print_endline "test_rlimit: OK"
