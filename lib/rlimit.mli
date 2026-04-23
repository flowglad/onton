(** Thin POSIX bindings for [RLIMIT_NOFILE] (per-process open-file cap).

    onton needs a generous FD limit because it opens many pipes (per-LLM and
    per-git subprocess), HTTPS sockets to GitHub, log and snapshot files, and
    the project lock. Too-low a soft limit surfaces as obscure "too many open
    files" failures mid-run — see issue #209. Preflight- check at startup and
    auto-raise to the hard cap where possible. *)

type limits = { soft : int; hard : int }

val get_nofile : unit -> limits
(** Current RLIMIT_NOFILE. Raises [Failure] on syscall error. *)

val try_raise_nofile_soft : target:int -> limits
(** Best-effort: attempt to raise soft to [min target hard]. Never raises — on
    failure, returns the (possibly unchanged) current limits so the caller can
    decide whether the effective value is sufficient. *)
