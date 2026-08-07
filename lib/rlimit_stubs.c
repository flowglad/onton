/* POSIX getrlimit/setrlimit bindings for RLIMIT_NOFILE only.
 *
 * We care about the file-descriptor limit because onton opens many pipes
 * and sockets (per-LLM-subprocess pipes, per-git-subprocess pipes, GitHub
 * HTTPS connections, the activity log, snapshot files, the project lock).
 * A low soft limit surfaces as "too many open files" mid-run, long after
 * enough state is committed to make recovery painful. Checking at startup
 * lets us fail fast with an actionable message instead.
 *
 * See issue #209. */

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <sys/resource.h>
#include <errno.h>
#include <string.h>

/* Returns (soft, hard) as an OCaml int pair. */
CAMLprim value caml_onton_getrlimit_nofile(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(pair);
  struct rlimit rl;
  if (getrlimit(RLIMIT_NOFILE, &rl) != 0) {
    caml_failwith(strerror(errno));
  }
  pair = caml_alloc_tuple(2);
  /* RLIM_INFINITY may exceed OCaml's int range on some platforms; clamp
   * to max_int. For RLIMIT_NOFILE this is defensive — no real system ships
   * with more than a few million allowed FDs. */
  long soft = (rl.rlim_cur > (rlim_t)Max_long) ? Max_long : (long)rl.rlim_cur;
  long hard = (rl.rlim_max > (rlim_t)Max_long) ? Max_long : (long)rl.rlim_max;
  Store_field(pair, 0, Val_long(soft));
  Store_field(pair, 1, Val_long(hard));
  CAMLreturn(pair);
}

/* Raises only the soft RLIMIT_NOFILE to [v_soft]; re-reads the kernel's
 * current hard limit so we preserve RLIM_INFINITY instead of sending back
 * the Max_long-clamped value from the getter (which setrlimit would treat
 * as a hard-limit lowering, rejected EPERM on some platforms).
 * Raises Failure on error. */
CAMLprim value caml_onton_setrlimit_nofile_soft(value v_soft) {
  CAMLparam1(v_soft);
  struct rlimit rl;
  if (getrlimit(RLIMIT_NOFILE, &rl) != 0) {
    caml_failwith(strerror(errno));
  }
  rl.rlim_cur = (rlim_t)Long_val(v_soft);
  if (setrlimit(RLIMIT_NOFILE, &rl) != 0) {
    caml_failwith(strerror(errno));
  }
  CAMLreturn(Val_unit);
}
