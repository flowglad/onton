#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/prctl.h>
#include <stdio.h>
#include <signal.h>
#elif defined(__APPLE__)
#include <sys/sysctl.h>
#else
#error "Process-tree supervision requires Linux or macOS"
#endif

CAMLprim value caml_onton_become_subreaper(value unit) {
  (void)unit;
#ifdef __linux__
  if (prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) != 0) {
    caml_failwith(strerror(errno));
  }
#endif
  return Val_unit;
}

/* Observe completion without freeing the leader PID. All group operations must
   finish before waitpid consumes it, otherwise the group ID can be recycled. */
CAMLprim value caml_onton_leader_exited(value leader) {
  siginfo_t info;
  int result;
  do {
    memset(&info, 0, sizeof(info));
    result = waitid(P_PID, Int_val(leader), &info,
                    WEXITED | WNOHANG | WNOWAIT);
  } while (result < 0 && errno == EINTR);
  if (result < 0) caml_failwith(strerror(errno));
  return Val_bool(info.si_pid != 0);
}

/* Reap adopted descendants, never the leader. On macOS these are usually
   launchd's children: ECHILD means wait for launchd, not cleanup completed. */
#ifdef __APPLE__
static int member_remaining(pid_t member) {
  int status;
  pid_t result;
  do {
    result = waitpid(member, &status, WNOHANG);
  } while (result < 0 && errno == EINTR);
  if (result == member) return 0;
  if (result == 0 || errno == ECHILD) return 1;
  return -1;
}

#endif

CAMLprim value caml_onton_descendants_remaining(value leader, value terminate) {
  pid_t pgid = Int_val(leader);
  int remaining = 0, error = 0;
#ifdef __APPLE__
  (void)terminate;
  int mib[] = { CTL_KERN, KERN_PROC, KERN_PROC_PGRP, pgid };
  struct kinfo_proc *members = NULL;
  size_t bytes;
  for (;;) {
    bytes = 0;
    if (sysctl(mib, 4, NULL, &bytes, NULL, 0) < 0) {
      error = errno;
      break;
    }
    /* The group may grow between the sizing and reading calls. Retry ENOMEM. */
    bytes += 16 * sizeof(*members);
    members = malloc(bytes);
    if (members == NULL) { error = ENOMEM; break; }
    if (sysctl(mib, 4, members, &bytes, NULL, 0) == 0) {
      for (size_t i = 0; i < bytes / sizeof(*members); ++i) {
        pid_t member = members[i].kp_proc.p_pid;
        if (member == pgid) continue;
        int present = member_remaining(member);
        if (present < 0) { error = errno; break; }
        remaining |= present;
      }
      free(members);
      break;
    }
    error = errno;
    free(members);
    if (error != ENOMEM) break;
    error = 0;
  }
#else
  /* After the leader exits, every surviving tree has a child owned by this
     subreaper at its root. Session/group changes do not change that ownership.
     This is a single-threaded supervisor: a live child remains PID-pinned until
     we reap it, so signaling after waitpid(WNOHANG) cannot target a reused PID.
     Even a reaped child requires another pass: its children may have just been
     adopted, including children created while this snapshot was read. */
  char path[96];
  snprintf(path, sizeof(path), "/proc/self/task/%ld/children", (long)getpid());
  FILE *children = fopen(path, "r");
  if (children == NULL) caml_failwith(strerror(errno));
  long number;
  int scanned;
  while ((scanned = fscanf(children, "%ld", &number)) == 1) {
    pid_t member = (pid_t)number;
    if (member == pgid) continue;
    remaining = 1;
    int status;
    pid_t result;
    do {
      result = waitpid(member, &status, WNOHANG);
    } while (result < 0 && errno == EINTR);
    if (result < 0) { error = errno; break; }
    if (result == 0 && Bool_val(terminate) && kill(member, SIGKILL) < 0
        && errno != ESRCH) { error = errno; break; }
  }
  if (error == 0 && (ferror(children) || scanned != EOF)) error = EIO;
  fclose(children);
#endif
  if (error != 0) caml_failwith(strerror(error));
  return Val_bool(remaining);
}
