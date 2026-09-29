#define _GNU_SOURCE
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

/* Return -1 on any unsupported platform or credential lookup failure. The
 * control server treats that result as unauthorized. */
CAMLprim value caml_onton_control_peer_uid(value v_fd) {
  CAMLparam1(v_fd);
  int fd = Int_val(v_fd);
#if defined(__APPLE__) || defined(__FreeBSD__)
  uid_t uid;
  gid_t gid;
  if (getpeereid(fd, &uid, &gid) == 0) CAMLreturn(Val_int(uid));
#elif defined(__linux__)
  struct ucred cred;
  socklen_t length = sizeof(cred);
  if (getsockopt(fd, SOL_SOCKET, SO_PEERCRED, &cred, &length) == 0 &&
      length == sizeof(cred)) CAMLreturn(Val_int(cred.uid));
#endif
  CAMLreturn(Val_int(-1));
}
