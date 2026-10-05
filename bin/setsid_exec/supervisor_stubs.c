#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <errno.h>
#include <string.h>
#ifdef __linux__
#include <sys/prctl.h>
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
