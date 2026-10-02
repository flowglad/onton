/* yaml 3.2 exposes parser deletion but omits event deletion from its bindings. */
#include <caml/mlvalues.h>
#include <caml/memory.h>

struct yaml_event_s;
extern void yaml_event_delete(struct yaml_event_s *event);

CAMLprim value onton_yaml_event_delete(value address)
{
  CAMLparam1(address);
  yaml_event_delete((struct yaml_event_s *)Nativeint_val(address));
  CAMLreturn(Val_unit);
}
