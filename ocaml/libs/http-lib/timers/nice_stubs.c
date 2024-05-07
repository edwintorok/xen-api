#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <errno.h>

CAMLprim value ml_nice(value delta)
{
  CAMLparam1(delta);
  /* see manpage, a successful nice can legitimately return -1. */
  errno = 0;
  int rc = nice(Int_val(delta));
  if (-1 == rc && errno)
    uerror("nice", Nothing);
  CAMLreturn(Val_int(rc));
}
