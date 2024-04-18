#include <bits/time.h>
#include <time.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

/* declared as noalloc and returns unboxed float,
   must not call any CAMLparam/CAMLreturn here!
   We must also not release the runtime lock!
 */

double caml_getrusage_thread_unboxed(value unit)
{
  struct timespec ts;
  if (clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts) < 0)
    uerror("clock_gettime", Nothing);

  return 1e-9 * ts.tv_nsec + ts.tv_sec;
}

CAMLprim value caml_getrusage_thread(value unit)
{
  return caml_copy_double(caml_getrusage_thread_unboxed(unit));
}
