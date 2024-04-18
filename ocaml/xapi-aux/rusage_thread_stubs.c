/* must be first according to getrusage(2) */
#define _GNU_SOURCE
#include <sys/resource.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

/* declared as noalloc and returns unboxed float,
   must not call any CAMLparam/CAMLreturn here!
 */

double caml_getrusage_thread_utime_unboxed(value unit)
{
  struct rusage rusage;
  const struct timeval *tv;

  if (getrusage(RUSAGE_THREAD, &rusage) < 0)
    uerror("getrusage", Nothing);
  tv = &rusage.ru_utime;
  return (tv->tv_sec + tv->tv_usec / 1e6);
}

CAMLprim value caml_getrusage_thread_utime(value unit)
{
  return caml_copy_double(caml_getrusage_thread_utime_unboxed(unit));
}
