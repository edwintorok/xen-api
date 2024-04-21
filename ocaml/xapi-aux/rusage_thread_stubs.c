#include <bits/time.h>
#include <time.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

/* declared as noalloc and returns unboxed float:
  - must not call any CAMLparam/CAMLreturn here
  - must not release the runtime lock!
*/
int64_t caml_getrusage_thread_ns_unboxed(value unit) {
  struct timespec ts;
  if (clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts) < 0)
    uerror("clock_gettime", Nothing);

  return (int64_t)ts.tv_nsec + (int64_t)ts.tv_sec * (int64_t)1000000000;
}

/* for bytecode compatibility: have to box the result */
CAMLprim value caml_getrusage_thread_ns(value unit) {
  return caml_copy_int64(caml_getrusage_thread_ns_unboxed(unit));
}
