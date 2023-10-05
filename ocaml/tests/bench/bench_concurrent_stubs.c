#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>

/* perform work proportional to argument,
   no syscalls, no instructions that could be intercepted by a hypervisor
   (like rdtsc or pause).
 */
static uint32_t fixed_work(long work)
{
  long i;
  uint64_t state = 0;
  /* see https://www.pcg-random.org/pdf/hmc-cs-2014-0905.pdf 6.3.2 */
  for (i=0;i<work;i++)
    state = state * 6364136223846793005ULL + 1;
  return (state ^ (state >> 22)) >> (22 + (state >> 61));
}

CAMLprim value caml_bench_fixed_work(value work)
{
  CAMLparam1(work);
  uint32_t output;

  caml_release_runtime_system();
  output = fixed_work(Long_val(work));
  caml_acquire_runtime_system();

  CAMLreturn(Val_long(output));
}