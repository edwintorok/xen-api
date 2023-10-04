#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <time.h>

#ifdef __linux__
#define FAST_CLOCK CLOCK_MONOTONIC_COARSE
#else
#define FAST_CLOCK CLOCK_MONOTONIC
#endif

#define NS_PER_S 1000000000

#ifdef __GNUC__
#if defined(__i386__) || defined(__x86_64__)
/* safe to use without CPUID checks: just a specially encoded NOP for CPUs that don't support it */
#define cpu_relax() __builtin_ia32_pause()
#else
#define cpu_relax()
#endif
#endif

CAMLprim value caml_bench_concurrent_parallel_c_work(value ms)
{
  struct timespec t, deadline;
  int ret;
  CAMLparam1(ms);
  caml_release_runtime_system();
  do {
    if ((ret = clock_gettime(FAST_CLOCK, &t)))
      break;
    deadline.tv_nsec = t.tv_nsec + Long_val(ms) * 1000000;
    deadline.tv_sec = t.tv_sec + deadline.tv_nsec / NS_PER_S;
    deadline.tv_nsec = deadline.tv_nsec % NS_PER_S;

    do {
      if ((ret = clock_gettime(FAST_CLOCK, &t)))
        break;
      cpu_relax();
    } while(t.tv_sec < deadline.tv_sec || (t.tv_sec == deadline.tv_sec && t.tv_nsec < deadline.tv_nsec));
  } while(0);
  caml_acquire_runtime_system();
  if (ret)
    uerror("clock_gettime", Nothing);
  CAMLreturn(Val_unit);  
}
