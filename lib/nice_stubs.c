#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <errno.h>

#include <string.h>
#include <signal.h>
#include <time.h>

CAMLprim value ml_nice(value n)
{
  CAMLparam1(n);
  /* see manpage, a successful nice can legitimately return -1. */
  errno = 0;
  int rc = nice(Int_val(n));
  if (rc == -1 && errno)
    uerror("nice", Nothing);
  CAMLreturn(Val_int(rc));
}

CAMLprim value ml_set_thread_timer(value interval)
{
  CAMLparam1(interval);
  struct sigevent sev;
  timer_t timerid;
  struct itimerspec spec;
  /* TODO: could use the type we already have in Unix for the itimerspec */
    
  spec.it_interval.tv_sec = 0;
  spec.it_interval.tv_nsec = Int64_val(interval);
  spec.it_value = spec.it_interval;

  memset(&sev, 0, sizeof(sev));
  sev.sigev_notify = SIGEV_SIGNAL;
  sev.sigev_signo = SIGVTALRM;
  
  if (timer_create(CLOCK_THREAD_CPUTIME_ID, &sev, &timerid) == -1)
    uerror("timer_create", Nothing);
  /* TODO: return the timer_t in an abstract wrapper */


  if (timer_settime(timerid, 0, &spec, NULL) == -1) {
    int saved_errno = errno;
    timer_delete(timerid);
    unix_error(saved_errno, "timer_settime", Nothing);
  }

  /* TODO: we leak the timer for now, this needs to be done properly */
  CAMLreturn (Val_unit);
}

