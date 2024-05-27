#include <bits/time.h>
#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <signal.h>
#include <string.h>
#include <time.h>
#include <sys/fcntl.h>

#define timer_t_val(v) (*((timer_t **)Data_custom_val(v)))

static int ml_cpu_timer_free(value timer) {
  /* Called from finalizer, must not raise exceptions, and must not use
    the CAML* macros
   */
  timer_t *timerid_ptr = timer_t_val(timer);
  if (timerid_ptr) {
    timer_t timerid = *timerid_ptr;
    /* prevent double-free */
    timer_t_val(timer) = NULL;
    caml_stat_free(timerid_ptr);

    return timer_delete(timerid);
  }
  /* timer has already been freed, matches what timer_delete would return on bad
   * timer_t */
  errno = EINVAL;
  return -1;
}

void ml_cpu_timer_finalise(value timer) { ml_cpu_timer_free(timer); }

static struct custom_operations timer_ops = {"timer_t",
                                             ml_cpu_timer_finalise,
                                             custom_compare_default,
                                             custom_hash_default,
                                             custom_serialize_default,
                                             custom_deserialize_default,
                                             custom_compare_ext_default,
                                             custom_fixed_length_default};

CAMLprim value ml_cpu_timer_create(value is_thread) {
  CAMLparam1(is_thread);
  CAMLlocal1(timer);
  clockid_t clock =
      Bool_val(is_thread) ? CLOCK_THREAD_CPUTIME_ID : CLOCK_PROCESS_CPUTIME_ID;
  struct sigevent sev;
  timer_t *timerid;

  timer = caml_alloc_custom(&timer_ops, sizeof(timer_t *), 0, 1);
  /* initialize, in case the allocation below fails  */
  timer_t_val(timer) = NULL;

  timerid = caml_stat_alloc(sizeof(timer_t));

  memset(&sev, 0, sizeof(sev));
  sev.sigev_notify = SIGEV_SIGNAL;
  sev.sigev_signo = SIGVTALRM; /* same as SIGPREEMPTION in OCaml runtime */
  sev.sigev_value.sival_ptr = timerid;

  caml_enter_blocking_section();
  int rc = timer_create(clock, &sev, timerid);
  caml_leave_blocking_section();

  if (-1 == rc) {
    caml_stat_free(timerid);
    uerror("timer_create", Nothing);
  }
  /* only store value once we know it contains a valid timer */
  timer_t_val(timer) = timerid;

  CAMLreturn(timer);
}

CAMLprim value ml_cpu_timer_destroy(value timer) {
  CAMLparam1(timer);
  caml_enter_blocking_section();
  int rc = ml_cpu_timer_free(timer);
  caml_leave_blocking_section();
  if (-1 == rc)
    uerror("timer_delete", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value ml_cpu_timer_settime(value timer, value interval) {
  CAMLparam2(timer, interval);
  timer_t *timerid_ptr = timer_t_val(timer);

  if (!timerid_ptr)
    unix_error(EINVAL, "timer_settime", Nothing);

  struct itimerspec spec;

  double t = Double_val(interval);
  spec.it_interval.tv_sec = (time_t)t;
  spec.it_interval.tv_nsec = (t - spec.it_interval.tv_sec) * 1e9;
  spec.it_value = spec.it_interval;

  timer_t timerid = *timerid_ptr;

  caml_enter_blocking_section();
  int rc = timer_settime(timerid, 0, &spec, NULL);
  caml_leave_blocking_section();

  if (-1 == rc)
    uerror("timer_settime", Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value ml_cpu_timer_gettime(value timer) {
  CAMLparam1(timer);
  timer_t *timerid_ptr = timer_t_val(timer);

  if (!timerid_ptr)
    unix_error(EINVAL, "timer_gettime", Nothing);

  timer_t timerid = *timerid_ptr;
  struct itimerspec spec;

  caml_enter_blocking_section();
  int rc = timer_gettime(timerid, &spec);
  caml_leave_blocking_section();
  if (-1 == rc)
    uerror("timer_gettime", Nothing);

  double t = spec.it_value.tv_nsec * 1e-9 + spec.it_value.tv_sec;
  CAMLreturn(caml_copy_double(t));
}

CAMLprim value ml_set_sigio(value fdval) {
  CAMLparam1(fdval);
  int fd = Int_val(fdval);
  
  if (fcntl(fd, F_SETFL, O_ASYNC) == -1)
    uerror("fcntl", Nothing);

  if (fcntl(fd, F_SETOWN, getpid()) == -1)
    uerror("fcntl", Nothing);

  CAMLreturn(Val_unit);
}
