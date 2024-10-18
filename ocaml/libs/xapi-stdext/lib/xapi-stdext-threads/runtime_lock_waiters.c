#define CAML_INTERNALS
#include <caml/signals.h>
#include <caml/threads.h>
#include <stdatomic.h>

static void noop(void) {}

static void (*old_enter_blocking_section_hook)(void) = noop;
static void (*old_leave_blocking_section_hook)(void) = noop;

static atomic_int waiting;

static void my_enter_blocking_section(void)
{
  old_enter_blocking_section_hook();
}

static void my_leave_blocking_section(void)
{
  atomic_fetch_add(&waiting, 1);
  old_leave_blocking_section_hook();
  atomic_fetch_add(&waiting, -1);
}

CAMLprim value caml_runtime_lock_waiters_get(value)
{
  return Int_val(atomic_load(&waiting));
}

CAMLprim value caml_runtime_lock_waiters_init(value)
{
  void (*hook)(void);
  
  hook = caml_enter_blocking_section_hook;
  if (!hook)
    return Val_false;
  old_enter_blocking_section_hook = hook;
  
  hook = caml_leave_blocking_section_hook;
  if (!hook)
    return Val_false;
  old_leave_blocking_section_hook = hook;

  atomic_init(&waiting, 0);

  caml_enter_blocking_section_hook = my_enter_blocking_section;
  caml_leave_blocking_section_hook = my_leave_blocking_section;
  return Val_true;
}
