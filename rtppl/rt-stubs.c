#include <time.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value clock_get_time_stub() {
  CAMLparam0();
  CAMLlocal1(out);

  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);

  out = caml_alloc(2, 0);
  Store_field(out, 0, Val_int(ts.tv_sec));
  Store_field(out, 1, Val_int(ts.tv_nsec));
  CAMLreturn(out);
}

void clock_nanosleep_stub(value t) {
  CAMLparam1(t);

  struct timespec ts;
  ts.tv_sec = Int_val(Field(t, 0));
  ts.tv_nsec = Int_val(Field(t, 1));
  clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &ts, NULL);

  CAMLreturn0;
}
