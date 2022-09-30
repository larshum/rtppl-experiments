#include <stdio.h>
#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "smemio.h" // shared memory definitions file

value lv_read_stub(value port) {
  CAMLparam1(port);
  CAMLlocal1(tsv);
  long long timestamp;
  double val;
  lv_read(port, &timestamp, &val);
  tsv = caml_alloc(2, 0);
  Store_field(tsv, 0, Val_int(timestamp));
  Store_field(tsv, 1, caml_copy_double(val));
  CAMLreturn(tsv);
}

void lv_write_stub(value port, value tsv) {
  CAMLparam2(port, tsv);
  long long timestamp = Int_val(Field(tsv, 0));
  double val = Double_val(Field(tsv, 1));
  lv_write(port, timestamp, val);
  CAMLreturn0;
}
