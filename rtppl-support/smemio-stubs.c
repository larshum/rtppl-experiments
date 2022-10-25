#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "shared_mem.h"       // shared memory definitions file
#include "shared_mem_conf.h"

static int initialized = 0;

static sensor_val_t sensor_data[SM_OBJECT_COUNT];

void initIfNeeded() {
  if (initialized != 1) {
    sm_open_all();
    initialized = 1;
  }
}

value lv_read_helper(value ocamlPort, value tsv, bool isFloat) {
  int port = Int_val(ocamlPort);
  initIfNeeded();
  sm_read(&sm_regs[port], &sensor_data[port]);
  tsv = caml_alloc(2, 0);
  Store_field(tsv, 0, Val_int(sensor_data[port].ts));
  if (isFloat) {
    Store_field(tsv, 1, caml_copy_double(*((double*)sensor_data[port].val)));
  } else {
    Store_field(tsv, 1, caml_copy_string((char*)sensor_data[port].val));
  }
  return tsv;
}

void lv_write_helper(value ocamlPort, value tsv, bool isFloat) {
  int port = Int_val(ocamlPort);
  initIfNeeded();
  sensor_data[port].ts = Int_val(Field(tsv, 0));
  if (isFloat) {
    double d = Double_val(Field(tsv, 1));
    sensor_data[port].val = (void*)&d;
  } else {
    sensor_data[port].val = (void*)Bytes_val(Field(tsv, 1));
  }
  sm_write(&sm_regs[port], &sensor_data[port]);
}

value lv_read_stub(value ocamlPort) {
  CAMLparam1(ocamlPort);
  CAMLlocal1(tsv);
  lv_read_helper(ocamlPort, tsv, false);
  CAMLreturn(tsv);
}

value lv_read_float_stub(value ocamlPort) {
  CAMLparam1(ocamlPort);
  CAMLlocal1(tsv);
  lv_read_helper(ocamlPort, tsv, true);
  CAMLreturn(tsv);
}

void lv_write_stub(value ocamlPort, value tsv) {
  CAMLparam2(ocamlPort, tsv);
  lv_write_helper(ocamlPort, tsv, false);
  CAMLreturn0;
}

void lv_write_float_stub(value ocamlPort, value tsv) {
  CAMLparam2(ocamlPort, tsv);
  lv_write_helper(ocamlPort, tsv, true);
  CAMLreturn0;
}
