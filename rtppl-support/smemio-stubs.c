#include <stdio.h>
#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "shared_mem.h"       // shared memory definitions file
#include "shared_mem_conf.h"

static int initialized = 0;

static sensor_val_t sensor_data[SM_OBJECT_COUNT];

value lv_read_stub(value port) {
  CAMLparam1(port);
  CAMLlocal1(tsv);

  if (initialized != 1) {
    sm_open_all();
    initialized = 1;
  }
  sm_read(&sm_regs[Int_val(port)], &sensor_data[Int_val(port)]);

  tsv = caml_alloc(2, 0);
  Store_field(tsv, 0, Val_int(sensor_data[Int_val(port)].ts));
  Store_field(tsv, 1, caml_copy_double(sensor_data[Int_val(port)].val));
  CAMLreturn(tsv);
}

void lv_write_stub(value port, value tsv) {
  CAMLparam2(port, tsv);

  sensor_data[Int_val(port)].ts = Int_val(Field(tsv, 0));
  sensor_data[Int_val(port)].val = Double_val(Field(tsv, 1));

  if (initialized != 1) {
    sm_open_all();
    initialized = 1;
  }
  sm_write(&sm_regs[Int_val(port)], &sensor_data[Int_val(port)]);
  CAMLreturn0;
}
