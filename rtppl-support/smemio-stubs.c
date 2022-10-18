#include <stdio.h>
#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "shared_mem.h"       // shared memory definitions file
#include "shared_mem_conf.h"

static int initialized = 0;

static sensor_val_t sensor_data[SM_OBJECT_COUNT];

value lv_read_stub(value ocamlPort) {
  CAMLparam1(ocamlPort);
  CAMLlocal1(tsv);

  int port = Int_val(ocamlPort);
  if (initialized != 1) {
    sm_open_all();
    initialized = 1;
  }
  sm_read(&sm_regs[port], &sensor_data[port]);

  tsv = caml_alloc(2, 0);
  Store_field(tsv, 0, Val_int(sensor_data[port].ts));
  Store_field(tsv, 1, caml_copy_string((char*)sensor_data[port].val));
  CAMLreturn(tsv);
}

void lv_write_stub(value ocamlPort, value tsv) {
  CAMLparam2(ocamlPort, tsv);

  int port = Int_val(ocamlPort);
  sensor_data[port].ts = Int_val(Field(tsv, 0));
  sensor_data[port].val = (void*)Bytes_val(Field(tsv, 1));

  if (initialized != 1) {
    sm_open_all();
    initialized = 1;
  }
  sm_write(&sm_regs[port], &sensor_data[port]);
  CAMLreturn0;
}
