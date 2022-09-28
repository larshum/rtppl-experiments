#include <stdio.h>
#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "smemio.h" // shared memory definitions file

value open_sm_stub(value str) {
  CAMLparam1(str);
  const char *c = (const char*)String_val(str);
  int fd = open_sm(c);
  CAMLreturn(Val_int(fd));
}

value read_sm_stub(value ocaml_fd, value payload_length) {
  CAMLparam2(ocaml_fd, payload_length);
  CAMLlocal1(payload);

  int fd = Int_val(ocaml_fd);
  int n = Int_val(payload_length);
  int sz = n * sizeof(double);
  double *buffer = (double*)malloc(sz);

  int nread = read_sm(fd, buffer, n);
  int nelems = nread / sizeof(double);
  payload = caml_alloc(nelems, Double_array_tag);
  for (int i = 0; i < nelems; i++) {
    Store_double_field(payload, i, buffer[i]);
  }
  free(buffer);

  CAMLreturn(payload);
}

void write_sm_stub(value fd, value payload) {
  CAMLparam2(fd, payload);

  int n = Wosize_val(payload);
  int sz = n * sizeof(double);
  double *buffer = (double*)malloc(sz);
  for (int i = 0; i < n; i++) {
    buffer[i] = Double_field(payload, i);
  }

  write_sm(fd, buffer, sz);
  free(buffer);

  CAMLreturn0;
}
